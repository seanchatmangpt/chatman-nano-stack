#!/bin/bash
set -e

# CNS Primary Node Setup Script
echo "=== Setting up CNS UHFT Primary Node ==="

# Update system and install dependencies
yum update -y
yum install -y gcc gcc-c++ make cmake git wget epel-release
yum install -y python3 python3-pip nodejs npm

# Install performance tools
yum install -y perf sysstat htop iotop

# Kernel tuning for UHFT
echo "=== Applying UHFT kernel optimizations ==="
cat >> /etc/sysctl.conf << EOF
# UHFT Network Optimizations
net.core.rmem_max = 134217728
net.core.wmem_max = 134217728
net.ipv4.tcp_rmem = 4096 87380 134217728
net.ipv4.tcp_wmem = 4096 65536 134217728
net.core.netdev_max_backlog = 5000
net.ipv4.tcp_congestion_control = bbr
net.ipv4.tcp_mtu_probing = 1

# CPU and scheduler optimizations
kernel.sched_min_granularity_ns = 10000000
kernel.sched_wakeup_granularity_ns = 15000000
kernel.sched_migration_cost_ns = 5000000

# Disable CPU frequency scaling
cpupower frequency-set -g performance
EOF

sysctl -p

# Disable CPU C-states for consistent latency
for i in /sys/devices/system/cpu/cpu*/cpuidle/state*/disable; do
    echo 1 > $i 2>/dev/null || true
done

# Set CPU affinity for network interrupts
echo "=== Setting CPU affinity ==="
systemctl stop irqbalance
for irq in $(grep eth0 /proc/interrupts | awk '{print $1}' | sed 's/://'); do
    echo 2 > /proc/irq/$irq/smp_affinity
done

# Clone and build CNS
echo "=== Building CNS from source ==="
cd /opt
git clone https://github.com/cns/bitactor.git
cd bitactor

# Build with optimizations
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release \
         -DENABLE_SIMD=ON \
         -DENABLE_UHFT=ON \
         -DUSE_TCMALLOC=ON

make -j$(nproc)
make install

# Install Bloomberg B-PIPE connector
echo "=== Installing Bloomberg B-PIPE connector ==="
cd /opt
wget https://cdn.bloomberg.com/blpapi/cpp/blpapi-cpp-3.23.0.1-linux.tar.gz
tar -xzf blpapi-cpp-3.23.0.1-linux.tar.gz
cd blpapi-cpp-3.23.0.1/Linux
cp -r include/* /usr/local/include/
cp -r lib/* /usr/local/lib/
ldconfig

# Install Reuters connector
echo "=== Installing Reuters Elektron connector ==="
cd /opt
git clone https://github.com/Refinitiv/Real-Time-SDK.git
cd Real-Time-SDK/Cpp-C/Ema
mkdir build && cd build
cmake .. && make -j$(nproc)
make install

# Setup CNS configuration
cat > /etc/cns/config.json << EOF
{
  "mode": "production",
  "news_sources": {
    "bloomberg": {
      "enabled": true,
      "api_key": "${news_api_key}",
      "endpoint": "tcp://btpipe.bloomberg.com:8194",
      "subscriptions": ["NEWS", "FLASH", "EARNINGS"]
    },
    "reuters": {
      "enabled": true,
      "endpoint": "${reuters_endpoint}",
      "service": "ELEKTRON_DD",
      "subscriptions": ["NEWS", "ECONOMIC", "CORPORATE"]
    }
  },
  "validation": {
    "perfect_hash_size": 1048576,
    "cache_size_mb": 4096,
    "batch_size": 1000,
    "tick_budget_ns": 8000
  },
  "telemetry": {
    "cloudwatch_enabled": true,
    "namespace": "CNS/UHFT",
    "metrics_interval_ms": 100
  }
}
EOF

# Create systemd service
cat > /etc/systemd/system/cns-validator.service << EOF
[Unit]
Description=CNS UHFT News Validator
After=network.target

[Service]
Type=simple
User=cns
ExecStart=/usr/local/bin/cns-validator --config /etc/cns/config.json
Restart=always
RestartSec=1
LimitNOFILE=1000000
CPUAffinity=0-23

[Install]
WantedBy=multi-user.target
EOF

# Create monitoring script
cat > /usr/local/bin/cns-monitor.sh << 'EOF'
#!/bin/bash
while true; do
    # Get CNS metrics
    LATENCY=$(curl -s localhost:9090/metrics | grep cns_validation_latency_ns | tail -1 | awk '{print $2}')
    THROUGHPUT=$(curl -s localhost:9090/metrics | grep cns_throughput_per_sec | tail -1 | awk '{print $2}')
    
    # Send to CloudWatch
    aws cloudwatch put-metric-data \
        --namespace "CNS/UHFT" \
        --metric-name "ValidationLatencyNS" \
        --value "$LATENCY" \
        --dimensions Instance=$(ec2-metadata --instance-id | cut -d' ' -f2)
    
    aws cloudwatch put-metric-data \
        --namespace "CNS/UHFT" \
        --metric-name "NewsThroughput" \
        --value "$THROUGHPUT" \
        --dimensions Instance=$(ec2-metadata --instance-id | cut -d' ' -f2)
    
    sleep 1
done
EOF
chmod +x /usr/local/bin/cns-monitor.sh

# Start services
useradd -r -s /bin/false cns
systemctl daemon-reload
systemctl enable cns-validator
systemctl start cns-validator

# Start monitoring
nohup /usr/local/bin/cns-monitor.sh > /var/log/cns-monitor.log 2>&1 &

echo "=== CNS Primary setup complete ==="