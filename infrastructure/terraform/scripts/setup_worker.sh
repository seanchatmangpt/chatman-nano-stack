#!/bin/bash
set -e

# CNS Worker Node Setup Script
echo "=== Setting up CNS UHFT Worker Node ==="

# Same base setup as primary
yum update -y
yum install -y gcc gcc-c++ make cmake git wget epel-release

# Apply same kernel optimizations
cat >> /etc/sysctl.conf << EOF
net.core.rmem_max = 134217728
net.core.wmem_max = 134217728
net.ipv4.tcp_rmem = 4096 87380 134217728
net.ipv4.tcp_wmem = 4096 65536 134217728
net.core.netdev_max_backlog = 5000
net.ipv4.tcp_congestion_control = bbr
EOF
sysctl -p

# Install CNS worker
cd /opt
git clone https://github.com/cns/bitactor.git
cd bitactor
mkdir build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release -DENABLE_SIMD=ON
make -j$(nproc) && make install

# Configure as worker
cat > /etc/cns/worker-config.json << EOF
{
  "mode": "worker",
  "primary_node": "${primary_ip}",
  "worker_id": "$(hostname)",
  "validation": {
    "batch_size": 1000,
    "tick_budget_ns": 8000
  }
}
EOF

# Worker service
cat > /etc/systemd/system/cns-worker.service << EOF
[Unit]
Description=CNS UHFT Worker
After=network.target

[Service]
Type=simple
User=cns
ExecStart=/usr/local/bin/cns-worker --config /etc/cns/worker-config.json
Restart=always
RestartSec=1
LimitNOFILE=1000000

[Install]
WantedBy=multi-user.target
EOF

useradd -r -s /bin/false cns
systemctl daemon-reload
systemctl enable cns-worker
systemctl start cns-worker

echo "=== CNS Worker setup complete ==="