#!/bin/bash
# BitActor UHFT Node Bootstrap Script
# Optimized for sub-microsecond latency

set -euxo pipefail

# Performance tuning variables
WORKLOAD="${workload}"
PROJECT_NAME="${project_name}"
REGION="${region}"

# Log everything
exec > >(tee /var/log/user-data.log)
exec 2>&1

echo "=== BitActor UHFT Node Bootstrap ==="
echo "Workload: $WORKLOAD"
echo "Project: $PROJECT_NAME"
echo "Region: $REGION"

# Update system
apt-get update
apt-get upgrade -y

# Install performance tools
apt-get install -y \
    linux-tools-aws \
    linux-tools-$(uname -r) \
    numactl \
    hwloc \
    irqbalance \
    tuned \
    sysstat \
    nicstat \
    ethtool \
    net-tools \
    dpdk \
    dpdk-dev \
    libdpdk-dev \
    hugepages \
    stress-ng

# Kernel tuning for UHFT
cat >> /etc/sysctl.conf <<EOF
# Network optimizations
net.core.rmem_max = 134217728
net.core.wmem_max = 134217728
net.ipv4.tcp_rmem = 4096 87380 134217728
net.ipv4.tcp_wmem = 4096 65536 134217728
net.core.netdev_max_backlog = 30000
net.core.netdev_budget = 600
net.ipv4.tcp_congestion_control = bbr
net.ipv4.tcp_notsent_lowat = 16384
net.ipv4.tcp_low_latency = 1
net.ipv4.tcp_timestamps = 0
net.ipv4.tcp_sack = 0
net.ipv4.tcp_dsack = 0

# Disable unnecessary features
net.ipv4.tcp_slow_start_after_idle = 0
net.ipv4.tcp_no_metrics_save = 1
net.ipv4.route.flush = 1

# Memory optimizations
vm.swappiness = 0
vm.dirty_ratio = 5
vm.dirty_background_ratio = 2
vm.overcommit_memory = 1

# Scheduler optimizations
kernel.sched_min_granularity_ns = 10000000
kernel.sched_wakeup_granularity_ns = 15000000
kernel.sched_migration_cost_ns = 5000000
kernel.sched_autogroup_enabled = 0
EOF

sysctl -p

# Disable CPU frequency scaling
for cpu in /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor; do
    echo performance > $cpu
done

# Disable CPU C-states for lowest latency
for cpu in /sys/devices/system/cpu/cpu*/cpuidle/state*/disable; do
    echo 1 > $cpu
done

# Setup hugepages
echo 2048 > /sys/kernel/mm/hugepages/hugepages-2048kB/nr_hugepages
mkdir -p /mnt/huge
mount -t hugetlbfs nodev /mnt/huge
echo "nodev /mnt/huge hugetlbfs defaults 0 0" >> /etc/fstab

# CPU isolation based on workload
case "$WORKLOAD" in
    "market-data")
        # Isolate CPUs 8-15 for market data processing
        sed -i 's/GRUB_CMDLINE_LINUX=""/GRUB_CMDLINE_LINUX="isolcpus=8-15 nohz_full=8-15 rcu_nocbs=8-15"/' /etc/default/grub
        
        # IRQ affinity - move all IRQs to CPU 0-7
        systemctl stop irqbalance
        for irq in $(cat /proc/interrupts | grep -E "eth|ena" | awk '{print $1}' | sed 's/:$//'); do
            echo 0-7 > /proc/irq/$irq/smp_affinity_list
        done
        ;;
        
    "order-engine")
        # Isolate CPUs 12-23 for order processing
        sed -i 's/GRUB_CMDLINE_LINUX=""/GRUB_CMDLINE_LINUX="isolcpus=12-23 nohz_full=12-23 rcu_nocbs=12-23"/' /etc/default/grub
        ;;
        
    "risk-engine")
        # Isolate CPUs 16-31 for risk calculations
        sed -i 's/GRUB_CMDLINE_LINUX=""/GRUB_CMDLINE_LINUX="isolcpus=16-31 nohz_full=16-31 rcu_nocbs=16-31"/' /etc/default/grub
        ;;
esac

update-grub

# Network optimization
if [[ -f /sys/class/net/eth0/device/sriov_numvfs ]]; then
    # Enable SR-IOV if available
    echo 8 > /sys/class/net/eth0/device/sriov_numvfs
fi

# Enable jumbo frames
for interface in $(ls /sys/class/net | grep -E "eth|ena"); do
    ip link set dev $interface mtu 9000
    ethtool -G $interface rx 4096 tx 4096
    ethtool -C $interface adaptive-rx off adaptive-tx off rx-usecs 0 tx-usecs 0
    ethtool -K $interface tso off gso off gro off lro off
done

# Setup DPDK if enabled
if [[ "$WORKLOAD" == "market-data" ]] || [[ "$WORKLOAD" == "execution-gateway" ]]; then
    # Bind network interface to DPDK
    modprobe uio_pci_generic
    
    # Reserve 1GB hugepages for DPDK
    echo 512 > /sys/kernel/mm/hugepages/hugepages-2048kB/nr_hugepages
    
    # Get PCI address of secondary network interface
    DPDK_IF=$(lspci | grep Ethernet | tail -1 | awk '{print $1}')
    if [[ ! -z "$DPDK_IF" ]]; then
        dpdk-devbind.py --bind=uio_pci_generic $DPDK_IF
    fi
fi

# Install Docker and container runtime
curl -fsSL https://get.docker.com | sh
systemctl enable docker
systemctl start docker

# Configure Docker for performance
cat > /etc/docker/daemon.json <<EOF
{
    "storage-driver": "overlay2",
    "log-driver": "json-file",
    "log-opts": {
        "max-size": "10m",
        "max-file": "3"
    },
    "default-ulimits": {
        "memlock": {
            "Name": "memlock",
            "Hard": -1,
            "Soft": -1
        },
        "nofile": {
            "Name": "nofile",
            "Hard": 1048576,
            "Soft": 1048576
        }
    }
}
EOF

systemctl restart docker

# Install monitoring agents
# Prometheus node exporter
wget https://github.com/prometheus/node_exporter/releases/download/v1.6.1/node_exporter-1.6.1.linux-amd64.tar.gz
tar xvf node_exporter-1.6.1.linux-amd64.tar.gz
cp node_exporter-1.6.1.linux-amd64/node_exporter /usr/local/bin/
useradd -rs /bin/false node_exporter

cat > /etc/systemd/system/node_exporter.service <<EOF
[Unit]
Description=Node Exporter
After=network.target

[Service]
User=node_exporter
Group=node_exporter
Type=simple
ExecStart=/usr/local/bin/node_exporter \
    --collector.cpu.info \
    --collector.diskstats.ignored-devices="^(ram|loop|fd)\\\\d+$" \
    --collector.filesystem.ignored-mount-points="^/(sys|proc|dev|run)($|/)" \
    --collector.netclass.ignored-devices="^(veth.*|docker.*|br-.*)$" \
    --collector.netdev.device-exclude="^(veth.*|docker.*|br-.*)$" \
    --collector.processes \
    --collector.interrupts

[Install]
WantedBy=multi-user.target
EOF

systemctl daemon-reload
systemctl enable node_exporter
systemctl start node_exporter

# Install and configure CloudWatch agent
wget https://s3.amazonaws.com/amazoncloudwatch-agent/amazon_linux/amd64/latest/amazon-cloudwatch-agent.rpm
rpm -U ./amazon-cloudwatch-agent.rpm

cat > /opt/aws/amazon-cloudwatch-agent/etc/amazon-cloudwatch-agent.json <<EOF
{
    "metrics": {
        "namespace": "BitActor/UHFT",
        "metrics_collected": {
            "cpu": {
                "measurement": [
                    {
                        "name": "cpu_usage_active",
                        "rename": "CPU_Usage",
                        "unit": "Percent"
                    }
                ],
                "totalcpu": false,
                "metrics_collection_interval": 1,
                "resources": ["*"]
            },
            "disk": {
                "measurement": [
                    {
                        "name": "used_percent",
                        "rename": "Disk_Used",
                        "unit": "Percent"
                    }
                ],
                "metrics_collection_interval": 60,
                "resources": ["*"]
            },
            "mem": {
                "measurement": [
                    {
                        "name": "mem_used_percent",
                        "rename": "Memory_Used",
                        "unit": "Percent"
                    }
                ],
                "metrics_collection_interval": 1
            },
            "net": {
                "measurement": [
                    {
                        "name": "net_packets_recv",
                        "rename": "Network_Packets_In",
                        "unit": "Count"
                    },
                    {
                        "name": "net_packets_sent",
                        "rename": "Network_Packets_Out",
                        "unit": "Count"
                    }
                ],
                "metrics_collection_interval": 1
            }
        },
        "append_dimensions": {
            "InstanceId": "\${aws:InstanceId}",
            "InstanceType": "\${aws:InstanceType}",
            "Workload": "$WORKLOAD",
            "Region": "$REGION"
        }
    }
}
EOF

/opt/aws/amazon-cloudwatch-agent/bin/amazon-cloudwatch-agent-ctl \
    -a fetch-config \
    -m ec2 \
    -s \
    -c file:/opt/aws/amazon-cloudwatch-agent/etc/amazon-cloudwatch-agent.json

# Security hardening
# Disable unnecessary services
systemctl disable cups
systemctl disable avahi-daemon
systemctl disable bluetooth

# Setup firewall rules
apt-get install -y ufw
ufw default deny incoming
ufw default allow outgoing
ufw allow 22/tcp  # SSH
ufw allow 4369/tcp  # EPMD
ufw allow 9090/tcp  # Metrics
ufw allow 9100/tcp  # BitActor
ufw allow 8080/tcp  # Health checks
ufw --force enable

# Create BitActor user
useradd -m -s /bin/bash bitactor
usermod -aG docker bitactor

# Set resource limits
cat >> /etc/security/limits.conf <<EOF
bitactor soft nofile 1048576
bitactor hard nofile 1048576
bitactor soft memlock unlimited
bitactor hard memlock unlimited
bitactor soft stack unlimited
bitactor hard stack unlimited
EOF

# Final optimizations
echo never > /sys/kernel/mm/transparent_hugepage/enabled
echo never > /sys/kernel/mm/transparent_hugepage/defrag

# Signal completion
touch /var/lib/cloud/instance/bitactor-bootstrap-complete

echo "=== BitActor UHFT Node Bootstrap Complete ==="
echo "Workload: $WORKLOAD optimizations applied"
echo "Reboot required for some settings to take effect"