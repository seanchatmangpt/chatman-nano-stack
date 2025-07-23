#!/bin/bash
# CNS Local Deployment Script
# Runs GitHub Actions workflows locally using act
# Built for reliability. Designed to last.

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
DEPLOYMENT_DIR="${CNS_DEPLOYMENT_DIR:-$HOME/cns-deployment}"
LOG_DIR="${DEPLOYMENT_DIR}/logs"
BACKUP_DIR="${DEPLOYMENT_DIR}/backups"

# Functions
log() {
    echo -e "${BLUE}[$(date '+%Y-%m-%d %H:%M:%S')]${NC} $*"
}

error() {
    echo -e "${RED}[ERROR]${NC} $*" >&2
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $*"
}

warning() {
    echo -e "${YELLOW}[WARNING]${NC} $*"
}

check_dependencies() {
    log "Checking dependencies..."
    
    # Check for required tools
    local missing=()
    
    command -v uv >/dev/null 2>&1 || missing+=("uv")
    command -v gcc >/dev/null 2>&1 || missing+=("gcc")
    command -v make >/dev/null 2>&1 || missing+=("make")
    command -v git >/dev/null 2>&1 || missing+=("git")
    
    if [ ${#missing[@]} -ne 0 ]; then
        error "Missing required dependencies: ${missing[*]}"
        error "Please install missing tools and try again"
        exit 1
    fi
    
    # Check for act (GitHub Actions local runner)
    if ! command -v act >/dev/null 2>&1; then
        warning "act not found. Installing..."
        if [[ "$OSTYPE" == "darwin"* ]]; then
            brew install act
        else
            curl -s https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash
        fi
    fi
    
    success "All dependencies satisfied"
}

setup_deployment_env() {
    log "Setting up deployment environment..."
    
    # Create directory structure
    mkdir -p "$DEPLOYMENT_DIR"/{production,staging,backups,logs,metrics}
    mkdir -p "$LOG_DIR"/{deployment,monitoring,errors}
    
    # Initialize deployment tracking
    if [ ! -f "$DEPLOYMENT_DIR/deployments.json" ]; then
        echo "[]" > "$DEPLOYMENT_DIR/deployments.json"
    fi
    
    success "Deployment environment ready"
}

run_validation() {
    log "Running CNS validation suite..."
    
    # Run OTEL validation
    if ! uv run python validate_otel.py > "$LOG_DIR/validation-$(date +%Y%m%d-%H%M%S).log" 2>&1; then
        error "Validation failed. Check logs at $LOG_DIR"
        return 1
    fi
    
    # Run neural integration tests
    if ! uv run python neural_integration_test.py > "$LOG_DIR/neural-test-$(date +%Y%m%d-%H%M%S).log" 2>&1; then
        error "Neural integration tests failed"
        return 1
    fi
    
    success "All validations passed"
}

run_benchmarks() {
    log "Running performance benchmarks..."
    
    local benchmark_log="$LOG_DIR/benchmark-$(date +%Y%m%d-%H%M%S).log"
    
    if ! uv run python run_benchmark.py > "$benchmark_log" 2>&1; then
        error "Benchmarks failed"
        return 1
    fi
    
    # Extract performance score
    local score=$(grep "Performance Score:" "$benchmark_log" | grep -o '[0-9]*\.[0-9]*' | head -1)
    
    if [ -z "$score" ]; then
        error "Could not extract performance score"
        return 1
    fi
    
    log "Performance score: $score/100"
    
    # Check threshold
    if (( $(echo "$score < 80" | bc -l) )); then
        error "Performance score too low: $score/100 (minimum: 80)"
        return 1
    fi
    
    success "Performance benchmarks passed"
}

build_artifacts() {
    log "Building CNS artifacts..."
    
    local build_dir="$DEPLOYMENT_DIR/build-$(date +%Y%m%d-%H%M%S)"
    mkdir -p "$build_dir"
    
    # Compile all ontologies
    for ttl in $(find ontologies -name "*.ttl"); do
        log "Compiling $ttl..."
        local output_name=$(basename "$ttl" .ttl)
        if ! uv run python owl_compiler.py "$ttl" --output "$build_dir/$output_name" 2>&1; then
            error "Failed to compile $ttl"
            return 1
        fi
    done
    
    # Build C binaries with optimizations
    for dir in "$build_dir"/*; do
        if [ -f "$dir/Makefile" ]; then
            log "Building $(basename "$dir")..."
            if ! make -C "$dir" CFLAGS="-O3 -march=native -flto" > "$LOG_DIR/build-$(basename "$dir").log" 2>&1; then
                error "Build failed for $(basename "$dir")"
                return 1
            fi
        fi
    done
    
    # Create deployment package
    local package_name="cns-deployment-$(git rev-parse --short HEAD)-$(date +%Y%m%d-%H%M%S).tar.gz"
    
    tar -czf "$DEPLOYMENT_DIR/$package_name" \
        -C "$(dirname "$build_dir")" "$(basename "$build_dir")" \
        cns_status.py \
        cns_monitor.py \
        neural_integration_test.py \
        validate_otel.py \
        generated_signatures.py \
        ttl2dspy.py
    
    success "Build artifacts created: $package_name"
    echo "$build_dir"
}

deploy_to_environment() {
    local env=$1
    local build_dir=$2
    
    log "Deploying to $env environment..."
    
    local deploy_dir="$DEPLOYMENT_DIR/$env"
    local timestamp=$(date +%Y%m%d-%H%M%S)
    
    # Backup current deployment
    if [ -d "$deploy_dir/current" ]; then
        log "Backing up current deployment..."
        mv "$deploy_dir/current" "$BACKUP_DIR/$env-backup-$timestamp"
    fi
    
    # Deploy new version
    mkdir -p "$deploy_dir/current"
    cp -r "$build_dir"/* "$deploy_dir/current/"
    cp cns_status.py cns_monitor.py "$deploy_dir/current/"
    
    # Create deployment record
    local deployment_record=$(cat <<EOF
{
    "environment": "$env",
    "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
    "commit": "$(git rev-parse HEAD)",
    "branch": "$(git rev-parse --abbrev-ref HEAD)",
    "build_dir": "$build_dir",
    "status": "deployed"
}
EOF
)
    
    # Update deployments log
    jq ". += [$deployment_record]" "$DEPLOYMENT_DIR/deployments.json" > "$DEPLOYMENT_DIR/deployments.json.tmp"
    mv "$DEPLOYMENT_DIR/deployments.json.tmp" "$DEPLOYMENT_DIR/deployments.json"
    
    success "Deployed to $env environment"
}

verify_deployment() {
    local env=$1
    
    log "Verifying $env deployment..."
    
    local deploy_dir="$DEPLOYMENT_DIR/$env/current"
    
    # Test binaries
    for binary in "$deploy_dir"/*/; do
        if [ -x "$binary/$(basename "$binary")" ]; then
            log "Testing $(basename "$binary")..."
            if ! "$binary/$(basename "$binary")" --self-test > "$LOG_DIR/verify-$(basename "$binary").log" 2>&1; then
                error "Verification failed for $(basename "$binary")"
                return 1
            fi
        fi
    done
    
    # Test Python components
    cd "$deploy_dir"
    if ! python cns_status.py --format json > /dev/null 2>&1; then
        error "CNS status check failed"
        return 1
    fi
    
    success "Deployment verification passed"
}

monitor_deployment() {
    local env=$1
    
    log "Starting deployment monitoring..."
    
    # Start CNS monitor in background
    cd "$DEPLOYMENT_DIR/$env/current"
    nohup uv run python cns_monitor.py --duration 5 --interval 60 > "$LOG_DIR/monitoring/monitor-$(date +%Y%m%d-%H%M%S).log" 2>&1 &
    local monitor_pid=$!
    
    log "Monitor started with PID: $monitor_pid"
    
    # Create monitoring record
    cat > "$DEPLOYMENT_DIR/$env/monitor.pid" <<EOF
$monitor_pid
EOF
    
    success "Deployment monitoring active"
}

rollback_deployment() {
    local env=$1
    
    log "Rolling back $env deployment..."
    
    # Find latest backup
    local latest_backup=$(ls -t "$BACKUP_DIR/$env-backup-"* 2>/dev/null | head -1)
    
    if [ -z "$latest_backup" ]; then
        error "No backup found for rollback"
        return 1
    fi
    
    # Stop current monitoring
    if [ -f "$DEPLOYMENT_DIR/$env/monitor.pid" ]; then
        kill $(cat "$DEPLOYMENT_DIR/$env/monitor.pid") 2>/dev/null || true
    fi
    
    # Restore backup
    rm -rf "$DEPLOYMENT_DIR/$env/current"
    cp -r "$latest_backup" "$DEPLOYMENT_DIR/$env/current"
    
    success "Rolled back to $(basename "$latest_backup")"
}

# Main deployment flow
main() {
    local environment="${1:-local}"
    local action="${2:-deploy}"
    
    log "CNS Local Deployment System"
    log "Environment: $environment"
    log "Action: $action"
    
    case "$action" in
        deploy)
            check_dependencies
            setup_deployment_env
            
            # Run full deployment pipeline
            run_validation || exit 1
            run_benchmarks || exit 1
            
            local build_dir=$(build_artifacts)
            if [ -z "$build_dir" ]; then
                error "Build failed"
                exit 1
            fi
            
            deploy_to_environment "$environment" "$build_dir"
            verify_deployment "$environment"
            monitor_deployment "$environment"
            
            success "Deployment complete!"
            log "Access deployment at: $DEPLOYMENT_DIR/$environment/current"
            ;;
            
        validate)
            run_validation
            ;;
            
        benchmark)
            run_benchmarks
            ;;
            
        monitor)
            monitor_deployment "$environment"
            ;;
            
        rollback)
            rollback_deployment "$environment"
            ;;
            
        status)
            log "Deployment status for $environment:"
            if [ -f "$DEPLOYMENT_DIR/deployments.json" ]; then
                jq ".[] | select(.environment == \"$environment\") | ." "$DEPLOYMENT_DIR/deployments.json" | tail -1
            else
                warning "No deployments found"
            fi
            ;;
            
        ci)
            log "Running CI pipeline locally with act..."
            act -j test -P ubuntu-latest=ghcr.io/catthehacker/ubuntu:full-20.04
            ;;
            
        *)
            error "Unknown action: $action"
            echo "Usage: $0 [environment] [action]"
            echo "Environments: local, staging, production"
            echo "Actions: deploy, validate, benchmark, monitor, rollback, status, ci"
            exit 1
            ;;
    esac
}

# Run main function
main "$@"