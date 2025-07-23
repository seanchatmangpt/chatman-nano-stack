#!/usr/bin/env python3
"""
CNS Performance Monitor - Real-time OpenTelemetry Integration
Built for reliability. Designed to last.
"""

import asyncio
import json
import psutil
import subprocess
import time
from datetime import datetime, timedelta
from pathlib import Path
from typing import Dict, List, Any, Optional

# OpenTelemetry imports
from opentelemetry import metrics, trace
from opentelemetry.sdk.metrics import MeterProvider
from opentelemetry.sdk.trace import TracerProvider
from opentelemetry.sdk.metrics.export import ConsoleMetricExporter, PeriodicExportingMetricReader
from opentelemetry.sdk.trace.export import ConsoleSpanExporter, BatchSpanProcessor


class CNSPerformanceMonitor:
    """Real-time performance monitoring with OpenTelemetry integration"""
    
    def __init__(self):
        # Initialize OTEL 
        self._setup_telemetry()
        
        # Performance tracking
        self.start_time = time.time()
        self.metrics_history: List[Dict[str, Any]] = []
        self.alerts_triggered: List[Dict[str, Any]] = []
        
        # Thresholds for alerts
        self.thresholds = {
            "cpu_percent_critical": 90.0,
            "memory_percent_critical": 85.0,
            "disk_percent_critical": 95.0,
            "compilation_time_warning": 10.0,  # seconds
            "benchmark_score_warning": 80.0,   # percentage
            "uptime_hours_info": 24.0
        }
    
    def _setup_telemetry(self) -> None:
        """Setup OpenTelemetry providers and instruments"""
        # Metrics setup
        metric_reader = PeriodicExportingMetricReader(
            ConsoleMetricExporter(), export_interval_millis=5000
        )
        metrics.set_meter_provider(MeterProvider(metric_readers=[metric_reader]))
        
        # Tracing setup  
        trace.set_tracer_provider(TracerProvider())
        tracer_provider = trace.get_tracer_provider()
        span_processor = BatchSpanProcessor(ConsoleSpanExporter())
        tracer_provider.add_span_processor(span_processor)
        
        self.meter = metrics.get_meter("cns.monitor", version="1.0.0")
        self.tracer = trace.get_tracer("cns.monitor")
        
        # Create monitoring instruments
        self.system_cpu = self.meter.create_gauge(
            name="system_cpu_percent",
            description="System CPU usage percentage",
            unit="%"
        )
        
        self.system_memory = self.meter.create_gauge(
            name="system_memory_percent", 
            description="System memory usage percentage",
            unit="%"
        )
        
        self.system_disk = self.meter.create_gauge(
            name="system_disk_percent",
            description="System disk usage percentage", 
            unit="%"
        )
        
        self.cns_health_score = self.meter.create_gauge(
            name="cns_health_score",
            description="CNS system health score (0-100)",
            unit="score"
        )
        
        self.compilation_duration = self.meter.create_histogram(
            name="compilation_duration_seconds",
            description="Ontology compilation duration in seconds",
            unit="s"
        )
        
        self.benchmark_score = self.meter.create_histogram(
            name="benchmark_score_percent",
            description="Benchmark performance score",
            unit="%"
        )
        
        self.alert_counter = self.meter.create_counter(
            name="alerts_total",
            description="Total number of alerts triggered",
        )
        
        self.uptime_gauge = self.meter.create_gauge(
            name="cns_uptime_seconds",
            description="CNS system uptime in seconds",
            unit="s"
        )
    
    async def start_monitoring(self, duration_minutes: int = 60, interval_seconds: int = 10) -> None:
        """Start continuous monitoring for specified duration"""
        print(f"🔍 CNS Performance Monitor Starting")
        print(f"Duration: {duration_minutes} minutes")
        print(f"Interval: {interval_seconds} seconds")
        print(f"Started: {datetime.now().isoformat()}")
        print("="*50)
        
        end_time = time.time() + (duration_minutes * 60)
        
        try:
            while time.time() < end_time:
                with self.tracer.start_as_current_span("monitoring_cycle") as span:
                    # Collect metrics
                    metrics = await self._collect_metrics()
                    
                    # Update OTEL instruments
                    self._update_telemetry(metrics)
                    
                    # Check for alerts
                    alerts = self._check_alerts(metrics)
                    
                    # Store metrics history
                    self.metrics_history.append({
                        "timestamp": datetime.now().isoformat(),
                        "metrics": metrics,
                        "alerts": alerts
                    })
                    
                    # Generate periodic reports
                    if len(self.metrics_history) % 6 == 0:  # Every minute if 10s interval
                        self._generate_mermaid_report()
                    
                    span.set_attributes({
                        "cpu_percent": metrics["system"]["cpu_percent"],
                        "memory_percent": metrics["system"]["memory_percent"],
                        "health_score": metrics["cns"]["health_score"],
                        "alerts_count": len(alerts)
                    })
                
                await asyncio.sleep(interval_seconds)
                
        except KeyboardInterrupt:
            print("\n🛑 Monitoring stopped by user")
        
        # Generate final report
        self._generate_final_report()
    
    async def _collect_metrics(self) -> Dict[str, Any]:
        """Collect comprehensive system and CNS metrics"""
        # System metrics
        cpu_percent = psutil.cpu_percent(interval=0.1)
        memory = psutil.virtual_memory()
        disk = psutil.disk_usage('/')
        uptime = time.time() - self.start_time
        
        # CNS-specific metrics
        cns_metrics = await self._collect_cns_metrics()
        
        return {
            "system": {
                "cpu_percent": cpu_percent,
                "memory_percent": memory.percent,
                "memory_available_gb": memory.available / (1024**3),
                "disk_percent": disk.percent,
                "disk_free_gb": disk.free / (1024**3),
                "load_average": list(psutil.getloadavg()) if hasattr(psutil, 'getloadavg') else [0, 0, 0],
                "uptime_seconds": uptime
            },
            "cns": cns_metrics,
            "timestamp": time.time()
        }
    
    async def _collect_cns_metrics(self) -> Dict[str, Any]:
        """Collect CNS-specific performance metrics"""
        metrics = {
            "health_score": 100.0,
            "compilation_working": False,
            "benchmarks_working": False, 
            "generated_files": 0,
            "last_compilation": None,
            "benchmark_score": 0.0,
            "processes_running": 0
        }
        
        try:
            # Test compilation health
            compilation_start = time.time()
            result = await asyncio.create_subprocess_exec(
                'python', 'owl_compiler.py', '--help',
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE,
                cwd='/Users/sac/cns'
            )
            await result.wait()  
            compilation_duration = time.time() - compilation_start
            
            metrics["compilation_working"] = result.returncode == 0
            metrics["compilation_duration"] = compilation_duration
            
            # Test benchmark health
            if Path("/Users/sac/cns/live_system/owl_ontology").exists():
                benchmark_start = time.time()
                benchmark_result = await asyncio.create_subprocess_exec(
                    'uv', 'run', 'python', 'run_benchmark.py',
                    stdout=asyncio.subprocess.PIPE,
                    stderr=asyncio.subprocess.PIPE,
                    cwd='/Users/sac/cns'
                )
                await benchmark_result.wait()
                benchmark_duration = time.time() - benchmark_start
                
                metrics["benchmarks_working"] = benchmark_result.returncode == 0
                metrics["benchmark_duration"] = benchmark_duration
                
                # Parse benchmark score from output
                if benchmark_result.stdout:
                    stdout = (await benchmark_result.stdout.read()).decode()
                    if "Performance Score:" in stdout:
                        try:
                            score_line = [line for line in stdout.split('\n') if 'Performance Score:' in line][0]
                            score_str = score_line.split('Performance Score:')[1].strip().split('/')[0]
                            metrics["benchmark_score"] = float(score_str)
                        except:
                            pass
            
            # Count generated files
            live_system = Path("/Users/sac/cns/live_system")
            if live_system.exists():
                c_files = len(list(live_system.glob("*.c")))
                h_files = len(list(live_system.glob("*.h")))
                metrics["generated_files"] = c_files + h_files
            
            # Find CNS processes
            cns_processes = 0
            for proc in psutil.process_iter(['pid', 'name', 'cmdline']):
                try:
                    if any('cns' in str(item).lower() or 'owl_compiler' in str(item).lower() 
                          for item in proc.info['cmdline'] or []):
                        cns_processes += 1
                except (psutil.NoSuchProcess, psutil.AccessDenied):
                    continue
            metrics["processes_running"] = cns_processes
            
            # Calculate health score
            health_factors = []
            if metrics["compilation_working"]: health_factors.append(30)
            if metrics["benchmarks_working"]: health_factors.append(30)
            if metrics["generated_files"] > 0: health_factors.append(20)
            if metrics["benchmark_score"] > 80: health_factors.append(20)
            
            metrics["health_score"] = sum(health_factors)
            
        except Exception as e:
            print(f"⚠️  Error collecting CNS metrics: {e}")
            metrics["health_score"] = 0.0
        
        return metrics
    
    def _update_telemetry(self, metrics: Dict[str, Any]) -> None:
        """Update OpenTelemetry instruments with current metrics"""
        # System metrics
        self.system_cpu.set(metrics["system"]["cpu_percent"])
        self.system_memory.set(metrics["system"]["memory_percent"])
        self.system_disk.set(metrics["system"]["disk_percent"])
        self.uptime_gauge.set(metrics["system"]["uptime_seconds"])
        
        # CNS metrics
        self.cns_health_score.set(metrics["cns"]["health_score"])
        
        if "compilation_duration" in metrics["cns"]:
            self.compilation_duration.record(metrics["cns"]["compilation_duration"])
        
        if metrics["cns"]["benchmark_score"] > 0:
            self.benchmark_score.record(metrics["cns"]["benchmark_score"])
    
    def _check_alerts(self, metrics: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Check metrics against thresholds and generate alerts"""
        alerts = []
        
        # CPU alert
        if metrics["system"]["cpu_percent"] > self.thresholds["cpu_percent_critical"]:
            alert = {
                "level": "CRITICAL",
                "type": "system_cpu",
                "message": f"CPU usage critical: {metrics['system']['cpu_percent']:.1f}%",
                "threshold": self.thresholds["cpu_percent_critical"],
                "value": metrics["system"]["cpu_percent"],
                "timestamp": datetime.now().isoformat()
            }
            alerts.append(alert)
            self.alert_counter.add(1, {"level": "critical", "type": "cpu"})
        
        # Memory alert  
        if metrics["system"]["memory_percent"] > self.thresholds["memory_percent_critical"]:
            alert = {
                "level": "CRITICAL", 
                "type": "system_memory",
                "message": f"Memory usage critical: {metrics['system']['memory_percent']:.1f}%",
                "threshold": self.thresholds["memory_percent_critical"],
                "value": metrics["system"]["memory_percent"],
                "timestamp": datetime.now().isoformat()
            }
            alerts.append(alert)
            self.alert_counter.add(1, {"level": "critical", "type": "memory"})
        
        # CNS health alert
        if metrics["cns"]["health_score"] < self.thresholds["benchmark_score_warning"]:
            alert = {
                "level": "WARNING",
                "type": "cns_health", 
                "message": f"CNS health degraded: {metrics['cns']['health_score']:.1f}/100",
                "threshold": self.thresholds["benchmark_score_warning"],
                "value": metrics["cns"]["health_score"],
                "timestamp": datetime.now().isoformat()
            }
            alerts.append(alert)
            self.alert_counter.add(1, {"level": "warning", "type": "health"})
        
        # Compilation performance alert
        if "compilation_duration" in metrics["cns"] and metrics["cns"]["compilation_duration"] > self.thresholds["compilation_time_warning"]:
            alert = {
                "level": "WARNING",
                "type": "compilation_slow",
                "message": f"Compilation slow: {metrics['cns']['compilation_duration']:.1f}s",
                "threshold": self.thresholds["compilation_time_warning"],
                "value": metrics["cns"]["compilation_duration"],
                "timestamp": datetime.now().isoformat()
            }
            alerts.append(alert)
            self.alert_counter.add(1, {"level": "warning", "type": "compilation"})
        
        # Store new alerts
        self.alerts_triggered.extend(alerts)
        
        return alerts
    
    def _generate_mermaid_report(self) -> None:
        """Generate periodic Mermaid monitoring report"""
        if not self.metrics_history:
            return
            
        latest = self.metrics_history[-1]
        
        print(f"\n{'='*60}")
        print(f"📊 CNS PERFORMANCE MONITOR - {datetime.now().strftime('%H:%M:%S')}")
        print(f"{'='*60}")
        
        # Current status
        health_status = "🟢 OPTIMAL" if latest["metrics"]["cns"]["health_score"] > 90 else \
                       "🟡 DEGRADED" if latest["metrics"]["cns"]["health_score"] > 70 else "🔴 CRITICAL"
        
        print(f"Health: {health_status} ({latest['metrics']['cns']['health_score']:.1f}/100)")
        print(f"CPU: {latest['metrics']['system']['cpu_percent']:.1f}%")
        print(f"Memory: {latest['metrics']['system']['memory_percent']:.1f}%")
        print(f"Uptime: {latest['metrics']['system']['uptime_seconds']/3600:.1f}h")
        
        if latest["alerts"]:
            print(f"🚨 Active Alerts: {len(latest['alerts'])}")
            for alert in latest["alerts"]:
                print(f"   {alert['level']}: {alert['message']}")
        
        # Generate real-time Mermaid
        print("\n```mermaid")
        print("graph TD")
        print("    A[CNS Performance Monitor] --> B[System Health]")
        print("    A --> C[CNS Components]")
        print("    A --> D[Alert Status]")
        
        # System health
        cpu_status = "NORMAL" if latest["metrics"]["system"]["cpu_percent"] < 70 else "HIGH"
        mem_status = "NORMAL" if latest["metrics"]["system"]["memory_percent"] < 70 else "HIGH"
        print(f"    B --> B1[CPU: {latest['metrics']['system']['cpu_percent']:.1f}% - {cpu_status}]")
        print(f"    B --> B2[Memory: {latest['metrics']['system']['memory_percent']:.1f}% - {mem_status}]")
        
        # CNS components
        comp_status = "WORKING" if latest["metrics"]["cns"]["compilation_working"] else "FAILED"
        bench_status = "WORKING" if latest["metrics"]["cns"]["benchmarks_working"] else "FAILED"
        print(f"    C --> C1[Compilation: {comp_status}]")
        print(f"    C --> C2[Benchmarks: {bench_status}]")
        print(f"    C --> C3[Generated Files: {latest['metrics']['cns']['generated_files']}]")
        
        # Alert status
        alert_count = len(latest["alerts"])
        if alert_count == 0:
            print("    D --> D1[No Active Alerts]")
            print("    class D1 normal")
        else:
            print(f"    D --> D1[{alert_count} Active Alerts]")
            print("    class D1 alert")
        
        # Styling
        print("    classDef normal fill:#90EE90")
        print("    classDef alert fill:#FFB6C1")
        print("    classDef warning fill:#FFFFE0")
        print("```")
        
        # Performance timeline
        if len(self.metrics_history) >= 6:
            print("\n```mermaid")
            print("timeline")
            print("    title CNS Performance Timeline (Last 6 samples)")
            
            recent_samples = self.metrics_history[-6:]
            for i, sample in enumerate(recent_samples):
                timestamp = datetime.fromisoformat(sample["timestamp"]).strftime("%H:%M")
                health = sample["metrics"]["cns"]["health_score"]
                cpu = sample["metrics"]["system"]["cpu_percent"]
                print(f"    {timestamp} : Health {health:.0f}% CPU {cpu:.0f}%")
            print("```")
    
    def _generate_final_report(self) -> None:
        """Generate comprehensive final monitoring report"""
        print(f"\n{'='*70}")
        print("🏁 CNS PERFORMANCE MONITOR - FINAL REPORT")
        print(f"{'='*70}")
        
        if not self.metrics_history:
            print("No metrics collected during monitoring period.")
            return
        
        # Summary statistics
        total_samples = len(self.metrics_history)
        monitoring_duration = time.time() - self.start_time
        
        # Calculate averages
        avg_cpu = sum(m["metrics"]["system"]["cpu_percent"] for m in self.metrics_history) / total_samples
        avg_memory = sum(m["metrics"]["system"]["memory_percent"] for m in self.metrics_history) / total_samples  
        avg_health = sum(m["metrics"]["cns"]["health_score"] for m in self.metrics_history) / total_samples
        
        print(f"Monitoring Duration: {monitoring_duration/60:.1f} minutes")
        print(f"Total Samples: {total_samples}")
        print(f"Sample Interval: {monitoring_duration/total_samples:.1f}s")
        print()
        print(f"Average CPU Usage: {avg_cpu:.1f}%")
        print(f"Average Memory Usage: {avg_memory:.1f}%")
        print(f"Average CNS Health: {avg_health:.1f}/100")
        print(f"Total Alerts: {len(self.alerts_triggered)}")
        
        # Alert summary
        if self.alerts_triggered:
            alert_types = {}
            for alert in self.alerts_triggered:
                alert_types[alert["type"]] = alert_types.get(alert["type"], 0) + 1
            
            print(f"\nAlert Breakdown:")
            for alert_type, count in alert_types.items():
                print(f"  {alert_type}: {count}")
        
        # Performance trend
        print("\n```mermaid")
        print("xychart-beta")
        print('    title "CNS Health Score Over Time"')
        print('    x-axis "Time"')
        print('    y-axis "Health Score" 0 --> 100')
        
        # Sample up to 10 data points for readability
        step = max(1, len(self.metrics_history) // 10)
        sampled = self.metrics_history[::step][:10]
        
        x_labels = [datetime.fromisoformat(s["timestamp"]).strftime("%H:%M") for s in sampled]
        y_values = [s["metrics"]["cns"]["health_score"] for s in sampled]
        
        print(f'    line [{", ".join(map(str, y_values))}]')
        print("```")
        
        # Final status
        final_health = self.metrics_history[-1]["metrics"]["cns"]["health_score"]
        if final_health > 90:
            print("\n🎉 MONITORING COMPLETE - System is OPTIMAL")
        elif final_health > 70:
            print("\n⚠️  MONITORING COMPLETE - System shows DEGRADED performance")
        else:
            print("\n🚨 MONITORING COMPLETE - System requires IMMEDIATE attention")


async def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description="CNS Performance Monitor")
    parser.add_argument('--duration', type=int, default=60, help='Monitoring duration in minutes')
    parser.add_argument('--interval', type=int, default=10, help='Sample interval in seconds')
    parser.add_argument('--continuous', action='store_true', help='Run continuously until stopped')
    
    args = parser.parse_args()
    
    monitor = CNSPerformanceMonitor()
    
    try:
        if args.continuous:
            print("🔄 Starting continuous monitoring (Ctrl+C to stop)")
            await monitor.start_monitoring(duration_minutes=999999, interval_seconds=args.interval)
        else:
            await monitor.start_monitoring(duration_minutes=args.duration, interval_seconds=args.interval)
    except KeyboardInterrupt:
        print("\n👋 Monitoring stopped by user")


if __name__ == "__main__":
    asyncio.run(main())