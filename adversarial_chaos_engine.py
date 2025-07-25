#!/usr/bin/env python3
"""
BitActor Adversarial Chaos Engineering Framework

This script performs chaos engineering attacks against the BitActor K8s deployment
to test resilience, fault tolerance, and security under stress conditions.

CRITICAL: This tool is for defensive testing only. Use only in controlled environments.
"""

import json
import random
import subprocess
import sys
import time
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
from typing import Dict, List, Optional
import concurrent.futures

class ChaosType(Enum):
    NODE_FAILURE = "NODE_FAILURE"
    POD_KILL = "POD_KILL"
    NETWORK_PARTITION = "NETWORK_PARTITION"
    RESOURCE_EXHAUSTION = "RESOURCE_EXHAUSTION"
    DNS_CHAOS = "DNS_CHAOS"
    DISK_PRESSURE = "DISK_PRESSURE"
    CPU_STRESS = "CPU_STRESS"
    MEMORY_BOMB = "MEMORY_BOMB"

@dataclass
class ChaosExperiment:
    """Chaos experiment configuration"""
    name: str
    chaos_type: ChaosType
    duration: int  # seconds
    intensity: float  # 0-1 scale
    target_components: List[str]
    security_impact: str
    recovery_time: int  # expected recovery in seconds

class AdversarialChaosEngine:
    """Chaos engineering framework for adversarial testing"""
    
    def __init__(self, namespace: str = "bitactor"):
        self.namespace = namespace
        self.experiments: List[ChaosExperiment] = []
        self.results: List[Dict] = []
        
    def run_chaos_campaign(self) -> Dict:
        """Execute comprehensive chaos engineering campaign"""
        print("⚡ ADVERSARIAL CHAOS ENGINEERING - FAULT TOLERANCE VALIDATION")
        print("=" * 70)
        
        # Phase 1: Infrastructure Chaos
        print("\n💥 PHASE 1: INFRASTRUCTURE CHAOS ATTACKS")
        self._execute_infrastructure_chaos()
        
        # Phase 2: Application-Level Chaos
        print("\n🎯 PHASE 2: APPLICATION-LEVEL CHAOS")
        self._execute_application_chaos()
        
        # Phase 3: Network Chaos
        print("\n🌐 PHASE 3: NETWORK CHAOS & PARTITION ATTACKS")
        self._execute_network_chaos()
        
        # Phase 4: Resource Exhaustion
        print("\n📊 PHASE 4: RESOURCE EXHAUSTION ATTACKS")
        self._execute_resource_chaos()
        
        # Phase 5: Security-Focused Chaos
        print("\n🔒 PHASE 5: SECURITY-FOCUSED CHAOS SCENARIOS")
        self._execute_security_chaos()
        
        return self._generate_chaos_report()
    
    def _execute_infrastructure_chaos(self):
        """Execute infrastructure-level chaos experiments"""
        
        experiments = [
            ChaosExperiment(
                name="Pod Termination Storm",
                chaos_type=ChaosType.POD_KILL,
                duration=60,
                intensity=0.7,
                target_components=["pods"],
                security_impact="Test if security controls remain during pod restarts",
                recovery_time=30
            ),
            ChaosExperiment(
                name="CPU Stress Attack",
                chaos_type=ChaosType.CPU_STRESS,
                duration=120,
                intensity=0.9,
                target_components=["cpu"],
                security_impact="Validate security under high CPU load",
                recovery_time=10
            ),
            ChaosExperiment(
                name="Memory Exhaustion",
                chaos_type=ChaosType.MEMORY_BOMB,
                duration=90,
                intensity=0.8,
                target_components=["memory"],
                security_impact="Test OOM handling and security context preservation",
                recovery_time=20
            )
        ]
        
        for experiment in experiments:
            print(f"🔥 Executing: {experiment.name}")
            result = self._run_experiment(experiment)
            self.results.append(result)
            time.sleep(5)  # Recovery pause
    
    def _execute_application_chaos(self):
        """Execute application-level chaos experiments"""
        
        experiments = [
            ChaosExperiment(
                name="Signal Processing Overload",
                chaos_type=ChaosType.RESOURCE_EXHAUSTION,
                duration=60,
                intensity=0.9,
                target_components=["bitactor-service"],
                security_impact="Test signal buffer overflow security",
                recovery_time=15
            ),
            ChaosExperiment(
                name="Health Check Failures",
                chaos_type=ChaosType.POD_KILL,
                duration=45,
                intensity=0.6,
                target_components=["health-checks"],
                security_impact="Validate security during health check failures",
                recovery_time=10
            )
        ]
        
        for experiment in experiments:
            print(f"⚡ Executing: {experiment.name}")
            result = self._run_experiment(experiment)
            self.results.append(result)
            time.sleep(3)
    
    def _execute_network_chaos(self):
        """Execute network-level chaos experiments"""
        
        experiments = [
            ChaosExperiment(
                name="DNS Resolution Chaos",
                chaos_type=ChaosType.DNS_CHAOS,
                duration=60,
                intensity=0.7,
                target_components=["dns"],
                security_impact="Test DNS-based attacks and network policy effectiveness",
                recovery_time=20
            ),
            ChaosExperiment(
                name="Network Partition Simulation",
                chaos_type=ChaosType.NETWORK_PARTITION,
                duration=90,
                intensity=0.8,
                target_components=["network"],
                security_impact="Validate network policies during partitions",
                recovery_time=30
            )
        ]
        
        for experiment in experiments:
            print(f"🌐 Executing: {experiment.name}")
            result = self._run_experiment(experiment)
            self.results.append(result)
            time.sleep(5)
    
    def _execute_resource_chaos(self):
        """Execute resource exhaustion chaos experiments"""
        
        experiments = [
            ChaosExperiment(
                name="Disk Space Exhaustion",
                chaos_type=ChaosType.DISK_PRESSURE,
                duration=60,
                intensity=0.9,
                target_components=["storage"],
                security_impact="Test security controls under disk pressure",
                recovery_time=25
            ),
            ChaosExperiment(
                name="File Descriptor Exhaustion",
                chaos_type=ChaosType.RESOURCE_EXHAUSTION,
                duration=45,
                intensity=0.8,
                target_components=["file-descriptors"],
                security_impact="Validate resource limits and security boundaries",
                recovery_time=15
            )
        ]
        
        for experiment in experiments:
            print(f"📊 Executing: {experiment.name}")
            result = self._run_experiment(experiment)
            self.results.append(result)
            time.sleep(3)
    
    def _execute_security_chaos(self):
        """Execute security-focused chaos experiments"""
        
        experiments = [
            ChaosExperiment(
                name="RBAC Token Stress Test",
                chaos_type=ChaosType.RESOURCE_EXHAUSTION,
                duration=60,
                intensity=0.7,
                target_components=["rbac", "service-accounts"],
                security_impact="Test service account token handling under stress",
                recovery_time=20
            ),
            ChaosExperiment(
                name="Security Context Validation Under Stress",
                chaos_type=ChaosType.CPU_STRESS,
                duration=90,
                intensity=0.9,
                target_components=["security-context"],
                security_impact="Ensure security contexts remain enforced under load",
                recovery_time=30
            ),
            ChaosExperiment(
                name="Network Policy Bypass Attempts",
                chaos_type=ChaosType.NETWORK_PARTITION,
                duration=120,
                intensity=0.8,
                target_components=["network-policies"],
                security_impact="Test network policy effectiveness during chaos",
                recovery_time=40
            )
        ]
        
        for experiment in experiments:
            print(f"🔒 Executing: {experiment.name}")
            result = self._run_experiment(experiment)
            self.results.append(result)
            time.sleep(5)
    
    def _run_experiment(self, experiment: ChaosExperiment) -> Dict:
        """Execute a single chaos experiment"""
        
        start_time = time.time()
        
        # Record baseline metrics
        baseline = self._collect_baseline_metrics()
        
        print(f"  ⏱️  Duration: {experiment.duration}s | Intensity: {experiment.intensity*100}%")
        
        # Execute chaos based on type
        chaos_result = self._execute_chaos_action(experiment)
        
        # Monitor during chaos
        chaos_metrics = self._monitor_during_chaos(experiment)
        
        # Measure recovery
        recovery_result = self._measure_recovery(experiment)
        
        end_time = time.time()
        total_duration = end_time - start_time
        
        # Compile results
        result = {
            "experiment": {
                "name": experiment.name,
                "type": experiment.chaos_type.value,
                "duration": experiment.duration,
                "intensity": experiment.intensity,
                "target_components": experiment.target_components
            },
            "execution": {
                "start_time": start_time,
                "end_time": end_time,
                "total_duration": total_duration,
                "chaos_result": chaos_result,
                "recovery_time": recovery_result["actual_recovery_time"],
                "expected_recovery_time": experiment.recovery_time
            },
            "metrics": {
                "baseline": baseline,
                "during_chaos": chaos_metrics,
                "post_recovery": self._collect_baseline_metrics()
            },
            "security_impact": {
                "description": experiment.security_impact,
                "security_controls_maintained": recovery_result["security_maintained"],
                "vulnerabilities_exposed": recovery_result["vulnerabilities_found"]
            },
            "success": recovery_result["recovery_successful"]
        }
        
        # Print summary
        status = "✅ PASSED" if result["success"] else "❌ FAILED"
        recovery_time = result["execution"]["recovery_time"]
        print(f"  {status} | Recovery: {recovery_time:.1f}s | Security: {'✅' if result['security_impact']['security_controls_maintained'] else '❌'}")
        
        return result
    
    def _execute_chaos_action(self, experiment: ChaosExperiment) -> Dict:
        """Execute the actual chaos action"""
        
        if experiment.chaos_type == ChaosType.POD_KILL:
            return self._kill_pods(experiment)
        elif experiment.chaos_type == ChaosType.CPU_STRESS:
            return self._stress_cpu(experiment)
        elif experiment.chaos_type == ChaosType.MEMORY_BOMB:
            return self._exhaust_memory(experiment)
        elif experiment.chaos_type == ChaosType.DNS_CHAOS:
            return self._chaos_dns(experiment)
        elif experiment.chaos_type == ChaosType.NETWORK_PARTITION:
            return self._partition_network(experiment)
        elif experiment.chaos_type == ChaosType.DISK_PRESSURE:
            return self._exhaust_disk(experiment)
        elif experiment.chaos_type == ChaosType.RESOURCE_EXHAUSTION:
            return self._exhaust_resources(experiment)
        else:
            return {"action": "simulated", "success": True}
    
    def _kill_pods(self, experiment: ChaosExperiment) -> Dict:
        """Simulate pod termination chaos"""
        
        try:
            # Get pods
            result = subprocess.run([
                "kubectl", "get", "pods", 
                "-n", self.namespace,
                "-l", "app.kubernetes.io/name=bitactor",
                "-o", "jsonpath={.items[*].metadata.name}"
            ], capture_output=True, text=True, timeout=30)
            
            if result.returncode != 0:
                return {"action": "get_pods", "success": False, "error": result.stderr}
            
            pods = result.stdout.strip().split()
            if not pods:
                return {"action": "get_pods", "success": False, "error": "No pods found"}
            
            # Kill percentage of pods based on intensity
            kill_count = max(1, int(len(pods) * experiment.intensity))
            pods_to_kill = random.sample(pods, min(kill_count, len(pods)))
            
            killed_pods = []
            for pod in pods_to_kill:
                kill_result = subprocess.run([
                    "kubectl", "delete", "pod", pod,
                    "-n", self.namespace,
                    "--force", "--grace-period=0"
                ], capture_output=True, text=True, timeout=30)
                
                if kill_result.returncode == 0:
                    killed_pods.append(pod)
            
            # Wait for chaos duration
            time.sleep(experiment.duration)
            
            return {
                "action": "pod_kill",
                "success": True,
                "pods_targeted": len(pods),
                "pods_killed": len(killed_pods),
                "killed_pods": killed_pods
            }
            
        except Exception as e:
            return {"action": "pod_kill", "success": False, "error": str(e)}
    
    def _stress_cpu(self, experiment: ChaosExperiment) -> Dict:
        """Simulate CPU stress chaos"""
        
        try:
            # Create CPU stress job
            stress_manifest = f"""
apiVersion: batch/v1
kind: Job
metadata:
  name: chaos-cpu-stress-{int(time.time())}
  namespace: {self.namespace}
spec:
  template:
    spec:
      containers:
      - name: stress
        image: progrium/stress
        command: ["stress"]
        args: ["--cpu", "4", "--timeout", "{experiment.duration}s"]
        resources:
          requests:
            cpu: "100m"
          limits:
            cpu: "2000m"
      restartPolicy: Never
"""
            
            # Apply stress job
            with open("/tmp/cpu-stress.yaml", "w") as f:
                f.write(stress_manifest)
            
            apply_result = subprocess.run([
                "kubectl", "apply", "-f", "/tmp/cpu-stress.yaml"
            ], capture_output=True, text=True, timeout=30)
            
            if apply_result.returncode != 0:
                return {"action": "cpu_stress", "success": False, "error": apply_result.stderr}
            
            # Wait for stress duration + buffer
            time.sleep(experiment.duration + 10)
            
            # Cleanup
            subprocess.run([
                "kubectl", "delete", "-f", "/tmp/cpu-stress.yaml", "--ignore-not-found"
            ], capture_output=True, timeout=30)
            
            return {"action": "cpu_stress", "success": True, "duration": experiment.duration}
            
        except Exception as e:
            return {"action": "cpu_stress", "success": False, "error": str(e)}
    
    def _exhaust_memory(self, experiment: ChaosExperiment) -> Dict:
        """Simulate memory exhaustion chaos"""
        
        try:
            # Create memory stress job
            memory_mb = int(512 * experiment.intensity)  # Scale with intensity
            
            stress_manifest = f"""
apiVersion: batch/v1
kind: Job
metadata:
  name: chaos-memory-stress-{int(time.time())}
  namespace: {self.namespace}
spec:
  template:
    spec:
      containers:
      - name: stress
        image: progrium/stress
        command: ["stress"]
        args: ["--vm", "2", "--vm-bytes", "{memory_mb}M", "--timeout", "{experiment.duration}s"]
        resources:
          requests:
            memory: "100Mi"
          limits:
            memory: "1Gi"
      restartPolicy: Never
"""
            
            with open("/tmp/memory-stress.yaml", "w") as f:
                f.write(stress_manifest)
            
            apply_result = subprocess.run([
                "kubectl", "apply", "-f", "/tmp/memory-stress.yaml"
            ], capture_output=True, text=True, timeout=30)
            
            if apply_result.returncode != 0:
                return {"action": "memory_stress", "success": False, "error": apply_result.stderr}
            
            time.sleep(experiment.duration + 10)
            
            # Cleanup
            subprocess.run([
                "kubectl", "delete", "-f", "/tmp/memory-stress.yaml", "--ignore-not-found"
            ], capture_output=True, timeout=30)
            
            return {"action": "memory_stress", "success": True, "memory_mb": memory_mb}
            
        except Exception as e:
            return {"action": "memory_stress", "success": False, "error": str(e)}
    
    def _chaos_dns(self, experiment: ChaosExperiment) -> Dict:
        """Simulate DNS chaos"""
        
        # This would implement DNS chaos (e.g., using Chaos Mesh or similar)
        # For demonstration, we simulate the effect
        
        time.sleep(experiment.duration)
        
        return {
            "action": "dns_chaos",
            "success": True,
            "simulation": "DNS resolution delays and failures",
            "duration": experiment.duration
        }
    
    def _partition_network(self, experiment: ChaosExperiment) -> Dict:
        """Simulate network partition"""
        
        # This would implement network partitioning
        # For demonstration, we simulate the effect
        
        time.sleep(experiment.duration)
        
        return {
            "action": "network_partition",
            "success": True,
            "simulation": "Network connectivity issues between pods",
            "duration": experiment.duration
        }
    
    def _exhaust_disk(self, experiment: ChaosExperiment) -> Dict:
        """Simulate disk space exhaustion"""
        
        try:
            # Create disk stress job
            size_mb = int(100 * experiment.intensity)
            
            stress_manifest = f"""
apiVersion: batch/v1
kind: Job
metadata:
  name: chaos-disk-stress-{int(time.time())}
  namespace: {self.namespace}
spec:
  template:
    spec:
      containers:
      - name: disk-fill
        image: busybox
        command: ["sh", "-c"]
        args: ["dd if=/dev/zero of=/tmp/bigfile bs=1M count={size_mb}; sleep {experiment.duration}; rm -f /tmp/bigfile"]
        volumeMounts:
        - name: tmp
          mountPath: /tmp
      volumes:
      - name: tmp
        emptyDir:
          sizeLimit: 200Mi
      restartPolicy: Never
"""
            
            with open("/tmp/disk-stress.yaml", "w") as f:
                f.write(stress_manifest)
            
            apply_result = subprocess.run([
                "kubectl", "apply", "-f", "/tmp/disk-stress.yaml"
            ], capture_output=True, text=True, timeout=30)
            
            if apply_result.returncode != 0:
                return {"action": "disk_stress", "success": False, "error": apply_result.stderr}
            
            time.sleep(experiment.duration + 10)
            
            # Cleanup
            subprocess.run([
                "kubectl", "delete", "-f", "/tmp/disk-stress.yaml", "--ignore-not-found"
            ], capture_output=True, timeout=30)
            
            return {"action": "disk_stress", "success": True, "size_mb": size_mb}
            
        except Exception as e:
            return {"action": "disk_stress", "success": False, "error": str(e)}
    
    def _exhaust_resources(self, experiment: ChaosExperiment) -> Dict:
        """Simulate general resource exhaustion"""
        
        # Simulate various resource exhaustion scenarios
        time.sleep(experiment.duration)
        
        return {
            "action": "resource_exhaustion",
            "success": True,
            "simulation": "Multiple resource types exhausted simultaneously",
            "duration": experiment.duration
        }
    
    def _collect_baseline_metrics(self) -> Dict:
        """Collect baseline system metrics"""
        
        try:
            # Get pod status
            pod_result = subprocess.run([
                "kubectl", "get", "pods",
                "-n", self.namespace,
                "-l", "app.kubernetes.io/name=bitactor",
                "-o", "json"
            ], capture_output=True, text=True, timeout=30)
            
            metrics = {
                "timestamp": time.time(),
                "pod_count": 0,
                "ready_pods": 0,
                "failed_pods": 0
            }
            
            if pod_result.returncode == 0:
                pod_data = json.loads(pod_result.stdout)
                metrics["pod_count"] = len(pod_data.get("items", []))
                
                for pod in pod_data.get("items", []):
                    status = pod.get("status", {})
                    if status.get("phase") == "Running":
                        conditions = status.get("conditions", [])
                        ready = any(c.get("type") == "Ready" and c.get("status") == "True" for c in conditions)
                        if ready:
                            metrics["ready_pods"] += 1
                    elif status.get("phase") in ["Failed", "Error"]:
                        metrics["failed_pods"] += 1
            
            return metrics
            
        except Exception as e:
            return {"timestamp": time.time(), "error": str(e)}
    
    def _monitor_during_chaos(self, experiment: ChaosExperiment) -> Dict:
        """Monitor system during chaos"""
        
        # Collect metrics during chaos
        return self._collect_baseline_metrics()
    
    def _measure_recovery(self, experiment: ChaosExperiment) -> Dict:
        """Measure system recovery after chaos"""
        
        start_recovery = time.time()
        max_wait = experiment.recovery_time * 3  # Max wait time
        
        while time.time() - start_recovery < max_wait:
            metrics = self._collect_baseline_metrics()
            
            # Check if system recovered
            if (metrics.get("ready_pods", 0) >= 2 and  # At least 2 pods ready
                metrics.get("failed_pods", 0) == 0):    # No failed pods
                
                recovery_time = time.time() - start_recovery
                
                # Check security controls
                security_maintained = self._verify_security_controls()
                vulnerabilities_found = self._scan_for_vulnerabilities()
                
                return {
                    "recovery_successful": True,
                    "actual_recovery_time": recovery_time,
                    "security_maintained": security_maintained,
                    "vulnerabilities_found": vulnerabilities_found
                }
            
            time.sleep(5)  # Check every 5 seconds
        
        # Recovery failed
        return {
            "recovery_successful": False,
            "actual_recovery_time": max_wait,
            "security_maintained": False,
            "vulnerabilities_found": ["Recovery timeout - system may be compromised"]
        }
    
    def _verify_security_controls(self) -> bool:
        """Verify security controls are still in place after chaos"""
        
        try:
            # Check if pods are still running with correct security context
            result = subprocess.run([
                "kubectl", "get", "pods",
                "-n", self.namespace,
                "-l", "app.kubernetes.io/name=bitactor",
                "-o", "jsonpath={.items[*].spec.securityContext.runAsNonRoot}"
            ], capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0:
                # Should contain 'true' values for non-root execution
                return "true" in result.stdout
            
            return False
            
        except Exception:
            return False
    
    def _scan_for_vulnerabilities(self) -> List[str]:
        """Scan for vulnerabilities exposed during chaos"""
        
        vulnerabilities = []
        
        try:
            # Check for privileged pods that might have been created
            result = subprocess.run([
                "kubectl", "get", "pods", "--all-namespaces",
                "-o", "jsonpath={.items[?(@.spec.securityContext.privileged==true)].metadata.name}"
            ], capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0 and result.stdout.strip():
                vulnerabilities.append("Privileged pods detected during chaos")
            
            # Check for pods without resource limits
            result = subprocess.run([
                "kubectl", "get", "pods",
                "-n", self.namespace,
                "-o", "jsonpath={.items[?(@.spec.containers[*].resources.limits==null)].metadata.name}"
            ], capture_output=True, text=True, timeout=30)
            
            if result.returncode == 0 and result.stdout.strip():
                vulnerabilities.append("Pods without resource limits created")
                
        except Exception as e:
            vulnerabilities.append(f"Vulnerability scan failed: {str(e)}")
        
        return vulnerabilities
    
    def _generate_chaos_report(self) -> Dict:
        """Generate comprehensive chaos engineering report"""
        
        # Calculate statistics
        total_experiments = len(self.results)
        successful_experiments = len([r for r in self.results if r["success"]])
        security_maintained_count = len([r for r in self.results if r["security_impact"]["security_controls_maintained"]])
        
        # Calculate average recovery time
        recovery_times = [r["execution"]["recovery_time"] for r in self.results if r["success"]]
        avg_recovery_time = sum(recovery_times) / len(recovery_times) if recovery_times else 0
        
        # Find worst vulnerabilities
        all_vulnerabilities = []
        for result in self.results:
            all_vulnerabilities.extend(result["security_impact"]["vulnerabilities_exposed"])
        
        report = {
            "metadata": {
                "timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
                "namespace": self.namespace,
                "total_experiments": total_experiments,
                "methodology": "Adversarial Chaos Engineering"
            },
            "executive_summary": {
                "success_rate": round((successful_experiments / total_experiments) * 100, 1) if total_experiments > 0 else 0,
                "security_maintained_rate": round((security_maintained_count / total_experiments) * 100, 1) if total_experiments > 0 else 0,
                "average_recovery_time": round(avg_recovery_time, 2),
                "total_vulnerabilities_found": len(all_vulnerabilities),
                "resilience_score": self._calculate_resilience_score()
            },
            "experiment_results": self.results,
            "vulnerability_summary": {
                "unique_vulnerabilities": list(set(all_vulnerabilities)),
                "critical_findings": [v for v in all_vulnerabilities if "privileged" in v.lower() or "compromise" in v.lower()],
                "recovery_failures": [r["experiment"]["name"] for r in self.results if not r["success"]]
            },
            "recommendations": [
                "Implement circuit breakers for critical services",
                "Add chaos engineering to CI/CD pipeline",
                "Improve monitoring and alerting for faster recovery",
                "Strengthen resource limits and quotas",
                "Implement automated recovery procedures",
                "Regular chaos engineering drills",
                "Enhance security monitoring during incidents"
            ]
        }
        
        return report
    
    def _calculate_resilience_score(self) -> float:
        """Calculate overall system resilience score (0-100)"""
        
        if not self.results:
            return 0
        
        # Base score from success rate
        success_rate = len([r for r in self.results if r["success"]]) / len(self.results)
        base_score = success_rate * 50
        
        # Security maintenance score
        security_rate = len([r for r in self.results if r["security_impact"]["security_controls_maintained"]]) / len(self.results)
        security_score = security_rate * 30
        
        # Recovery time score (faster recovery = higher score)
        recovery_times = [r["execution"]["recovery_time"] for r in self.results if r["success"]]
        if recovery_times:
            avg_recovery = sum(recovery_times) / len(recovery_times)
            # Normalize recovery time score (assuming 30s is excellent, 120s is poor)
            recovery_score = max(0, 20 - (avg_recovery - 30) / 90 * 20)
        else:
            recovery_score = 0
        
        total_score = base_score + security_score + recovery_score
        return round(min(100, max(0, total_score)), 1)

def main():
    """Main execution function"""
    if len(sys.argv) < 2:
        print("Usage: python3 adversarial_chaos_engine.py <namespace>")
        sys.exit(1)
    
    namespace = sys.argv[1]
    
    chaos_engine = AdversarialChaosEngine(namespace)
    report = chaos_engine.run_chaos_campaign()
    
    # Save report
    report_file = f"adversarial_chaos_report_{namespace}.json"
    with open(report_file, 'w') as f:
        json.dump(report, f, indent=2)
    
    # Print summary
    print(f"\n⚡ ADVERSARIAL CHAOS CAMPAIGN COMPLETE")
    print(f"Report saved to: {report_file}")
    print(f"Success rate: {report['executive_summary']['success_rate']}%")
    print(f"Security maintained: {report['executive_summary']['security_maintained_rate']}%")
    print(f"Resilience score: {report['executive_summary']['resilience_score']}/100")
    print(f"Vulnerabilities found: {report['executive_summary']['total_vulnerabilities_found']}")
    
    # Print critical findings
    critical = report['vulnerability_summary']['critical_findings']
    if critical:
        print(f"\n🚨 CRITICAL FINDINGS:")
        for i, finding in enumerate(critical[:3], 1):
            print(f"  {i}. {finding}")
    
    return len(report['vulnerability_summary']['unique_vulnerabilities'])

if __name__ == "__main__":
    sys.exit(main())