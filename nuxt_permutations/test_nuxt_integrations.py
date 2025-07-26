#!/usr/bin/env python3
"""
Nuxt.js Integration Testing and OTEL Validation
Tests all Nuxt.js projects for proper functionality and TypeScript compliance
"""

import json
import subprocess
import time
import asyncio
from pathlib import Path
from datetime import datetime
from typing import Dict, List, Any

class NuxtIntegrationTester:
    def __init__(self, base_path: str = "/Users/sac/cns/nuxt_permutations"):
        self.base_path = Path(base_path)
        self.projects = [
            {
                "name": "nuxt-ash-graphql",
                "port": 3000,
                "type": "GraphQL Integration",
                "features": ["Apollo GraphQL", "Ash API", "Resource Management"]
            },
            {
                "name": "nuxt-bitactor-performance", 
                "port": 3001,
                "type": "Performance Dashboard",
                "features": ["WebSocket", "Real-time Charts", "Actor Monitoring"]
            },
            {
                "name": "nuxt-semantic-web",
                "port": 3002, 
                "type": "SSR/SSG Semantic Web",
                "features": ["JSON-LD", "TTL Parser", "SEO Optimization"]
            }
        ]
        self.test_results = {}
        
    async def run_comprehensive_tests(self) -> Dict[str, Any]:
        """Run all integration tests and generate OTEL metrics"""
        print("🧪 Starting Nuxt.js Integration Tests")
        print("=" * 60)
        
        results = {
            "test_run_id": f"nuxt_test_{int(time.time())}",
            "timestamp": datetime.now().isoformat(),
            "total_projects": len(self.projects),
            "projects": {},
            "overall_status": "pending"
        }
        
        for project in self.projects:
            print(f"\n🔍 Testing {project['name']}")
            project_results = await self.test_project(project)
            results["projects"][project["name"]] = project_results
            
        # Calculate overall status
        all_passed = all(
            proj["status"] == "passed" 
            for proj in results["projects"].values()
        )
        results["overall_status"] = "passed" if all_passed else "failed"
        
        # Generate OTEL metrics
        otel_metrics = self.generate_otel_metrics(results)
        results["otel_metrics"] = otel_metrics
        
        # Save results
        self.save_test_results(results)
        
        return results
        
    async def test_project(self, project: Dict[str, Any]) -> Dict[str, Any]:
        """Test individual Nuxt.js project"""
        project_path = self.base_path / project["name"]
        
        test_results = {
            "name": project["name"],
            "type": project["type"],
            "status": "pending",
            "tests": {},
            "metrics": {},
            "errors": []
        }
        
        # Test 1: Check project structure
        structure_test = self.test_project_structure(project_path)
        test_results["tests"]["structure"] = structure_test
        
        # Test 2: Check package.json and dependencies
        package_test = self.test_package_configuration(project_path)
        test_results["tests"]["package"] = package_test
        
        # Test 3: Check TypeScript compliance (should be JavaScript only)
        typescript_test = self.test_typescript_compliance(project_path)
        test_results["tests"]["no_typescript"] = typescript_test
        
        # Test 4: Validate Nuxt.js configuration
        config_test = self.test_nuxt_config(project_path)
        test_results["tests"]["nuxt_config"] = config_test
        
        # Test 5: Check component syntax
        component_test = self.test_component_syntax(project_path)
        test_results["tests"]["components"] = component_test
        
        # Test 6: Build test (if possible)
        build_test = await self.test_build_process(project_path)
        test_results["tests"]["build"] = build_test
        
        # Calculate overall project status
        all_tests_passed = all(
            test["status"] == "passed" 
            for test in test_results["tests"].values()
        )
        test_results["status"] = "passed" if all_tests_passed else "failed"
        
        return test_results
        
    def test_project_structure(self, project_path: Path) -> Dict[str, Any]:
        """Test if project has required structure"""
        required_files = [
            "package.json",
            "nuxt.config.js",  # Should be .js, not .ts
            "pages",
            "components" 
        ]
        
        missing_files = []
        for file_name in required_files:
            file_path = project_path / file_name
            if not file_path.exists():
                missing_files.append(file_name)
                
        return {
            "status": "passed" if not missing_files else "failed",
            "missing_files": missing_files,
            "message": "All required files present" if not missing_files else f"Missing: {missing_files}"
        }
        
    def test_package_configuration(self, project_path: Path) -> Dict[str, Any]:
        """Test package.json configuration"""
        package_file = project_path / "package.json"
        
        if not package_file.exists():
            return {"status": "failed", "message": "package.json not found"}
            
        try:
            with open(package_file) as f:
                package_data = json.load(f)
                
            # Check required scripts
            required_scripts = ["dev", "build", "generate"]
            missing_scripts = [
                script for script in required_scripts 
                if script not in package_data.get("scripts", {})
            ]
            
            # Check for TypeScript dependencies (should not have any)
            typescript_deps = []
            deps = {**package_data.get("dependencies", {}), **package_data.get("devDependencies", {})}
            for dep in deps:
                if "typescript" in dep.lower() or dep in ["@types/node", "@typescript-eslint/parser"]:
                    typescript_deps.append(dep)
                    
            issues = []
            if missing_scripts:
                issues.append(f"Missing scripts: {missing_scripts}")
            if typescript_deps:
                issues.append(f"TypeScript dependencies found: {typescript_deps}")
                
            return {
                "status": "passed" if not issues else "failed",
                "issues": issues,
                "message": "Package configuration valid" if not issues else f"Issues: {issues}"
            }
            
        except Exception as e:
            return {"status": "failed", "message": f"Error reading package.json: {e}"}
            
    def test_typescript_compliance(self, project_path: Path) -> Dict[str, Any]:
        """Ensure no TypeScript files are present"""
        typescript_files = []
        
        for file_path in project_path.rglob("*.ts"):
            if file_path.name != "types" and not file_path.name.endswith(".d.ts"):
                typescript_files.append(str(file_path.relative_to(project_path)))
                
        for file_path in project_path.rglob("*.tsx"):
            typescript_files.append(str(file_path.relative_to(project_path)))
            
        return {
            "status": "passed" if not typescript_files else "failed",
            "typescript_files": typescript_files,
            "message": "No TypeScript files found" if not typescript_files else f"TypeScript files found: {typescript_files}"
        }
        
    def test_nuxt_config(self, project_path: Path) -> Dict[str, Any]:
        """Test Nuxt.js configuration"""
        config_file = project_path / "nuxt.config.js"
        
        if not config_file.exists():
            return {"status": "failed", "message": "nuxt.config.js not found"}
            
        try:
            with open(config_file) as f:
                config_content = f.read()
                
            # Basic validation - should be JavaScript, not TypeScript
            if "export default defineNuxtConfig" in config_content:
                return {
                    "status": "passed",
                    "message": "Valid Nuxt.js configuration found"
                }
            else:
                return {
                    "status": "failed", 
                    "message": "Invalid Nuxt.js configuration format"
                }
                
        except Exception as e:
            return {"status": "failed", "message": f"Error reading nuxt.config.js: {e}"}
            
    def test_component_syntax(self, project_path: Path) -> Dict[str, Any]:
        """Test Vue component syntax for JavaScript compliance"""
        component_issues = []
        
        # Check Vue files for TypeScript usage
        for vue_file in project_path.rglob("*.vue"):
            try:
                with open(vue_file) as f:
                    content = f.read()
                    
                # Check for TypeScript indicators
                if 'lang="ts"' in content or 'lang="typescript"' in content:
                    component_issues.append(f"{vue_file.relative_to(project_path)}: TypeScript lang attribute")
                    
                if ': string' in content or ': number' in content or 'interface ' in content:
                    component_issues.append(f"{vue_file.relative_to(project_path)}: TypeScript syntax detected")
                    
            except Exception as e:
                component_issues.append(f"{vue_file.relative_to(project_path)}: Error reading file - {e}")
                
        return {
            "status": "passed" if not component_issues else "failed",
            "issues": component_issues,
            "message": "All components use JavaScript" if not component_issues else f"Component issues: {component_issues}"
        }
        
    async def test_build_process(self, project_path: Path) -> Dict[str, Any]:
        """Test if project can build successfully"""
        try:
            # Change to project directory and run build
            process = await asyncio.create_subprocess_exec(
                "npm", "run", "build",
                cwd=project_path,
                stdout=asyncio.subprocess.PIPE,
                stderr=asyncio.subprocess.PIPE
            )
            
            stdout, stderr = await asyncio.wait_for(process.communicate(), timeout=120)
            
            if process.returncode == 0:
                return {
                    "status": "passed",
                    "message": "Build completed successfully"
                }
            else:
                return {
                    "status": "failed",
                    "message": f"Build failed: {stderr.decode()}"
                }
                
        except asyncio.TimeoutError:
            return {
                "status": "failed",
                "message": "Build timed out after 120 seconds"
            }
        except Exception as e:
            return {
                "status": "warning",
                "message": f"Could not test build: {e}"
            }
            
    def generate_otel_metrics(self, results: Dict[str, Any]) -> Dict[str, Any]:
        """Generate OpenTelemetry metrics for the test results"""
        otel_metrics = {
            "span_name": "nuxt_integration_tests",
            "trace_id": f"trace_{results['test_run_id']}",
            "start_time": results["timestamp"],
            "end_time": datetime.now().isoformat(),
            "attributes": {
                "test.framework": "nuxt_integration_tester",
                "test.total_projects": results["total_projects"],
                "test.overall_status": results["overall_status"]
            },
            "metrics": {
                "projects_tested": results["total_projects"],
                "projects_passed": sum(1 for p in results["projects"].values() if p["status"] == "passed"),
                "projects_failed": sum(1 for p in results["projects"].values() if p["status"] == "failed"),
                "total_tests_run": sum(len(p["tests"]) for p in results["projects"].values()),
                "test_success_rate": 0
            },
            "events": []
        }
        
        # Calculate success rate
        total_tests = otel_metrics["metrics"]["total_tests_run"]
        if total_tests > 0:
            passed_tests = sum(
                sum(1 for test in project["tests"].values() if test["status"] == "passed")
                for project in results["projects"].values()
            )
            otel_metrics["metrics"]["test_success_rate"] = round(passed_tests / total_tests * 100, 2)
            
        # Add events for each project
        for project_name, project_results in results["projects"].items():
            otel_metrics["events"].append({
                "name": f"project_test_completed",
                "timestamp": datetime.now().isoformat(),
                "attributes": {
                    "project.name": project_name,
                    "project.status": project_results["status"],
                    "project.type": project_results["type"]
                }
            })
            
        return otel_metrics
        
    def save_test_results(self, results: Dict[str, Any]):
        """Save test results to file"""
        results_file = self.base_path / f"test_results_{results['test_run_id']}.json"
        
        with open(results_file, 'w') as f:
            json.dump(results, f, indent=2)
            
        print(f"\n📊 Test results saved to: {results_file}")
        
    def generate_mermaid_report(self, results: Dict[str, Any]) -> str:
        """Generate Mermaid diagram showing test results"""
        mermaid = f"""```mermaid
graph TD
    A[Nuxt.js Integration Tests] --> B[Total Projects: {results['total_projects']}]
    B --> C[Overall Status: {results['overall_status'].upper()}]
    
    %% Project Results
"""
        
        for project_name, project_results in results["projects"].items():
            status_color = "green" if project_results["status"] == "passed" else "red"
            mermaid += f"    C --> {project_name.replace('-', '_')}[{project_name}<br/>Status: {project_results['status']}<br/>Type: {project_results['type']}]\n"
            mermaid += f"    class {project_name.replace('-', '_')} {status_color}\n"
            
        mermaid += "\n    %% OTEL Metrics\n"
        otel = results["otel_metrics"]["metrics"]
        mermaid += f"    C --> OTEL[OTEL Metrics<br/>Success Rate: {otel['test_success_rate']}%<br/>Total Tests: {otel['total_tests_run']}]\n"
        mermaid += "    class OTEL blue\n"
        
        mermaid += "\n    classDef green fill:#d4edda,stroke:#155724,color:#155724\n"
        mermaid += "    classDef red fill:#f8d7da,stroke:#721c24,color:#721c24\n"
        mermaid += "    classDef blue fill:#cce5ff,stroke:#004085,color:#004085\n"
        mermaid += "```"
        
        return mermaid


async def main():
    """Run the Nuxt.js integration tests"""
    tester = NuxtIntegrationTester()
    
    results = await tester.run_comprehensive_tests()
    
    # Generate report
    mermaid_report = tester.generate_mermaid_report(results)
    
    # Save Mermaid report
    report_file = tester.base_path / "NUXT_INTEGRATION_TEST_RESULTS.md"
    with open(report_file, 'w') as f:
        f.write(f"# Nuxt.js Integration Test Results\n\n")
        f.write(f"**Test Run ID:** {results['test_run_id']}  \n")
        f.write(f"**Timestamp:** {results['timestamp']}  \n")
        f.write(f"**Overall Status:** {results['overall_status'].upper()}  \n\n")
        f.write(f"## Test Results Summary\n\n")
        f.write(mermaid_report)
        f.write(f"\n\n## OTEL Metrics\n\n")
        f.write(f"```json\n{json.dumps(results['otel_metrics'], indent=2)}\n```\n")
        
    print(f"\n📋 Mermaid report saved to: {report_file}")
    print(f"\n✅ Test Summary:")
    print(f"   - Projects Tested: {results['total_projects']}")
    print(f"   - Overall Status: {results['overall_status'].upper()}")
    print(f"   - Success Rate: {results['otel_metrics']['metrics']['test_success_rate']}%")
    
    return results


if __name__ == "__main__":
    asyncio.run(main())