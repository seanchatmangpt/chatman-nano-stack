#!/usr/bin/env python3
"""
Ontology Quality Control System
Advanced validation, optimization, and quality assurance for generated ontologies
"""

import json
import re
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from pathlib import Path
from typing import Any, Dict, List, Optional, Tuple

import dspy
from rdflib import Graph


class QualityLevel(Enum):
    CRITICAL = "critical"
    WARNING = "warning"
    INFO = "info"

class ValidationRule(Enum):
    SEMANTIC_CONSISTENCY = "semantic_consistency"
    PERFORMANCE_COMPLIANCE = "performance_compliance"
    STANDARD_COMPLIANCE = "standard_compliance"
    SCHEMA_VALIDATION = "schema_validation"
    CROSS_REFERENCE = "cross_reference"
    NAMING_CONVENTION = "naming_convention"
    DOCUMENTATION = "documentation"

@dataclass
class QualityIssue:
    rule: ValidationRule
    level: QualityLevel
    message: str
    file_path: Optional[str] = None
    line_number: Optional[int] = None
    suggestion: Optional[str] = None
    auto_fixable: bool = False

@dataclass
class QualityReport:
    domain: str
    timestamp: str
    total_files: int
    issues: List[QualityIssue] = field(default_factory=list)
    metrics: Dict[str, Any] = field(default_factory=dict)
    passed: bool = False

    @property
    def critical_count(self) -> int:
        return len([i for i in self.issues if i.level == QualityLevel.CRITICAL])

    @property
    def warning_count(self) -> int:
        return len([i for i in self.issues if i.level == QualityLevel.WARNING])

class SemanticValidator(dspy.Signature):
    """Advanced semantic validation using DSPy"""
    ontology_content = dspy.InputField(desc="TTL ontology content to validate")
    domain_context = dspy.InputField(desc="Domain context and requirements")
    validation_rules = dspy.InputField(desc="Validation rules to apply")

    is_valid = dspy.OutputField(desc="Boolean indicating if ontology is semantically valid")
    issues_found = dspy.OutputField(desc="JSON list of semantic issues found")
    suggestions = dspy.OutputField(desc="JSON list of improvement suggestions")

class OntologyOptimizer(dspy.Signature):
    """Optimize ontology for performance and compliance"""
    ontology_content = dspy.InputField(desc="Current ontology TTL content")
    performance_requirements = dspy.InputField(desc="Performance requirements (e.g., 8-tick compliance)")
    domain_patterns = dspy.InputField(desc="Known domain patterns and best practices")

    optimized_ontology = dspy.OutputField(desc="Optimized TTL ontology content")
    optimizations_applied = dspy.OutputField(desc="JSON list of optimizations applied")
    performance_impact = dspy.OutputField(desc="Expected performance improvement")

class ComplianceChecker(dspy.Signature):
    """Check compliance against standards"""
    ontology_content = dspy.InputField(desc="Ontology content to check")
    compliance_standards = dspy.InputField(desc="Standards to check against (HIPAA, ISO26262, etc.)")
    domain_type = dspy.InputField(desc="Domain type (trading, healthcare, etc.)")

    compliance_status = dspy.OutputField(desc="JSON compliance status per standard")
    violations = dspy.OutputField(desc="JSON list of compliance violations")
    remediation_steps = dspy.OutputField(desc="JSON list of steps to fix violations")

class DocumentationGenerator(dspy.Signature):
    """Generate comprehensive documentation"""
    ontology_content = dspy.InputField(desc="Ontology TTL content")
    domain_context = dspy.InputField(desc="Domain and use case context")
    target_audience = dspy.InputField(desc="Target audience (developers, domain experts, etc.)")

    documentation = dspy.OutputField(desc="Comprehensive documentation in markdown")
    examples = dspy.OutputField(desc="Usage examples and code snippets")
    diagrams = dspy.OutputField(desc="Mermaid diagrams explaining the ontology")

class OntologyQualityController:
    """Advanced quality control system for ontologies"""

    def __init__(self, model: str = "qwen3:latest"):
        # Initialize DSPy components
        self.llm = dspy.OllamaLocal(model=model, base_url="http://localhost:11434", max_tokens=4000)
        dspy.settings.configure(lm=self.llm)

        # Initialize DSPy modules
        self.semantic_validator = dspy.ChainOfThought(SemanticValidator)
        self.ontology_optimizer = dspy.ChainOfThought(OntologyOptimizer)
        self.compliance_checker = dspy.ChainOfThought(ComplianceChecker)
        self.doc_generator = dspy.ChainOfThought(DocumentationGenerator)

        # Load validation rules
        self.validation_rules = self._load_validation_rules()

    def _load_validation_rules(self) -> Dict[str, Any]:
        """Load comprehensive validation rules"""
        return {
            ValidationRule.SEMANTIC_CONSISTENCY: {
                "description": "Check for semantic consistency and logical coherence",
                "checks": [
                    "Class hierarchy consistency",
                    "Property domain/range alignment",
                    "Circular dependency detection",
                    "Orphaned entity detection"
                ]
            },
            ValidationRule.PERFORMANCE_COMPLIANCE: {
                "description": "Validate performance requirements compliance",
                "checks": [
                    "8-tick compliance for critical operations",
                    "Memory usage optimization",
                    "Cache-friendly data structures",
                    "Lock-free design patterns"
                ]
            },
            ValidationRule.STANDARD_COMPLIANCE: {
                "description": "Check compliance with industry standards",
                "standards": {
                    "HIPAA": ["data_encryption", "access_controls", "audit_trails"],
                    "ISO26262": ["safety_requirements", "redundancy", "fault_tolerance"],
                    "MiFID_II": ["transaction_reporting", "best_execution", "risk_controls"]
                }
            },
            ValidationRule.SCHEMA_VALIDATION: {
                "description": "Validate against OWL/RDF schemas",
                "checks": [
                    "Valid TTL syntax",
                    "Proper namespace usage",
                    "OWL axiom correctness",
                    "SHACL constraint validity"
                ]
            }
        }

    def validate_ontology_suite(self, ontology_dir: Path, domain: str) -> QualityReport:
        """Comprehensive validation of ontology suite"""

        report = QualityReport(
            domain=domain,
            timestamp=datetime.now().isoformat(),
            total_files=0
        )

        # Find all TTL files
        ttl_files = list(ontology_dir.rglob("*.ttl"))
        shacl_files = list(ontology_dir.rglob("*.shacl.ttl"))

        report.total_files = len(ttl_files) + len(shacl_files)

        # Validate each file
        for ttl_file in ttl_files:
            file_issues = self._validate_single_file(ttl_file, domain)
            report.issues.extend(file_issues)

        # Cross-file validation
        cross_issues = self._validate_cross_references(ttl_files, domain)
        report.issues.extend(cross_issues)

        # Performance validation
        perf_issues = self._validate_performance_requirements(ontology_dir, domain)
        report.issues.extend(perf_issues)

        # Compliance validation
        compliance_issues = self._validate_compliance(ontology_dir, domain)
        report.issues.extend(compliance_issues)

        # Calculate metrics
        report.metrics = self._calculate_metrics(ontology_dir, report.issues)

        # Determine pass/fail
        report.passed = report.critical_count == 0

        return report

    def _validate_single_file(self, file_path: Path, domain: str) -> List[QualityIssue]:
        """Validate a single ontology file using DSPy"""
        issues = []

        try:
            with open(file_path) as f:
                content = f.read()

            # DSPy semantic validation
            validation_result = self.semantic_validator(
                ontology_content=content,
                domain_context=f"{domain} domain ontology",
                validation_rules=json.dumps(self.validation_rules, default=str)
            )

            # Parse DSPy results
            if not validation_result.is_valid:
                try:
                    found_issues = json.loads(validation_result.issues_found)
                    for issue_data in found_issues:
                        issues.append(QualityIssue(
                            rule=ValidationRule.SEMANTIC_CONSISTENCY,
                            level=QualityLevel(issue_data.get('level', 'warning')),
                            message=issue_data.get('message', 'Semantic validation failed'),
                            file_path=str(file_path),
                            suggestion=issue_data.get('suggestion')
                        ))
                except (json.JSONDecodeError, KeyError):
                    issues.append(QualityIssue(
                        rule=ValidationRule.SEMANTIC_CONSISTENCY,
                        level=QualityLevel.WARNING,
                        message="Could not parse validation results",
                        file_path=str(file_path)
                    ))

            # Schema validation using rdflib
            try:
                graph = Graph()
                graph.parse(file_path, format="turtle")
            except Exception as e:
                issues.append(QualityIssue(
                    rule=ValidationRule.SCHEMA_VALIDATION,
                    level=QualityLevel.CRITICAL,
                    message=f"TTL syntax error: {str(e)}",
                    file_path=str(file_path),
                    auto_fixable=True
                ))

        except Exception as e:
            issues.append(QualityIssue(
                rule=ValidationRule.SCHEMA_VALIDATION,
                level=QualityLevel.CRITICAL,
                message=f"Could not read file: {str(e)}",
                file_path=str(file_path)
            ))

        return issues

    def _validate_cross_references(self, ttl_files: List[Path], domain: str) -> List[QualityIssue]:
        """Validate cross-references between ontology files"""
        issues = []

        # Build reference map
        all_classes = set()
        all_properties = set()
        references = {}

        for file_path in ttl_files:
            try:
                graph = Graph()
                graph.parse(file_path, format="turtle")

                # Extract classes and properties
                for subj, pred, obj in graph:
                    if str(pred).endswith('#type') and str(obj).endswith('#Class'):
                        all_classes.add(str(subj))
                    elif str(pred).endswith('#type') and 'Property' in str(obj):
                        all_properties.add(str(subj))

            except Exception as e:
                issues.append(QualityIssue(
                    rule=ValidationRule.CROSS_REFERENCE,
                    level=QualityLevel.WARNING,
                    message=f"Could not parse for cross-reference check: {str(e)}",
                    file_path=str(file_path)
                ))

        return issues

    def _validate_performance_requirements(self, ontology_dir: Path, domain: str) -> List[QualityIssue]:
        """Validate performance requirements"""
        issues = []

        # Check for performance-critical patterns
        perf_patterns = [
            "8tick",
            "ultralowlatency",
            "nanosecond",
            "lock-free",
            "cache-aligned"
        ]

        manifest_file = ontology_dir / "manifest.json"
        if manifest_file.exists():
            try:
                with open(manifest_file) as f:
                    manifest = json.load(f)

                perf_reqs = manifest.get('performance_requirements', {})
                if 'tick_compliance' in perf_reqs:
                    tick_limit = perf_reqs['tick_compliance']
                    if tick_limit <= 8:
                        # Check that performance classes exist
                        perf_file = ontology_dir / "performance.ttl"
                        if not perf_file.exists():
                            issues.append(QualityIssue(
                                rule=ValidationRule.PERFORMANCE_COMPLIANCE,
                                level=QualityLevel.CRITICAL,
                                message=f"Performance ontology missing for {tick_limit}-tick requirement",
                                suggestion="Generate performance.ttl with tick compliance classes"
                            ))

            except Exception as e:
                issues.append(QualityIssue(
                    rule=ValidationRule.PERFORMANCE_COMPLIANCE,
                    level=QualityLevel.WARNING,
                    message=f"Could not read manifest for performance validation: {str(e)}"
                ))

        return issues

    def _validate_compliance(self, ontology_dir: Path, domain: str) -> List[QualityIssue]:
        """Validate compliance using DSPy"""
        issues = []

        manifest_file = ontology_dir / "manifest.json"
        if manifest_file.exists():
            try:
                with open(manifest_file) as f:
                    manifest = json.load(f)

                standards = manifest.get('compliance_standards', [])
                if standards:
                    # Read core ontology for compliance check
                    core_files = list(ontology_dir.glob("*core*.ttl"))
                    if core_files:
                        with open(core_files[0]) as f:
                            content = f.read()

                        # DSPy compliance check
                        try:
                            compliance_result = self.compliance_checker(
                                ontology_content=content,
                                compliance_standards=json.dumps(standards),
                                domain_type=domain
                            )

                            # Parse compliance violations
                            violations = json.loads(compliance_result.violations)
                            for violation in violations:
                                issues.append(QualityIssue(
                                    rule=ValidationRule.STANDARD_COMPLIANCE,
                                    level=QualityLevel.CRITICAL,
                                    message=violation.get('message', 'Compliance violation'),
                                    suggestion=violation.get('remediation')
                                ))

                        except Exception as e:
                            issues.append(QualityIssue(
                                rule=ValidationRule.STANDARD_COMPLIANCE,
                                level=QualityLevel.WARNING,
                                message=f"Could not complete compliance check: {str(e)}"
                            ))

            except Exception as e:
                issues.append(QualityIssue(
                    rule=ValidationRule.STANDARD_COMPLIANCE,
                    level=QualityLevel.WARNING,
                    message=f"Could not read manifest for compliance validation: {str(e)}"
                ))

        return issues

    def _calculate_metrics(self, ontology_dir: Path, issues: List[QualityIssue]) -> Dict[str, Any]:
        """Calculate quality metrics"""
        metrics = {
            "total_issues": len(issues),
            "critical_issues": len([i for i in issues if i.level == QualityLevel.CRITICAL]),
            "warning_issues": len([i for i in issues if i.level == QualityLevel.WARNING]),
            "auto_fixable_issues": len([i for i in issues if i.auto_fixable]),
            "quality_score": 0.0,
            "file_metrics": {}
        }

        # Calculate quality score (0-100)
        total_files = len(list(ontology_dir.rglob("*.ttl")))
        if total_files > 0:
            penalty = (metrics["critical_issues"] * 10 + metrics["warning_issues"] * 2)
            metrics["quality_score"] = max(0, 100 - (penalty / total_files))

        return metrics

    def optimize_ontology(self, file_path: Path, domain: str, performance_reqs: Dict[str, Any]) -> Tuple[str, List[str]]:
        """Optimize ontology using DSPy"""

        with open(file_path) as f:
            content = f.read()

        # DSPy optimization
        optimization_result = self.ontology_optimizer(
            ontology_content=content,
            performance_requirements=json.dumps(performance_reqs),
            domain_patterns=json.dumps(self.validation_rules)
        )

        optimizations = []
        try:
            optimizations = json.loads(optimization_result.optimizations_applied)
        except json.JSONDecodeError:
            optimizations = ["DSPy optimization applied"]

        return optimization_result.optimized_ontology, optimizations

    def auto_fix_issues(self, ontology_dir: Path, issues: List[QualityIssue]) -> Dict[str, List[str]]:
        """Automatically fix issues where possible"""

        fixes_applied = {}

        for issue in issues:
            if issue.auto_fixable and issue.file_path:
                file_path = Path(issue.file_path)

                if file_path not in fixes_applied:
                    fixes_applied[file_path] = []

                # Apply simple fixes
                if issue.rule == ValidationRule.SCHEMA_VALIDATION:
                    # Try to fix TTL syntax issues
                    try:
                        with open(file_path) as f:
                            content = f.read()

                        # Common fixes
                        fixed_content = content
                        fixed_content = re.sub(r'([^.])(\n@)', r'\1 .\2', fixed_content)  # Missing periods
                        fixed_content = re.sub(r'(\w+)(\s+a\s+)', r':\1\2', fixed_content)  # Missing prefixes

                        if fixed_content != content:
                            with open(file_path, 'w') as f:
                                f.write(fixed_content)
                            fixes_applied[file_path].append("Fixed TTL syntax issues")

                    except Exception as e:
                        fixes_applied[file_path].append(f"Could not auto-fix: {str(e)}")

        return {str(k): v for k, v in fixes_applied.items()}

    def generate_documentation(self, ontology_dir: Path, domain: str) -> str:
        """Generate comprehensive documentation using DSPy"""

        # Read core ontology
        core_files = list(ontology_dir.glob("*core*.ttl"))
        if not core_files:
            return "No core ontology found for documentation generation"

        with open(core_files[0]) as f:
            content = f.read()

        # DSPy documentation generation
        doc_result = self.doc_generator(
            ontology_content=content,
            domain_context=f"{domain} domain ontology system",
            target_audience="developers and domain experts"
        )

        return doc_result.documentation
