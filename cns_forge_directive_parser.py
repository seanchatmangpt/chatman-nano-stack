#!/usr/bin/env python3
"""
CNS Forge Directive Parser
Converts outcome-based natural language directives into formal TTL specifications
"The specification is the system" - transforms intent into reality
"""

import re
import json
import html
import string
from typing import Dict, List, Any, Tuple, Optional
from dataclasses import dataclass
from datetime import datetime, timedelta
from pathlib import Path
import typer
from rdflib import Graph, URIRef, Literal, Namespace, RDF, RDFS
from rich.console import Console
from rich.table import Table
from rich.panel import Panel

# Input validation exceptions
class DirectiveValidationError(Exception):
    """Raised when directive input validation fails"""
    pass

class MaliciousInputError(DirectiveValidationError):
    """Raised when potentially malicious input is detected"""
    pass

app = typer.Typer()
console = Console()

# CNS Ontology Namespaces
CNS = Namespace("http://cns.io/forge#")
AEGIS = Namespace("http://cns.io/aegis/fabric#")
BITACTOR = Namespace("http://cns.io/bitactor#")
SLA = Namespace("http://cns.io/sla#")
INFRA = Namespace("http://cns.io/infrastructure#")

@dataclass
class ParsedDirective:
    """Structured representation of a parsed directive"""
    directive_id: str
    original_text: str
    intent_category: str
    target_metrics: Dict[str, Any]
    constraints: List[str]
    temporal_requirements: Dict[str, Any]
    success_criteria: List[str]
    generated_ttl: str
    confidence_score: float

class CNSForgeDirectiveParser:
    """
    Transforms high-level outcome-based directives into formal TTL specifications
    Examples:
    - "achieve five-nines availability" -> Availability SLA with 99.999% uptime
    - "maintain market latency below 50ms" -> Latency constraint with BitActor optimization
    - "guarantee data sovereignty" -> Geographic and regulatory compliance rules
    """
    
    def __init__(self):
        self.intent_patterns = self._load_intent_patterns()
        self.metric_extractors = self._build_metric_extractors()
        self.ttl_templates = self._load_ttl_templates()
        
    def _load_intent_patterns(self) -> Dict[str, List[str]]:
        """Load enhanced intent recognition patterns with weighted scoring"""
        return {
            'availability': [
                # High confidence patterns (weight: 3)
                (r'achieve.*(?:five[\-\s]nines?|99\.999%|5[\-\s]9s)', 3),
                (r'(?:maintain|ensure|guarantee).*(?:uptime|availability).*(?:above|over|>=?)\s*(\d+\.?\d*)%', 3),
                # Medium confidence patterns (weight: 2)
                (r'ensure.*(?:high|maximum|peak)\s*availability', 2),
                (r'zero.*downtime', 2),
                (r'always.*(?:available|online|accessible)', 2),
                (r'(?:high|maximum)\s*availability', 2),
                # Low confidence patterns (weight: 1)
                (r'uptime.*requirement', 1),
                (r'service.*level.*agreement', 1),
                (r'reliability.*target', 1)
            ],
            'latency': [
                # High confidence patterns (weight: 3)
                (r'maintain.*latency.*(?:below|under|<=?)\s*(\d+)\s*(ms|μs|ns)', 3),
                (r'(?:market|network|response)\s*latency.*(?:below|under|<=?)\s*(\d+)\s*(ms|μs|ns)', 3),
                (r'sub[\-\s](?:millisecond|microsecond)', 3),
                # Medium confidence patterns (weight: 2)
                (r'real[\-\s]time.*response', 2),
                (r'ultra[\-\s]low[\-\s]latency', 2),
                (r'low.*latency.*requirement', 2),
                (r'response.*time.*target', 2),
                # Low confidence patterns (weight: 1)
                (r'fast.*response', 1),
                (r'quick.*processing', 1),
                (r'performance.*target', 1)
            ],
            'throughput': [
                # High confidence patterns (weight: 3)
                (r'handle.*(\d+[kmb]?)\s*(?:ops|operations|requests|transactions).*(?:per\s*second|/s)', 3),
                (r'process.*(\d+[kmb]?)\s*(?:messages|events).*(?:per\s*second|/s)', 3),
                (r'throughput.*(?:above|over|>=?)\s*(\d+[kmb]?)', 3),
                # Medium confidence patterns (weight: 2)
                (r'scale.*to.*(\d+[kmb]?)\s*(?:users|connections)', 2),
                (r'(?:high|maximum)\s*throughput', 2),
                (r'concurrent.*users.*(\d+[kmb]?)', 2),
                # Low confidence patterns (weight: 1)
                (r'load.*capacity', 1),
                (r'scaling.*requirement', 1),
                (r'traffic.*volume', 1)
            ],
            'security': [
                # High confidence patterns (weight: 3)
                (r'guarantee.*data\s*sovereignty', 3),
                (r'comply.*with.*(?:gdpr|hipaa|sox|pci)', 3),
                (r'zero[\-\s]trust.*architecture', 3),
                (r'end[\-\s]to[\-\s]end.*encryption', 3),
                # Medium confidence patterns (weight: 2)
                (r'secure.*by.*design', 2),
                (r'encryption.*(?:required|mandatory)', 2),
                (r'security.*compliance', 2),
                (r'access.*control', 2),
                # Low confidence patterns (weight: 1)
                (r'security.*requirement', 1),
                (r'data.*protection', 1),
                (r'privacy.*concern', 1)
            ],
            'reliability': [
                # High confidence patterns (weight: 3)
                (r'fault[\-\s]tolerant', 3),
                (r'self[\-\s]healing', 3),
                (r'automatic.*recovery', 3),
                # Medium confidence patterns (weight: 2)
                (r'resilient.*to.*failures', 2),
                (r'chaos.*engineering', 2),
                (r'disaster.*recovery', 2),
                (r'backup.*strategy', 2),
                # Low confidence patterns (weight: 1)
                (r'robust.*system', 1),
                (r'failover.*mechanism', 1),
                (r'redundancy.*requirement', 1)
            ],
            'cost': [
                # High confidence patterns (weight: 3)
                (r'reduce.*(?:cost|spend).*by.*(\d+)%', 3),
                (r'budget.*constraint.*(\d+)', 3),
                (r'cost.*optimization', 3),
                # Medium confidence patterns (weight: 2)
                (r'minimize.*operational.*cost', 2),
                (r'cost[\-\s]effective', 2),
                (r'resource.*efficiency', 2),
                # Low confidence patterns (weight: 1)
                (r'economic.*requirement', 1),
                (r'financial.*constraint', 1),
                (r'price.*consideration', 1)
            ]
        }
    
    def _build_metric_extractors(self) -> Dict[str, callable]:
        """Build metric extraction functions"""
        return {
            'percentage': lambda text: self._extract_percentage(text),
            'time_duration': lambda text: self._extract_time_duration(text),
            'numeric_value': lambda text: self._extract_numeric_value(text),
            'currency': lambda text: self._extract_currency(text),
            'compliance_standards': lambda text: self._extract_compliance_standards(text)
        }
    
    def _load_ttl_templates(self) -> Dict[str, str]:
        """Load TTL generation templates"""
        return {
            'availability_sla': '''
@prefix cns: <http://cns.io/forge#> .
@prefix sla: <http://cns.io/sla#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

cns:{directive_id} a sla:AvailabilityRequirement ;
    sla:targetUptime "{uptime_percentage}"^^xsd:decimal ;
    sla:maxDowntimePerMonth "{max_downtime_minutes}"^^xsd:integer ;
    sla:measurementWindow "monthly" ;
    sla:priority "critical" ;
    cns:generatedFrom "{original_directive}" ;
    cns:requiresComponents (
        cns:LoadBalancer
        cns:HealthCheck
        cns:FailoverMechanism
        cns:MonitoringSystem
    ) .
''',
            'latency_constraint': '''
@prefix cns: <http://cns.io/forge#> .
@prefix bitactor: <http://cns.io/bitactor#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

cns:{directive_id} a bitactor:LatencyConstraint ;
    bitactor:maxLatencyNs "{max_latency_ns}"^^xsd:long ;
    bitactor:percentile "99.9" ;
    bitactor:measurementInterval "1s" ;
    cns:generatedFrom "{original_directive}" ;
    cns:requiresOptimization bitactor:AOTCompilation ;
    cns:requiresComponents (
        bitactor:Engine
        cns:PerformanceMonitor
        cns:LoadBalancer
    ) .
''',
            'throughput_requirement': '''
@prefix cns: <http://cns.io/forge#> .
@prefix bitactor: <http://cns.io/bitactor#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

cns:{directive_id} a bitactor:ThroughputRequirement ;
    bitactor:minOpsPerSecond "{min_throughput}"^^xsd:long ;
    bitactor:scalingTrigger "{scaling_threshold}"^^xsd:decimal ;
    cns:generatedFrom "{original_directive}" ;
    cns:requiresComponents (
        bitactor:Engine
        cns:AutoScaler
        cns:LoadDistributor
    ) .
''',
            'security_policy': '''
@prefix cns: <http://cns.io/forge#> .
@prefix aegis: <http://cns.io/aegis/fabric#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

cns:{directive_id} a aegis:SecurityPolicy ;
    aegis:encryptionStandard "AES256" ;
    aegis:complianceStandards "{compliance_list}" ;
    aegis:zeroTrustEnabled "true"^^xsd:boolean ;
    cns:generatedFrom "{original_directive}" ;
    cns:requiresComponents (
        aegis:Fabric
        cns:EncryptionModule
        cns:ComplianceMonitor
    ) .
'''
        }
    
    def _validate_and_sanitize_input(self, directive_text: str) -> str:
        """Comprehensive input validation and sanitization"""
        
        # Basic validation
        if not directive_text or not isinstance(directive_text, str):
            raise DirectiveValidationError("Directive text must be a non-empty string")
        
        # Length validation (prevent DoS attacks)
        if len(directive_text) > 10000:
            raise DirectiveValidationError("Directive text too long (max 10,000 characters)")
        
        if len(directive_text.strip()) < 3:
            raise DirectiveValidationError("Directive text too short (min 3 characters)")
        
        # Character validation - only allow safe characters
        allowed_chars = set(string.ascii_letters + string.digits + string.punctuation + ' \t\n\r')
        invalid_chars = set(directive_text) - allowed_chars
        if invalid_chars:
            raise MaliciousInputError(f"Invalid characters detected: {invalid_chars}")
        
        # Script injection detection
        script_patterns = [
            r'<script[^>]*>.*?</script>',
            r'javascript:',
            r'data:text/html',
            r'eval\s*\(',
            r'document\.',
            r'window\.',
            r'alert\s*\(',
            r'confirm\s*\(',
            r'prompt\s*\(',
            r'location\.',
            r'setTimeout\s*\(',
            r'setInterval\s*\(',
        ]
        
        text_lower = directive_text.lower()
        for pattern in script_patterns:
            if re.search(pattern, text_lower, re.IGNORECASE | re.DOTALL):
                raise MaliciousInputError(f"Potential script injection detected: {pattern}")
        
        # SQL injection detection
        sql_patterns = [
            r"('|(\\')|(;|\\;)|(--|\\-\\-)|(\/\*|\*\/))",
            r"(union\s+select)",
            r"(insert\s+into)",
            r"(delete\s+from)",
            r"(update\s+.+\s+set)",
            r"(drop\s+(table|database))",
            r"(create\s+(table|database))",
            r"(alter\s+table)",
            r"(truncate\s+table)"
        ]
        
        for pattern in sql_patterns:
            if re.search(pattern, text_lower, re.IGNORECASE):
                raise MaliciousInputError(f"Potential SQL injection detected: {pattern}")
        
        # Command injection detection
        command_patterns = [
            r'[\|;&`\$\(\)]',
            r'rm\s+-',
            r'chmod\s+',
            r'chown\s+',
            r'sudo\s+',
            r'su\s+',
            r'passwd\s+',
            r'cat\s+\/etc\/',
            r'nc\s+-',
            r'netcat\s+',
            r'wget\s+',
            r'curl\s+.*http',
            r'powershell',
            r'cmd\.exe',
            r'\/bin\/',
            r'\.\.\/\.\.\/',
        ]
        
        for pattern in command_patterns:
            if re.search(pattern, directive_text, re.IGNORECASE):
                raise MaliciousInputError(f"Potential command injection detected: {pattern}")
        
        # Path traversal detection
        if '../' in directive_text or '..\/' in directive_text:
            raise MaliciousInputError("Path traversal attempt detected")
        
        # Normalize whitespace and remove control characters
        sanitized_text = re.sub(r'\s+', ' ', directive_text.strip())
        sanitized_text = ''.join(char for char in sanitized_text if ord(char) >= 32 or char in '\t\n\r')
        
        # HTML escape for safety (preserve original meaning but prevent XSS)
        sanitized_text = html.escape(sanitized_text, quote=False)
        
        # Additional business logic validation
        if not any(char.isalpha() for char in sanitized_text):
            raise DirectiveValidationError("Directive must contain at least one letter")
        
        # Check for meaningful content (not just punctuation/numbers)
        meaningful_chars = sum(1 for char in sanitized_text if char.isalpha())
        if meaningful_chars < 3:
            raise DirectiveValidationError("Directive must contain meaningful text")
        
        return sanitized_text
    
    def parse_directive(self, directive_text: str) -> ParsedDirective:
        """Parse a natural language directive into structured specification"""
        # Validate and sanitize input first
        validated_text = self._validate_and_sanitize_input(directive_text)
        console.print(f"[bold blue]🎯 Parsing directive: {validated_text}[/bold blue]")
        
        # Generate unique ID
        directive_id = f"directive_{int(datetime.now().timestamp())}"
        
        # Identify intent category
        intent_category = self._classify_intent(validated_text)
        console.print(f"[cyan]📋 Intent category: {intent_category}[/cyan]")
        
        # Extract target metrics
        target_metrics = self._extract_metrics(validated_text, intent_category)
        console.print(f"[green]📊 Target metrics: {target_metrics}[/green]")
        
        # Extract constraints
        constraints = self._extract_constraints(validated_text)
        
        # Extract temporal requirements
        temporal_requirements = self._extract_temporal_requirements(validated_text)
        
        # Generate success criteria
        success_criteria = self._generate_success_criteria(intent_category, target_metrics)
        
        # Generate TTL specification
        generated_ttl = self._generate_ttl(
            directive_id, validated_text, intent_category, 
            target_metrics, constraints
        )
        
        # Calculate confidence score
        confidence_score = self._calculate_confidence(
            validated_text, intent_category, target_metrics
        )
        
        parsed_directive = ParsedDirective(
            directive_id=directive_id,
            original_text=validated_text,
            intent_category=intent_category,
            target_metrics=target_metrics,
            constraints=constraints,
            temporal_requirements=temporal_requirements,
            success_criteria=success_criteria,
            generated_ttl=generated_ttl,
            confidence_score=confidence_score
        )
        
        # Save parsed directive
        self._save_directive(parsed_directive)
        
        return parsed_directive
    
    def _classify_intent(self, text: str) -> str:
        """Classify the intent category using weighted pattern matching"""
        text_lower = text.lower()
        
        # Score each category using weighted patterns
        category_scores = {}
        for category, patterns in self.intent_patterns.items():
            score = 0
            pattern_matches = 0
            
            for pattern, weight in patterns:
                if re.search(pattern, text_lower):
                    score += weight
                    pattern_matches += 1
            
            # Bonus for multiple pattern matches
            if pattern_matches > 1:
                score += pattern_matches * 0.5
            
            category_scores[category] = score
        
        # Return highest scoring category
        if category_scores and max(category_scores.values()) > 0:
            return max(category_scores.items(), key=lambda x: x[1])[0]
        return 'general'
    
    def _extract_metrics(self, text: str, category: str) -> Dict[str, Any]:
        """Extract quantitative metrics from the directive"""
        metrics = {}
        
        if category == 'availability':
            # Extract uptime percentage
            uptime_match = re.search(r'(?:five[\-\s]nines?|5[\-\s]9s)', text.lower())
            if uptime_match:
                metrics['uptime_percentage'] = 99.999
            else:
                percentage_match = re.search(r'(\d+\.?\d*)%', text)
                if percentage_match:
                    metrics['uptime_percentage'] = float(percentage_match.group(1))
        
        elif category == 'latency':
            # Extract latency value and unit
            latency_match = re.search(r'(\d+)\s*(ms|μs|ns|microseconds?|nanoseconds?)', text.lower())
            if latency_match:
                value = int(latency_match.group(1))
                unit = latency_match.group(2)
                
                # Convert to nanoseconds
                if unit in ['ms', 'millisecond', 'milliseconds']:
                    metrics['max_latency_ns'] = value * 1_000_000
                elif unit in ['μs', 'us', 'microsecond', 'microseconds']:
                    metrics['max_latency_ns'] = value * 1_000
                else:  # nanoseconds
                    metrics['max_latency_ns'] = value
        
        elif category == 'throughput':
            # Extract throughput value
            throughput_match = re.search(r'(\d+(?:\.\d+)?)\s*([kmb]?)\s*(?:ops|operations|requests)', text.lower())
            if throughput_match:
                value = float(throughput_match.group(1))
                multiplier = throughput_match.group(2)
                
                # Apply multiplier
                if multiplier == 'k':
                    value *= 1_000
                elif multiplier == 'm':
                    value *= 1_000_000
                elif multiplier == 'b':
                    value *= 1_000_000_000
                
                metrics['min_throughput'] = int(value)
        
        elif category == 'security':
            # Extract compliance standards
            compliance = self._extract_compliance_standards(text)
            if compliance:
                metrics['compliance_standards'] = compliance
        
        elif category == 'cost':
            # Extract cost reduction percentage
            cost_match = re.search(r'reduce.*(?:cost|spend).*by.*(\d+)%', text.lower())
            if cost_match:
                metrics['cost_reduction_percentage'] = float(cost_match.group(1))
            
            # Extract budget constraints
            budget_match = re.search(r'budget.*(\$\d+(?:,\d+)*(?:\.\d+)?[kmb]?)', text.lower())
            if budget_match:
                budget_str = budget_match.group(1).replace('$', '').replace(',', '')
                metrics['budget_limit'] = budget_str
        
        return metrics
    
    def _extract_constraints(self, text: str) -> List[str]:
        """Extract operational constraints"""
        constraints = []
        
        # Budget constraints
        budget_match = re.search(r'budget.*(\$\d+(?:,\d+)*(?:\.\d+)?[kmb]?)', text.lower())
        if budget_match:
            constraints.append(f"budget_limit: {budget_match.group(1)}")
        
        # Geographic constraints
        geo_match = re.search(r'(?:within|in)\s+([a-z\s]+)(?:region|zone|country)', text.lower())
        if geo_match:
            constraints.append(f"geographic_constraint: {geo_match.group(1).strip()}")
        
        # Compliance constraints
        compliance = re.findall(r'(gdpr|hipaa|sox|pci|iso\s*27001)', text.lower())
        for standard in compliance:
            constraints.append(f"compliance_required: {standard.upper()}")
        
        return constraints
    
    def _extract_temporal_requirements(self, text: str) -> Dict[str, Any]:
        """Extract time-based requirements"""
        temporal = {}
        
        # Implementation deadline
        deadline_match = re.search(r'(?:by|within|before)\s+(\d+)\s+(days?|weeks?|months?)', text.lower())
        if deadline_match:
            value = int(deadline_match.group(1))
            unit = deadline_match.group(2)
            
            if 'day' in unit:
                deadline = datetime.now() + timedelta(days=value)
            elif 'week' in unit:
                deadline = datetime.now() + timedelta(weeks=value)
            else:  # months
                deadline = datetime.now() + timedelta(days=value * 30)
            
            temporal['implementation_deadline'] = deadline.isoformat()
        
        # Measurement window
        window_match = re.search(r'(?:per|every)\s+(minute|hour|day|week|month)', text.lower())
        if window_match:
            temporal['measurement_window'] = window_match.group(1)
        
        return temporal
    
    def _generate_success_criteria(self, category: str, metrics: Dict[str, Any]) -> List[str]:
        """Generate measurable success criteria"""
        criteria = []
        
        if category == 'availability':
            uptime = metrics.get('uptime_percentage', 99.9)
            criteria.append(f"System uptime >= {uptime}%")
            criteria.append("Zero unplanned outages > 5 minutes")
            criteria.append("Recovery time < 2 minutes")
        
        elif category == 'latency':
            max_latency = metrics.get('max_latency_ns', 100000)
            criteria.append(f"99.9th percentile latency <= {max_latency}ns")
            criteria.append("Zero requests timeout")
            criteria.append("Latency variance < 10%")
        
        elif category == 'throughput':
            min_throughput = metrics.get('min_throughput', 1000)
            criteria.append(f"Sustained throughput >= {min_throughput} ops/s")
            criteria.append("Auto-scaling triggers work correctly")
            criteria.append("No dropped requests under load")
        
        elif category == 'security':
            criteria.append("All data encrypted at rest and in transit")
            criteria.append("Zero security vulnerabilities (CVSS > 7.0)")
            criteria.append("Compliance audit passes")
        
        return criteria
    
    def _generate_ttl(self, directive_id: str, original_text: str, 
                     category: str, metrics: Dict[str, Any], 
                     constraints: List[str]) -> str:
        """Generate formal TTL specification"""
        
        template_key = f"{category}_{'sla' if category == 'availability' else 'constraint' if category == 'latency' else 'requirement' if category == 'throughput' else 'policy'}"
        template = self.ttl_templates.get(template_key, self.ttl_templates.get(f"{category}_requirement", ""))
        
        if not template:
            # Generic template
            template = f'''
@prefix cns: <http://cns.io/forge#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

cns:{directive_id} a cns:Requirement ;
    cns:category "{category}" ;
    cns:generatedFrom "{original_text}" ;
    cns:metrics "{json.dumps(metrics)}" ;
    cns:constraints "{json.dumps(constraints)}" .
'''
        
        # Fill template with extracted values
        try:
            return template.format(
                directive_id=directive_id,
                original_directive=original_text,
                uptime_percentage=metrics.get('uptime_percentage', 99.9),
                max_downtime_minutes=int((100 - metrics.get('uptime_percentage', 99.9)) * 43200 / 100),
                max_latency_ns=metrics.get('max_latency_ns', 100000),
                min_throughput=metrics.get('min_throughput', 1000),
                scaling_threshold=0.8,
                compliance_list=','.join(metrics.get('compliance_standards', []))
            )
        except KeyError as e:
            # Fallback to generic template
            return f'''
@prefix cns: <http://cns.io/forge#> .

cns:{directive_id} a cns:Requirement ;
    cns:category "{category}" ;
    cns:generatedFrom "{original_text}" ;
    cns:targetMetrics "{json.dumps(metrics)}" .
'''
    
    def _calculate_confidence(self, text: str, category: str, metrics: Dict[str, Any]) -> float:
        """Calculate enhanced confidence score for the parsing"""
        text_lower = text.lower()
        score = 0.0
        
        # Enhanced category confidence scoring (40% weight)
        if category != 'general':
            category_patterns = self.intent_patterns.get(category, [])
            pattern_score = 0
            max_possible_score = 0
            
            for pattern, weight in category_patterns:
                max_possible_score += weight
                if re.search(pattern, text_lower):
                    pattern_score += weight
            
            if max_possible_score > 0:
                category_confidence = pattern_score / max_possible_score
                score += 0.4 * category_confidence
        
        # Metrics extraction confidence (30% weight)
        if metrics:
            # More metrics = higher confidence
            metrics_confidence = min(1.0, len(metrics) / 2)
            
            # Bonus for specific numeric values
            numeric_bonus = 0
            for value in metrics.values():
                if isinstance(value, (int, float)) and value > 0:
                    numeric_bonus += 0.1
            
            score += 0.3 * (metrics_confidence + min(0.5, numeric_bonus))
        
        # Text specificity and clarity (20% weight)
        specificity_indicators = [
            'specific', 'exactly', 'precisely', 'target', 'requirement',
            'measurable', 'quantifiable', 'metric', 'benchmark'
        ]
        action_indicators = [
            'achieve', 'maintain', 'ensure', 'guarantee', 'implement',
            'deliver', 'provide', 'support', 'enable'
        ]
        
        specificity_score = sum(1 for indicator in specificity_indicators if indicator in text_lower)
        action_score = sum(1 for indicator in action_indicators if indicator in text_lower)
        
        clarity_confidence = min(1.0, (specificity_score + action_score) / 8)
        score += 0.2 * clarity_confidence
        
        # Completeness bonus (10% weight)
        completeness_factors = [
            bool(re.search(r'\d+', text)),  # Contains numbers
            len(text.split()) >= 4,  # Reasonable length
            '.' in text or '!' in text,  # Proper punctuation
            category in ['availability', 'latency', 'throughput', 'security']  # Core categories
        ]
        
        completeness_score = sum(completeness_factors) / len(completeness_factors)
        score += 0.1 * completeness_score
        
        return min(1.0, score)
    
    def _extract_percentage(self, text: str) -> Optional[float]:
        """Extract percentage values"""
        match = re.search(r'(\d+\.?\d*)%', text)
        return float(match.group(1)) if match else None
    
    def _extract_time_duration(self, text: str) -> Optional[Dict[str, Any]]:
        """Extract time duration values"""
        match = re.search(r'(\d+)\s*(ms|μs|ns|seconds?|minutes?|hours?)', text.lower())
        if match:
            return {'value': int(match.group(1)), 'unit': match.group(2)}
        return None
    
    def _extract_numeric_value(self, text: str) -> Optional[float]:
        """Extract numeric values"""
        match = re.search(r'(\d+(?:\.\d+)?)', text)
        return float(match.group(1)) if match else None
    
    def _extract_currency(self, text: str) -> Optional[str]:
        """Extract currency amounts"""
        match = re.search(r'\$(\d+(?:,\d+)*(?:\.\d+)?[kmb]?)', text.lower())
        return match.group(1) if match else None
    
    def _extract_compliance_standards(self, text: str) -> List[str]:
        """Extract compliance standards"""
        standards = re.findall(r'(gdpr|hipaa|sox|pci|iso\s*27001)', text.lower())
        return [s.upper().replace(' ', '') for s in standards]
    
    def _save_directive(self, directive: ParsedDirective):
        """Save parsed directive for future reference"""
        directive_file = Path(f"cns_forge_directives/{directive.directive_id}.json")
        directive_file.parent.mkdir(exist_ok=True)
        
        with open(directive_file, 'w') as f:
            json.dump({
                'directive_id': directive.directive_id,
                'original_text': directive.original_text,
                'intent_category': directive.intent_category,
                'target_metrics': directive.target_metrics,
                'constraints': directive.constraints,
                'temporal_requirements': directive.temporal_requirements,
                'success_criteria': directive.success_criteria,
                'confidence_score': directive.confidence_score,
                'generated_ttl': directive.generated_ttl,
                'created_at': datetime.now().isoformat()
            }, f, indent=2)
        
        # Also save TTL file
        ttl_file = Path(f"cns_forge_specifications/{directive.directive_id}.ttl")
        ttl_file.parent.mkdir(exist_ok=True)
        
        with open(ttl_file, 'w') as f:
            f.write(directive.generated_ttl)

@app.command()
def parse(directive: str):
    """Parse a natural language directive into TTL specification"""
    parser = CNSForgeDirectiveParser()
    result = parser.parse_directive(directive)
    
    # Display results
    table = Table(title="Parsed Directive Results")
    table.add_column("Property", style="cyan")
    table.add_column("Value", style="green")
    
    table.add_row("Directive ID", result.directive_id)
    table.add_row("Intent Category", result.intent_category)
    table.add_row("Target Metrics", str(result.target_metrics))
    table.add_row("Constraints", str(len(result.constraints)))
    table.add_row("Success Criteria", str(len(result.success_criteria)))
    table.add_row("Confidence Score", f"{result.confidence_score:.2f}")
    
    console.print(table)
    console.print(Panel(result.generated_ttl, title="Generated TTL Specification", border_style="green"))

@app.command()
def test_examples():
    """Test parsing with example directives"""
    parser = CNSForgeDirectiveParser()
    
    examples = [
        "achieve five-nines availability",
        "maintain market latency below 50ms", 
        "guarantee data sovereignty with GDPR compliance",
        "handle 1M operations per second with auto-scaling",
        "reduce operational costs by 30% within 6 months"
    ]
    
    for example in examples:
        console.print(f"\\n[bold yellow]Testing: {example}[/bold yellow]")
        result = parser.parse_directive(example)
        console.print(f"[green]✓ Category: {result.intent_category}, Confidence: {result.confidence_score:.2f}[/green]")

if __name__ == "__main__":
    app()