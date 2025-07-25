#!/usr/bin/env python3
"""
DFLSS (Design for Lean Six Sigma) Validation
Ensures Six Sigma quality levels (3.4 DPMO)
"""

import json
import statistics
from datetime import datetime

class DFLSSValidator:
    def __init__(self):
        self.dpmo_target = 3.4  # Defects per million opportunities
        self.sigma_level = 6
        
    def calculate_dpmo(self, defects, opportunities):
        """Calculate defects per million opportunities"""
        if opportunities == 0:
            return float('inf')
        return (defects / opportunities) * 1_000_000
    
    def validate_service_quality(self, service_name, metrics):
        """Validate service meets Six Sigma quality"""
        
        # Quality metrics
        total_requests = metrics.get('total_requests', 0)
        failed_requests = metrics.get('failed_requests', 0)
        response_times = metrics.get('response_times', [])
        
        # Calculate DPMO
        dpmo = self.calculate_dpmo(failed_requests, total_requests)
        
        # Calculate process capability
        if response_times:
            mean_response = statistics.mean(response_times)
            std_response = statistics.stdev(response_times) if len(response_times) > 1 else 0
            
            # Upper specification limit (1ms for 8-tick compliance)
            usl = 1.0
            
            # Process capability index
            if std_response > 0:
                cp = (usl - mean_response) / (3 * std_response)
            else:
                cp = float('inf')
        else:
            cp = 0
        
        # Validate quality gates
        quality_gates = {
            'dpmo': dpmo <= self.dpmo_target,
            'availability': (total_requests - failed_requests) / total_requests >= 0.99999 if total_requests > 0 else False,
            'performance': cp >= 1.33,  # Six Sigma capability
            'reliability': metrics.get('mtbf', 0) >= 8760  # Hours (1 year)
        }
        
        return {
            'service': service_name,
            'dpmo': dpmo,
            'sigma_level': self._dpmo_to_sigma(dpmo),
            'process_capability': cp,
            'quality_gates': quality_gates,
            'passed': all(quality_gates.values())
        }
    
    def _dpmo_to_sigma(self, dpmo):
        """Convert DPMO to Sigma level"""
        sigma_table = [
            (3.4, 6.0),
            (233, 5.0),
            (6210, 4.0),
            (66807, 3.0),
            (308538, 2.0),
            (691462, 1.0)
        ]
        
        for dpmo_limit, sigma in sigma_table:
            if dpmo <= dpmo_limit:
                return sigma
        return 0.0
    
    def generate_validation_report(self, services_data):
        """Generate comprehensive DFLSS validation report"""
        
        results = []
        for service, metrics in services_data.items():
            result = self.validate_service_quality(service, metrics)
            results.append(result)
        
        # Overall portfolio quality
        total_passed = sum(1 for r in results if r['passed'])
        portfolio_quality = (total_passed / len(results)) * 100 if results else 0
        
        report = {
            'timestamp': datetime.utcnow().isoformat(),
            'validation_type': 'DFLSS Six Sigma',
            'services': results,
            'portfolio_summary': {
                'total_services': len(results),
                'passed': total_passed,
                'failed': len(results) - total_passed,
                'quality_percentage': portfolio_quality,
                'six_sigma_achieved': portfolio_quality >= 99.99966
            }
        }
        
        return report

if __name__ == '__main__':
    # Sample metrics for validation
    sample_metrics = {
        'cns_litigator': {
            'total_requests': 1000000,
            'failed_requests': 3,
            'response_times': [0.5, 0.6, 0.7, 0.8, 0.9] * 200,
            'mtbf': 10000
        },
        'cns_quant': {
            'total_requests': 1000000,
            'failed_requests': 2,
            'response_times': [0.4, 0.5, 0.6, 0.7, 0.8] * 200,
            'mtbf': 12000
        },
        'cns_clinician': {
            'total_requests': 1000000,
            'failed_requests': 4,
            'response_times': [0.6, 0.7, 0.8, 0.9, 1.0] * 200,
            'mtbf': 9000
        },
        'cns_fabricator': {
            'total_requests': 1000000,
            'failed_requests': 1,
            'response_times': [0.3, 0.4, 0.5, 0.6, 0.7] * 200,
            'mtbf': 15000
        }
    }
    
    validator = DFLSSValidator()
    report = validator.generate_validation_report(sample_metrics)
    
    print(json.dumps(report, indent=2))
    
    # Save report
    with open('dflss_validation_report.json', 'w') as f:
        json.dump(report, f, indent=2)
    
    print(f"\n✅ DFLSS validation complete. Six Sigma achieved: {report['portfolio_summary']['six_sigma_achieved']}")
