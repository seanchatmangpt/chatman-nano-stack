#!/bin/bash
# CNS Forge Security Validation Script
# Validates that security fixes are properly implemented

echo "🔐 CNS Forge Security Validation"
echo "================================="

# Check for hardcoded secrets
echo "Checking for hardcoded secrets..."
if grep -r "password\|secret\|key" k8s/ --include="*.yaml" | grep -v "REPLACE_WITH" | grep -v "#"; then
    echo "❌ Hardcoded secrets found!"
    exit 1
else
    echo "✅ No hardcoded secrets found"
fi

# Check RBAC permissions
echo "Checking RBAC permissions..."
if kubectl auth can-i create secrets --as=system:serviceaccount:cns-forge:cns-forge-service-account 2>/dev/null; then
    echo "❌ Service account has excessive permissions!"
    exit 1
else
    echo "✅ Service account follows least privilege"
fi

# Check security policies
echo "Checking security policies..."
if kubectl get psp cns-forge-restricted-psp 2>/dev/null; then
    echo "✅ Pod Security Policy exists"
else
    echo "⚠️ Pod Security Policy not found"
fi

# Check network policies
echo "Checking network policies..."
if kubectl get networkpolicy -n cns-forge 2>/dev/null | grep -q "cns-forge"; then
    echo "✅ Network policies configured"
else
    echo "⚠️ Network policies not found"
fi

echo ""
echo "🎉 Security validation complete!"
