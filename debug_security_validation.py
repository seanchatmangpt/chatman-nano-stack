#!/usr/bin/env python3
"""
Debug script to check what security validations are failing
"""

from pathlib import Path

def debug_terraform_security():
    """Debug Terraform security validation"""
    print("🔍 DEBUGGING TERRAFORM SECURITY VALIDATION")
    print("=" * 60)
    
    terraform_files = list(Path("terraform").glob("*.tf"))
    print(f"Terraform files found: {len(terraform_files)}")
    for tf_file in terraform_files:
        print(f"  • {tf_file.name}")
    
    # Check each security requirement
    checks = []
    
    # Check 1: securityContext in deployment.tf
    try:
        deployment_content = Path("terraform/deployment.tf").read_text()
        check1 = "securityContext" in deployment_content
        checks.append(("securityContext in deployment.tf", check1))
        print(f"✅ Check 1 - securityContext in deployment.tf: {check1}")
    except Exception as e:
        checks.append(("securityContext in deployment.tf", False))
        print(f"❌ Check 1 - securityContext in deployment.tf: ERROR - {e}")
    
    # Check 2: NetworkPolicy in main.tf
    try:
        main_content = Path("terraform/main.tf").read_text()
        check2 = "NetworkPolicy" in main_content
        checks.append(("NetworkPolicy in main.tf", check2))
        print(f"✅ Check 2 - NetworkPolicy in main.tf: {check2}")
    except Exception as e:
        checks.append(("NetworkPolicy in main.tf", False))
        print(f"❌ Check 2 - NetworkPolicy in main.tf: ERROR - {e}")
    
    # Check 3: RBAC in main.tf
    try:
        main_content = Path("terraform/main.tf").read_text()
        check3 = "RBAC" in main_content
        checks.append(("RBAC in main.tf", check3))
        print(f"✅ Check 3 - RBAC in main.tf: {check3}")
    except Exception as e:
        checks.append(("RBAC in main.tf", False))
        print(f"❌ Check 3 - RBAC in main.tf: ERROR - {e}")
    
    # Check 4: At least 6 terraform files
    check4 = len(terraform_files) >= 6
    checks.append(("At least 6 terraform files", check4))
    print(f"✅ Check 4 - At least 6 terraform files: {check4} ({len(terraform_files)} found)")
    
    # Check 5: security-hardening.tf exists
    check5 = Path("terraform/security-hardening.tf").exists()
    checks.append(("security-hardening.tf exists", check5))
    print(f"✅ Check 5 - security-hardening.tf exists: {check5}")
    
    # Check 6: secret-management.tf exists
    check6 = Path("terraform/secret-management.tf").exists()
    checks.append(("secret-management.tf exists", check6))
    print(f"✅ Check 6 - secret-management.tf exists: {check6}")
    
    # Check 7: PodSecurityPolicy in security-hardening.tf
    try:
        security_content = Path("terraform/security-hardening.tf").read_text()
        check7 = "PodSecurityPolicy" in security_content
        checks.append(("PodSecurityPolicy in security-hardening.tf", check7))
        print(f"✅ Check 7 - PodSecurityPolicy in security-hardening.tf: {check7}")
    except Exception as e:
        checks.append(("PodSecurityPolicy in security-hardening.tf", False))
        print(f"❌ Check 7 - PodSecurityPolicy in security-hardening.tf: ERROR - {e}")
    
    # Check 8: ClusterRole in secret-management.tf
    try:
        secret_content = Path("terraform/secret-management.tf").read_text()
        check8 = "ClusterRole" in secret_content
        checks.append(("ClusterRole in secret-management.tf", check8))
        print(f"✅ Check 8 - ClusterRole in secret-management.tf: {check8}")
    except Exception as e:
        checks.append(("ClusterRole in secret-management.tf", False))
        print(f"❌ Check 8 - ClusterRole in secret-management.tf: ERROR - {e}")
    
    # Overall result
    all_passed = all(check[1] for check in checks)
    passed_count = sum(1 for check in checks if check[1])
    
    print(f"\n📊 TERRAFORM SECURITY VALIDATION SUMMARY:")
    print(f"   Checks passed: {passed_count}/{len(checks)}")
    print(f"   Overall result: {'✅ PASS' if all_passed else '❌ FAIL'}")
    
    return all_passed

def debug_secret_management():
    """Debug secret management validation"""
    print("\n🔍 DEBUGGING SECRET MANAGEMENT VALIDATION")
    print("=" * 60)
    
    checks = []
    
    # Check 1: Secret in configmap.yaml
    try:
        config_content = Path("kubernetes/configmap.yaml").read_text()
        check1 = "Secret" in config_content
        checks.append(("Secret in configmap.yaml", check1))
        print(f"✅ Check 1 - Secret in configmap.yaml: {check1}")
    except Exception as e:
        checks.append(("Secret in configmap.yaml", False))
        print(f"❌ Check 1 - Secret in configmap.yaml: ERROR - {e}")
    
    # Check 2: No base64 in configmap.yaml
    try:
        config_content = Path("kubernetes/configmap.yaml").read_text()
        check2 = "base64" not in config_content
        checks.append(("No base64 in configmap.yaml", check2))
        print(f"✅ Check 2 - No base64 in configmap.yaml: {check2}")
    except Exception as e:
        checks.append(("No base64 in configmap.yaml", False))
        print(f"❌ Check 2 - No base64 in configmap.yaml: ERROR - {e}")
    
    # Check 3: secret-management.tf exists
    check3 = Path("terraform/secret-management.tf").exists()
    checks.append(("secret-management.tf exists", check3))
    print(f"✅ Check 3 - secret-management.tf exists: {check3}")
    
    # Check 4: SealedSecret in secret-management.tf
    try:
        secret_content = Path("terraform/secret-management.tf").read_text()
        check4 = "SealedSecret" in secret_content
        checks.append(("SealedSecret in secret-management.tf", check4))
        print(f"✅ Check 4 - SealedSecret in secret-management.tf: {check4}")
    except Exception as e:
        checks.append(("SealedSecret in secret-management.tf", False))
        print(f"❌ Check 4 - SealedSecret in secret-management.tf: ERROR - {e}")
    
    # Check 5: secret_rotation in secret-management.tf
    try:
        secret_content = Path("terraform/secret-management.tf").read_text()
        check5 = "secret_rotation" in secret_content
        checks.append(("secret_rotation in secret-management.tf", check5))
        print(f"✅ Check 5 - secret_rotation in secret-management.tf: {check5}")
    except Exception as e:
        checks.append(("secret_rotation in secret-management.tf", False))
        print(f"❌ Check 5 - secret_rotation in secret-management.tf: ERROR - {e}")
    
    # Check 6: sealed_secrets_controller in secret-management.tf
    try:
        secret_content = Path("terraform/secret-management.tf").read_text()
        check6 = "sealed_secrets_controller" in secret_content
        checks.append(("sealed_secrets_controller in secret-management.tf", check6))
        print(f"✅ Check 6 - sealed_secrets_controller in secret-management.tf: {check6}")
    except Exception as e:
        checks.append(("sealed_secrets_controller in secret-management.tf", False))
        print(f"❌ Check 6 - sealed_secrets_controller in secret-management.tf: ERROR - {e}")
    
    # Overall result
    all_passed = all(check[1] for check in checks)
    passed_count = sum(1 for check in checks if check[1])
    
    print(f"\n📊 SECRET MANAGEMENT VALIDATION SUMMARY:")
    print(f"   Checks passed: {passed_count}/{len(checks)}")
    print(f"   Overall result: {'✅ PASS' if all_passed else '❌ FAIL'}")
    
    return all_passed

if __name__ == "__main__":
    terraform_result = debug_terraform_security()
    secret_result = debug_secret_management()
    
    print(f"\n🎯 OVERALL INFRASTRUCTURE SECURITY DEBUG:")
    print(f"   Terraform Security: {'✅ PASS' if terraform_result else '❌ FAIL'}")
    print(f"   Secret Management: {'✅ PASS' if secret_result else '❌ FAIL'}")
    
    if terraform_result and secret_result:
        print(f"\n✅ ALL INFRASTRUCTURE SECURITY CHECKS WOULD PASS")
    else:
        print(f"\n❌ INFRASTRUCTURE SECURITY ISSUES NEED RESOLUTION")