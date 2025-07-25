#!/usr/bin/env python3
"""
Security Utilities for CNS (Chatman Nano Stack)
Provides common security functions for input validation, path sanitization, and protection against attacks
"""

import os
import re
from pathlib import Path
from typing import Union, List, Optional


class SecurityError(Exception):
    """Raised when a security violation is detected"""
    pass


# Maximum file size: 100MB
MAX_FILE_SIZE = 100 * 1024 * 1024

# Maximum string length: 1MB  
MAX_STRING_LENGTH = 1024 * 1024

# Maximum input items for collections
MAX_COLLECTION_SIZE = 10000

# Forbidden patterns in various contexts
FORBIDDEN_CODE_PATTERNS = [
    'system(', 'exec(', '__import__', 'subprocess',
    'eval(', 'shell=True', '/bin/', 'rm -rf',
    'chmod', 'setuid', 'execve(', '__asm__',
    '$(', '`', '\\x', 'popen(', 'fork(',
    '../', '..\\', '\\\\', '//', 'file://'
]


def secure_file_path(user_path: Union[str, Path], allowed_dirs: Optional[List[Path]] = None) -> Path:
    """
    Validate and canonicalize file paths to prevent path traversal attacks
    
    Args:
        user_path: The user-provided path to validate
        allowed_dirs: List of allowed base directories. If None, uses safe defaults
        
    Returns:
        Canonicalized safe Path object
        
    Raises:
        SecurityError: If path traversal or other security issue detected
    """
    if not user_path:
        raise SecurityError("Empty path provided")
        
    # Convert to Path object
    path = Path(user_path)
    
    # Resolve to absolute canonical path (resolves symlinks and ..)
    try:
        canonical_path = path.resolve(strict=False)
    except Exception as e:
        raise SecurityError(f"Invalid path: {e}")
    
    # Define allowed directories if not provided
    if allowed_dirs is None:
        allowed_dirs = [
            Path.cwd(),  # Current working directory
            Path.home() / '.cache',  # User cache directory
            Path('/tmp'),  # Temporary directory
        ]
        
    # Ensure the path is within allowed directories
    is_allowed = False
    for allowed_dir in allowed_dirs:
        try:
            # Check if canonical_path is relative to allowed_dir
            canonical_path.relative_to(allowed_dir.resolve())
            is_allowed = True
            break
        except ValueError:
            # Not relative to this allowed_dir, continue checking
            continue
            
    if not is_allowed:
        raise SecurityError(f"Path traversal attempt detected: {user_path}")
        
    # Additional checks for suspicious patterns
    path_str = str(canonical_path)
    suspicious_patterns = ['..', '~', '$', '%', '\\x', '\\u', '\x00']
    
    for pattern in suspicious_patterns:
        if pattern in path_str:
            raise SecurityError(f"Suspicious pattern '{pattern}' in path: {user_path}")
            
    return canonical_path


def validate_input_size(data: Union[str, bytes, list, dict], max_size: Optional[int] = None) -> None:
    """
    Validate input size to prevent memory exhaustion attacks
    
    Args:
        data: Input data to validate
        max_size: Maximum allowed size. Uses defaults based on data type if None
        
    Raises:
        SecurityError: If input exceeds maximum size
    """
    if isinstance(data, (str, bytes)):
        size = len(data)
        limit = max_size or MAX_STRING_LENGTH
        data_type = "string/bytes"
    elif isinstance(data, (list, dict)):
        size = len(data)
        limit = max_size or MAX_COLLECTION_SIZE
        data_type = "collection"
    else:
        # For other types, convert to string and check
        size = len(str(data))
        limit = max_size or MAX_STRING_LENGTH
        data_type = "data"
        
    if size > limit:
        raise SecurityError(f"Input {data_type} size {size} exceeds maximum {limit}")


def sanitize_code_input(code_input: str, context: str = "general") -> str:
    """
    Sanitize code input to prevent injection attacks
    
    Args:
        code_input: The code string to sanitize
        context: Context for sanitization (e.g., "c_code", "python", "shell")
        
    Returns:
        Sanitized code string
        
    Raises:
        SecurityError: If malicious patterns detected
    """
    if not isinstance(code_input, str):
        code_input = str(code_input)
        
    # Check for forbidden patterns
    for pattern in FORBIDDEN_CODE_PATTERNS:
        if pattern in code_input:
            raise SecurityError(f"Forbidden pattern '{pattern}' detected in {context} code")
            
    # Context-specific sanitization
    if context == "c_code":
        # Remove potentially dangerous C constructs
        sanitized = code_input.replace(';', '_').replace('#include', '')
        sanitized = re.sub(r'\/\*.*?\*\/', '', sanitized)  # Remove C comments
        
    elif context == "shell":
        # Shell command sanitization
        sanitized = re.sub(r'[;&|`$<>]', '', code_input)
        
    elif context == "python":
        # Python code sanitization  
        sanitized = code_input.replace('__', '').replace('import', '')
        
    else:
        # General sanitization
        sanitized = code_input
        
    # Escape quotes
    sanitized = sanitized.replace('"', '\\"').replace("'", "\\'")
    
    # Remove null bytes
    sanitized = sanitized.replace('\x00', '')
    
    return sanitized


def validate_ttl_input(ttl_content: str) -> None:
    """
    Validate TTL/RDF content for security issues
    
    Args:
        ttl_content: TTL file content to validate
        
    Raises:
        SecurityError: If security issues detected
    """
    # Check size first
    validate_input_size(ttl_content)
    
    # Check for script injection in TTL
    script_patterns = [
        r'<script[^>]*>.*?</script>',
        r'javascript:',
        r'onerror\s*=',
        r'onclick\s*=',
    ]
    
    for pattern in script_patterns:
        if re.search(pattern, ttl_content, re.IGNORECASE):
            raise SecurityError(f"Script injection pattern detected in TTL")
            
    # Check for excessive entity declarations (XML bomb)
    entity_count = ttl_content.count('<!ENTITY')
    if entity_count > 100:
        raise SecurityError(f"Excessive entity declarations ({entity_count}) - possible XML bomb")


def create_safe_temp_file(prefix: str = "cns_", suffix: str = "") -> Path:
    """
    Create a secure temporary file with proper permissions
    
    Args:
        prefix: Prefix for temp file name
        suffix: Suffix for temp file name
        
    Returns:
        Path to secure temporary file
    """
    import tempfile
    
    # Sanitize prefix and suffix
    safe_prefix = re.sub(r'[^a-zA-Z0-9_-]', '', prefix)[:20]
    safe_suffix = re.sub(r'[^a-zA-Z0-9_.-]', '', suffix)[:20]
    
    # Create temp file with secure permissions (0o600)
    fd, temp_path = tempfile.mkstemp(prefix=safe_prefix, suffix=safe_suffix)
    os.close(fd)
    
    # Set secure permissions explicitly
    os.chmod(temp_path, 0o600)
    
    return Path(temp_path)


# Export all security functions
__all__ = [
    'SecurityError',
    'secure_file_path',
    'validate_input_size', 
    'sanitize_code_input',
    'validate_ttl_input',
    'create_safe_temp_file',
    'MAX_FILE_SIZE',
    'MAX_STRING_LENGTH',
    'MAX_COLLECTION_SIZE',
    'FORBIDDEN_CODE_PATTERNS'
]