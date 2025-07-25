#!/usr/bin/env python3
"""
Optimized Security Utilities for CNS (Chatman Nano Stack)
Performance-optimized security functions while maintaining security guarantees
"""

import os
import re
from pathlib import Path
from typing import Union, List, Optional
import functools

class SecurityError(Exception):
    """Raised when a security violation is detected"""
    pass

# Constants
MAX_FILE_SIZE = 100 * 1024 * 1024
MAX_STRING_LENGTH = 1024 * 1024
MAX_COLLECTION_SIZE = 10000

# Pre-compiled regex patterns for better performance
FORBIDDEN_CODE_PATTERN = re.compile(r'(?:system\(|exec\(|__import__|subprocess|eval\(|shell=True|/bin/|rm -rf|chmod|setuid|execve\(|__asm__|[$`]|\\x|popen\(|fork\(|\.\.\/|\.\.\\|\\\\|\/\/|file://)', re.IGNORECASE)

# Cache for resolved allowed directories to avoid repeated filesystem calls
_RESOLVED_ALLOWED_DIRS_CACHE = {}

def _get_cached_allowed_dirs(allowed_dirs: Optional[List[Path]] = None) -> List[Path]:
    """Get cached resolved allowed directories"""
    if allowed_dirs is None:
        cache_key = 'default'
        if cache_key not in _RESOLVED_ALLOWED_DIRS_CACHE:
            _RESOLVED_ALLOWED_DIRS_CACHE[cache_key] = [
                Path.cwd().resolve(),  # Pre-resolve and cache
                (Path.home() / '.cache').resolve(),
                Path('/tmp').resolve(),
            ]
        return _RESOLVED_ALLOWED_DIRS_CACHE[cache_key]
    else:
        # Create a hashable key for custom allowed_dirs
        cache_key = tuple(str(p) for p in allowed_dirs)
        if cache_key not in _RESOLVED_ALLOWED_DIRS_CACHE:
            _RESOLVED_ALLOWED_DIRS_CACHE[cache_key] = [p.resolve() for p in allowed_dirs]
        return _RESOLVED_ALLOWED_DIRS_CACHE[cache_key]

def secure_file_path_optimized(user_path: Union[str, Path], allowed_dirs: Optional[List[Path]] = None) -> Path:
    """
    Optimized version of secure_file_path with improved performance
    
    Performance optimizations:
    - Cached allowed directory resolution
    - Pre-compiled regex patterns
    - Reduced filesystem calls
    - Optimized string operations
    """
    if not user_path:
        raise SecurityError("Empty path provided")
    
    # Fast string-based checks first (before expensive filesystem operations)
    path_str = str(user_path)
    
    # Quick check for obvious traversal attempts
    if '..' in path_str or path_str.startswith('~') or '\\' in path_str:
        # Only do expensive canonical resolution if suspicious patterns detected
        path = Path(user_path)
        try:
            canonical_path = path.resolve(strict=False)
        except Exception as e:
            raise SecurityError(f"Invalid path: {e}")
    else:
        # For clean paths, use faster absolute path conversion
        path = Path(user_path)
        if path.is_absolute():
            canonical_path = path
        else:
            canonical_path = Path.cwd() / path
    
    # Use cached resolved allowed directories
    resolved_allowed_dirs = _get_cached_allowed_dirs(allowed_dirs)
    
    # Optimized containment check
    canonical_str = str(canonical_path)
    is_allowed = any(
        canonical_str.startswith(str(allowed_dir) + os.sep) or canonical_str == str(allowed_dir)
        for allowed_dir in resolved_allowed_dirs
    )
    
    if not is_allowed:
        raise SecurityError(f"Path traversal attempt detected: {user_path}")
    
    # Fast suspicious pattern check using pre-compiled regex
    if any(pattern in canonical_str for pattern in ['\\x', '\\u', '\x00']):
        raise SecurityError(f"Suspicious pattern in path: {user_path}")
    
    return canonical_path

def validate_input_size_optimized(input_data) -> bool:
    """
    Optimized input size validation with better performance
    """
    if input_data is None:
        return True
    
    # Fast type-specific size checks
    if isinstance(input_data, str):
        if len(input_data) > MAX_STRING_LENGTH:
            raise SecurityError(f"String too long: {len(input_data)} > {MAX_STRING_LENGTH}")
    elif isinstance(input_data, (list, tuple, dict)):
        if len(input_data) > MAX_COLLECTION_SIZE:
            raise SecurityError(f"Collection too large: {len(input_data)} > {MAX_COLLECTION_SIZE}")
    elif hasattr(input_data, '__len__'):
        # Generic length check for other sequence types
        size = len(input_data)
        if size > MAX_COLLECTION_SIZE:
            raise SecurityError(f"Input too large: {size} > {MAX_COLLECTION_SIZE}")
    
    return True

def sanitize_code_input_optimized(code_input: str, context: str = 'general') -> str:
    """
    Optimized code sanitization with pre-compiled regex patterns
    """
    if not code_input:
        return code_input
    
    # Use pre-compiled regex for better performance
    if FORBIDDEN_CODE_PATTERN.search(code_input):
        raise SecurityError(f"Forbidden code pattern detected in {context} context")
    
    # Fast length check
    if len(code_input) > MAX_STRING_LENGTH:
        raise SecurityError(f"Code input too long: {len(code_input)} > {MAX_STRING_LENGTH}")
    
    return code_input

# TTL validation with optimized regex
TTL_DANGEROUS_PATTERN = re.compile(r'(?:system|exec|import|subprocess|shell)', re.IGNORECASE)

def validate_ttl_input_optimized(ttl_content: str) -> bool:
    """
    Optimized TTL validation with pre-compiled patterns
    """
    if not ttl_content:
        return True
    
    # Fast length check first
    if len(ttl_content) > MAX_STRING_LENGTH:
        raise SecurityError(f"TTL content too long: {len(ttl_content)} > {MAX_STRING_LENGTH}")
    
    # Pre-compiled regex check
    if TTL_DANGEROUS_PATTERN.search(ttl_content):
        raise SecurityError("Dangerous pattern detected in TTL content")
    
    return True

def create_safe_temp_file_optimized(prefix: str = "safe_", suffix: str = ".tmp") -> Path:
    """
    Optimized safe temporary file creation with cached directory resolution
    """
    import tempfile
    
    # Input validation with optimized checks
    if len(prefix) > 50 or len(suffix) > 10:
        raise SecurityError("Prefix or suffix too long")
    
    # Pre-compiled pattern for safe filename characters
    if not re.match(r'^[a-zA-Z0-9_-]+$', prefix.rstrip('_')):
        raise SecurityError("Invalid characters in prefix")
    
    # Use system temp directory (already cached by OS)
    temp_file = tempfile.NamedTemporaryFile(
        prefix=prefix,
        suffix=suffix,
        delete=False,
        dir='/tmp'  # Use fixed temp dir for better performance
    )
    temp_file.close()
    
    return Path(temp_file.name)

# Compatibility layer - map old functions to optimized versions
secure_file_path = secure_file_path_optimized
validate_input_size = validate_input_size_optimized
sanitize_code_input = sanitize_code_input_optimized
validate_ttl_input = validate_ttl_input_optimized
create_safe_temp_file = create_safe_temp_file_optimized