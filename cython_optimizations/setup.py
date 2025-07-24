#!/usr/bin/env python3
"""
CYTHON OPTIMIZATION BUILD SCRIPT
Builds optimized Cython extensions for Python AOT compilation components
"""

from setuptools import setup, Extension
from Cython.Build import cythonize
import numpy
import sys
import os

# Compiler optimization flags
extra_compile_args = [
    "-O3",                    # Maximum optimization
    "-march=native",          # Optimize for current CPU
    "-mtune=native",          # Tune for current CPU  
    "-ffast-math",           # Fast math operations
    "-funroll-loops",        # Unroll loops
    "-finline-functions",    # Inline functions
    "-fomit-frame-pointer",  # Omit frame pointer
    "-DNPY_NO_DEPRECATED_API=NPY_1_7_API_VERSION"
]

extra_link_args = [
    "-O3",
    "-flto"  # Link-time optimization
]

# Platform-specific optimizations
if sys.platform == "linux":
    extra_compile_args.extend([
        "-fopenmp",              # OpenMP support
        "-pthread"               # POSIX threads
    ])
    extra_link_args.extend([
        "-fopenmp",
        "-pthread"
    ])
elif sys.platform == "darwin":  # macOS
    extra_compile_args.extend([
        "-Xpreprocessor", "-fopenmp",
        "-I$(brew --prefix libomp)/include"
    ])
    extra_link_args.extend([
        "-Xpreprocessor", "-fopenmp",
        "-L$(brew --prefix libomp)/lib",
        "-lomp"
    ])

# Define extensions
extensions = [
    Extension(
        "aot_lifecycle_optimized",
        sources=["aot_lifecycle_optimized.pyx"],
        include_dirs=[numpy.get_include()],
        extra_compile_args=extra_compile_args,
        extra_link_args=extra_link_args,
        language="c++"
    ),
    Extension(
        "bitactor_compiler_optimized", 
        sources=["bitactor_compiler_optimized.pyx"],
        include_dirs=[numpy.get_include()],
        extra_compile_args=extra_compile_args,
        extra_link_args=extra_link_args,
        language="c++"
    ),
    Extension(
        "owl_compiler_optimized",
        sources=["owl_compiler_optimized.pyx"], 
        include_dirs=[numpy.get_include()],
        extra_compile_args=extra_compile_args,
        extra_link_args=extra_link_args,
        language="c++"
    ),
    Extension(
        "performance_utils",
        sources=["performance_utils.pyx"],
        include_dirs=[numpy.get_include()],
        extra_compile_args=extra_compile_args,
        extra_link_args=extra_link_args,
        language="c++"
    )
]

# Compiler directives for maximum performance
compiler_directives = {
    'language_level': 3,
    'boundscheck': False,      # Disable bounds checking
    'wraparound': False,       # Disable negative index wrapping
    'nonecheck': False,        # Disable None checking
    'cdivision': True,         # Use C division semantics
    'overflowcheck': False,    # Disable overflow checking
    'initializedcheck': False, # Disable initialization checking
    'infer_types': True,       # Infer variable types
    'embedsignature': True,    # Embed function signatures
    'profile': False,          # Disable profiling overhead
    'linetrace': False,        # Disable line tracing
    'binding': False,          # Disable dynamic binding
    'auto_pickle': False       # Disable auto pickling
}

# Build configuration
setup(
    name="python_aot_optimizations",
    version="1.0.0",
    description="Ultra-optimized Cython extensions for Python AOT compilation",
    ext_modules=cythonize(
        extensions,
        compiler_directives=compiler_directives,
        annotate=True,  # Generate HTML annotation files
        nthreads=os.cpu_count()  # Parallel compilation
    ),
    zip_safe=False,
    python_requires=">=3.8",
    install_requires=[
        "numpy>=1.19.0",
        "cython>=0.29.0"
    ]
)

if __name__ == "__main__":
    print("🔧 Building Cython optimizations for Python AOT compilation...")
    print("Compiler flags:", " ".join(extra_compile_args))
    print("Platform optimizations enabled for:", sys.platform)