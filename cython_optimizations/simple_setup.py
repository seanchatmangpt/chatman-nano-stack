#!/usr/bin/env python3
"""
Simple Cython build script for working optimizations
"""

from setuptools import setup, Extension
from Cython.Build import cythonize
import numpy

# Simple extension
ext = Extension(
    "simple_aot_optimized",
    sources=["simple_aot_optimized.pyx"],
    include_dirs=[numpy.get_include()],
    extra_compile_args=["-O3", "-ffast-math"],
    language="c++"
)

setup(
    name="simple_aot_optimizations",
    ext_modules=cythonize([ext], 
                         compiler_directives={
                             'language_level': 3,
                             'boundscheck': False,
                             'wraparound': False
                         }),
    zip_safe=False
)