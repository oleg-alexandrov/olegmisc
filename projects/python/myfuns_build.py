# Code for building myfunsmodule.c.

# Run this build script as:
# python myfuns_build.py build
# Then need to set the path to the build directory.
# (Here we cheat a little by just making a link.)
# ln -s build/lib.linux-i686-2.4/myfuns.so .

# See a sample run script at myfuns_run.py.

from distutils.core import setup, Extension

module1 = Extension('myfuns',
                    sources = ['myfuns_module.c'])

setup (name = 'PackageName',
       version = '1.0',
       description = 'This is a demo package',
       ext_modules = [module1])
