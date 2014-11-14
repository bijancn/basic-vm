Usage
--------------------------------------------------------------------------------
All instructions work when run from the top-level, i.e. where this very
`README` file is located.
When you run the commands from other directories, you might have to copy around
files.

To create build directories for `gfortran` and `ifort`, just run
`./share/create_builds.sh` or edit the compiler and flag list therein.
This allows you to run `./build/COMPILER/bin/vm_driver` given there has been no
problems during compile and link phase.
In case you don't want to use `cmake`, it is also straightforward to write
a `Makefile` or generate it with `Autotools` since there are only three
source code files and a driver file in `src/`.

To use another bytecode, you can overwrite the default `share/basic_test.bc`
with `python share/basic_bytecode_generator.py` after you have changed
`N_factorials` to the value you desire.

Dependencies
--------------------------------------------------------------------------------
When you have a modern system, the below dependencies are most likely already
fulfilled. In Ubuntu and derivates you need at most a
`sudo apt-get install cmake gfortran`.

- For automatic `Fortran` compiling and linking: `cmake 2.8`
- `Fortran` compiler: Tested are `gfortran 4.8.2` and `ifort 14.0.3`
- To create bytecode automatically for the `basic_vm`: `python 2.7`
