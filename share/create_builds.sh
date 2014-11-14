fflags=-O3
compilers='gfortran ifort'

for compiler in $compilers; do 
  mkdir -p build/$compiler
  cd build/$compiler
  cmake -D CMAKE_Fortran_COMPILER="$compiler" -D CMAKE_Fortran_FLAGS=$fflags ../..
  make
  cd ../..
done
