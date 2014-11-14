compilers='gfortran ifort'

for compiler in $compilers; do 
  cd build/$compiler
  make
  cd ../..
  ./build/$compiler/bin/vm_driver
done
