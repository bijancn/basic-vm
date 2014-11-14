# Include the directory itself as a path to include directories
set(CMAKE_INCLUDE_CURRENT_DIR ON)

set(vm_driver_srcs
  util.f90
  iso_varying_string.f90
  vm.f90
  driver.f90
  )

# file(GLOB vm_SOURCES *.f90)

add_executable(vm_driver ${vm_driver_srcs})
