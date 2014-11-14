program vm_driver
  use vm_m
  use util
  use iso_varying_string, string_t => varying_string
  implicit none
  type(vm_t) :: vm
  type(string_t) :: bytecode_file
  real(default) :: input = 0.5_default
  complex(default), dimension(1) :: i_pi
  bytecode_file = 'share/basic_test.bc'
  i_pi = cmplx (zero, one, default) * pi
  call vm%init (bytecode_file, verbose = .True., const_cmplx = i_pi)
  call vm%run (input)
  call vm%write ()
  call vm%final ()
end program vm_driver
