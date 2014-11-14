module vm_m
  use util
  use iso_varying_string, string_t => varying_string
  implicit none
  private

  integer, parameter :: ADD_REAL_FRACTION = 1
  integer, parameter :: ADD_INPUT = 2
  integer, parameter :: COMPUTE_RESULT = 3

  integer, parameter :: len_instructions = 5
  character(*), parameter :: vm_version = 'basic-vm-1.0'
  integer, parameter :: N_version_lines = 1
  ! Comment lines including the first header description line
  integer, parameter :: N_comments = 6
  ! Actual data lines plus intermediate description lines
  ! 'description \n 1 2 3 \n description \n 3 2 1' would count as 3
  integer, parameter :: N_header_lines = 1

  ! Don't touch this type
  type :: basic_vm_t
     private
     logical :: verbose
     type(string_t) :: bytecode_file
     integer :: bytecode_fh, out_fh
     integer :: N_instructions, N_levels
     integer :: N_table_lines
     integer, dimension(:, :), allocatable :: instructions
     integer, dimension(:), allocatable :: levels
  end type

  ! This is for customization
  type, public, extends (basic_vm_t) :: vm_t
     private
     integer :: N_factorials
     integer :: N_input_real, N_tmp_real, N_tmp_cmplx, N_output_cmplx
     real(default), dimension(:), allocatable :: table_factorials
     real(default), dimension(:), allocatable :: const_real
     complex(default), dimension(:), allocatable :: const_cmplx
     real(default), dimension(:), allocatable :: input_real
     real(default), dimension(:), allocatable :: tmp_real
     complex(default), dimension(:), allocatable :: tmp_cmplx
     complex(default), dimension(:), allocatable :: output_cmplx
   contains
     procedure :: init => vm_init
     procedure :: write => vm_write
     procedure :: run => vm_run
     procedure :: final => vm_final
  end type

contains

  ! Add here additional constants that you need
  subroutine vm_init (vm, bytecode_file, const_real, const_cmplx, &
         verbose, out_fh)
    class(vm_t), intent(out) :: vm
    type(string_t), intent(in) :: bytecode_file
    real(default), dimension(:), optional, intent(in) :: const_real
    complex(default), dimension(:), optional, intent(in) :: const_cmplx
    logical, optional, intent(in) :: verbose
    integer, optional, intent(in) :: out_fh
    vm%bytecode_file = bytecode_file
    if (present (const_real)) then
       allocate (vm%const_real (size (const_real)))
       vm%const_real = const_real
    end if
    if (present (const_cmplx)) then
       allocate (vm%const_cmplx (size (const_cmplx)))
       vm%const_cmplx = const_cmplx
    end if
    call basic_init (vm, verbose, out_fh)
  end subroutine vm_init

  subroutine vm_write (vm)
    class(vm_t), intent(in) :: vm
    integer :: i
    call basic_write (vm)
    write (vm%out_fh, *) 'N_factorials     =    ', vm%N_factorials
    write (vm%out_fh, *) 'N_input_real     =    ', vm%N_input_real
    write (vm%out_fh, *) 'N_tmp_real       =    ', vm%N_tmp_real
    write (vm%out_fh, *) 'N_tmp_cmplx      =    ', vm%N_tmp_cmplx
    write (vm%out_fh, *) 'N_output_cmplx   =    ', vm%N_output_cmplx
    write (vm%out_fh, *) 'table_factorials =    ', vm%table_factorials
    write (vm%out_fh, *) 'input_real       =    ', vm%input_real
    write (vm%out_fh, *) 'tmp_real         =    ', vm%tmp_real
    write (vm%out_fh, *) 'tmp_cmplx        =    ', vm%tmp_cmplx
    write (vm%out_fh, *) 'output_cmplx     =    ', vm%output_cmplx
  end subroutine vm_write

  subroutine vm_final (vm)
    class(vm_t), intent(inout) :: vm
    deallocate (vm%table_factorials)
    deallocate (vm%input_real)
    deallocate (vm%tmp_real)
    deallocate (vm%tmp_cmplx)
    deallocate (vm%output_cmplx)
  end subroutine vm_final

  subroutine load_header (vm, IO)
    type(vm_t), intent(inout) :: vm
    integer, intent(inout) :: IO
    integer, dimension(len_instructions) :: line
    read(vm%bytecode_fh, fmt = *, iostat = IO) line
    vm%N_factorials = line(1)
    vm%N_input_real = line(2)
    vm%N_tmp_real = line(3)
    vm%N_tmp_cmplx = line(4)
    vm%N_output_cmplx = line(5)
    ! Add 1 for seperating label lines like 'Another table'
    vm%N_table_lines = 1 + vm%N_factorials
  end subroutine load_header

  subroutine read_tables (vm, IO)
    type(vm_t), intent(inout) :: vm
    integer, intent(inout) :: IO
    integer :: i
    do i = 1, vm%N_factorials
       read(vm%bytecode_fh, fmt = *, iostat = IO) vm%table_factorials(i)
    end do
  end subroutine read_tables

  subroutine alloc_arrays (vm)
    type(vm_t) :: vm
    allocate (vm%table_factorials (vm%N_factorials))
    allocate (vm%input_real (vm%N_input_real))
    allocate (vm%tmp_real (vm%N_tmp_real))
    allocate (vm%tmp_cmplx (vm%N_tmp_cmplx))
    allocate (vm%output_cmplx (vm%N_output_cmplx))
  end subroutine alloc_arrays

  ! Make sure the decode function knows all possible instructions

  pure subroutine decode (vm, instruction_index)
    type(vm_t), intent(inout) :: vm
    integer, intent(in) :: instruction_index
    integer, dimension(len_instructions) :: i
    i = vm%instructions (:, instruction_index)
    select case (i(1))
    case ( : -1)       ! Jump over subinstructions
    case (ADD_REAL_FRACTION)
       vm%tmp_real (i(2)) = vm%tmp_real (i(2)) + (one * i(3)) / i(4)
    case (ADD_INPUT)
       vm%tmp_cmplx (i(2)) = vm%tmp_cmplx (i(2)) + &
            (vm%const_cmplx (i(3)) *  vm%input_real (i(3))) ** i(4) &
            / vm%table_factorials (i(5))
    case (COMPUTE_RESULT)
       vm%output_cmplx (i(2)) = vm%tmp_cmplx (i(2)) + &
            exp (vm%tmp_real (i(3))) + vm%tmp_real (i(4)) - vm%tmp_real (i(5))
    end select
  end subroutine decode

  ! Set the changing input here
  ! When you want to act additively on the tmp arrays, null them first

  subroutine vm_run (vm, input)
    class(vm_t), intent(inout) :: vm
    real(default), intent(in) :: input
    vm%input_real = input
    vm%tmp_real = zero
    vm%tmp_cmplx = cmplx (zero, zero, default)
    call iterate_instructions (vm)
  end subroutine vm_run

  subroutine extended_version_check (vm, IO)
    type(vm_t), intent(in) :: vm
    integer, intent(inout) :: IO
    character(256) :: buffer
  end subroutine extended_version_check

  !======================================================================!
  !  None of the functions below have to be adapted when changing the VM !
  !======================================================================!

  subroutine basic_init (vm, verbose, out_fh)
    type(vm_t), intent(inout) :: vm
    logical, optional, intent(in) :: verbose
    integer, optional, intent(in) :: out_fh
    if (present (verbose)) then
       vm%verbose = verbose
    else
       vm%verbose = .true.
    end if
    if (present (out_fh)) then
       vm%out_fh = out_fh
    else
       vm%out_fh = stdout
    end if
    call set_stream (vm)
    call alloc_and_count (vm)
    call read_bytecode (vm)
    call sanity_check (vm)
  end subroutine basic_init

  subroutine basic_write (vm)
    type(vm_t), intent(in) :: vm
    integer :: i
    write (vm%out_fh, *) '=====> VM ', vm_version, ' <====='
    write (vm%out_fh, *) 'verbose          =    ', vm%verbose
    write (vm%out_fh, *) 'bytecode_file    =    ', char (vm%bytecode_file)
    write (vm%out_fh, *) 'N_instructions   =    ', vm%N_instructions
    write (vm%out_fh, *) 'N_levels         =    ', vm%N_levels
    write (vm%out_fh, *) 'instructions     =    '
    do i = 1, vm%N_instructions
       write (vm%out_fh, *) vm%instructions(:, i)
    end do
    write (vm%out_fh, *) 'levels           =    ', vm%levels
  end subroutine basic_write

  subroutine alloc_and_count (vm)
    type(vm_t), intent(inout) :: vm
    integer, dimension(len_instructions) :: line
    character(256) :: buffer
    integer :: i, IO
    read(vm%bytecode_fh, fmt = *, iostat = IO) buffer
    if (vm_version /= buffer) then
       stop "Bytecode has been generated for another VM."
    else
       if (vm%verbose) then
          write (vm%out_fh, fmt = *) "Bytecode version fits."
       end if
    end if
    call extended_version_check (vm, IO)
    write (vm%out_fh, fmt = *) "Trying to allocate."
    do i = 1, N_comments
       read(vm%bytecode_fh, fmt = *, iostat = IO)
    end do
    call load_header (vm, IO)
    call alloc_arrays (vm)
    do i = 1, vm%N_table_lines + 1
       read(vm%bytecode_fh, fmt = *, iostat = IO)
    end do
    vm%N_instructions = 0
    vm%N_levels = 0
    do
       read(vm%bytecode_fh, fmt = *, end = 42) line
       if (line(1) /= 0) then
          vm%N_instructions = vm%N_instructions + 1
       else
          vm%N_levels = vm%N_levels + 1
       end if
    end do
    42 rewind(vm%bytecode_fh, iostat = IO)
    allocate (vm%instructions(len_instructions, vm%N_instructions))
    allocate (vm%levels(vm%N_levels))
    if (IO /= 0) then
       stop "Error: vm.alloc : Couldn't load bytecode!"
    end if
  end subroutine alloc_and_count

  subroutine read_bytecode (vm)
    type(vm_t), intent(inout) :: vm
    integer, dimension(len_instructions) :: line
    integer :: i, j, IO
    ! Jump over version number, comments, header and first table description
    do i = 1, N_version_lines + N_comments + N_header_lines + 1
       read (vm%bytecode_fh, fmt = *, iostat = IO)
    end do
    call read_tables (vm, IO)
    read (vm%bytecode_fh, fmt = *, iostat = IO)
    i = 0; j = 0
    do
       read (vm%bytecode_fh, fmt = *, iostat = IO) line
       if (IO /= 0) exit
       if (line(1) == 0) then
          if (j <= vm%N_levels) then
             j = j + 1
             vm%levels(j) = i                 ! last index of a level is saved
          else
             stop 'Error: vm.read_bytecode: File has more levels than anticipated!'
          end if
       else
          if (i <= vm%N_instructions) then
             i = i + 1                        ! A valid instruction line
             vm%instructions(:, i) = line
          else
             stop 'Error: vm.read_bytecode: File is larger than anticipated!'
          end if
       end if
    end do
  end subroutine read_bytecode

  subroutine iterate_instructions (vm)
    type(vm_t), intent(inout) :: vm
    integer :: i, j
    !$omp parallel
    do j = 1, vm%N_levels - 1
       !$omp do schedule (static)
       do i = vm%levels (j) + 1, vm%levels (j + 1)
          call decode (vm, i)
       end do
       !$omp end do
    end do
    !$omp end parallel
  end subroutine iterate_instructions

  subroutine set_stream (vm)
    type(vm_t), intent(inout) :: vm
    integer :: IO
    call find_free_unit (vm%bytecode_fh, IO)
    open (vm%bytecode_fh, file = char (vm%bytecode_file), form = 'formatted', &
         access = 'sequential', status = 'old', position = 'rewind', &
         iostat = IO, action = 'read')
    if (IO /= 0) then
       stop "Error: vm.set_stream: Bytecode file not found!"
    end if
  end subroutine set_stream

  subroutine sanity_check (vm)
    type(vm_t), intent(in) :: vm
    if (vm%levels(1) /= 0) then
       stop "Error: vm.vm_init: levels(1) != 0"
    end if
    if (vm%levels(vm%N_levels) /= vm%N_instructions) then
       stop "Error: vm.vm_init: levels(N_levels) != N_instructions"
    end if
    if (vm%verbose) then
       write(vm%out_fh, *) "vm passed sanity check. Starting calculation."
    end if
  end subroutine sanity_check

end module vm_m
