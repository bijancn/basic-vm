module util
  use, intrinsic :: iso_fortran_env, only : input_unit, output_unit, error_unit
  implicit none
  private

  integer, parameter, public :: stdin = input_unit
  integer, parameter, public :: stdout = output_unit
  integer, parameter, public :: stderr = error_unit

  public :: single, double, quadruple, default
  integer, parameter :: single = &
       & selected_real_kind (precision(1.), range(1.))
  integer, parameter :: double = &
       & selected_real_kind (precision(1._single) + 1, range(1._single) + 1)
  integer, parameter :: quadruple = &
       & selected_real_kind (precision (1._double) + 1, range (1._double))
  integer, parameter :: default = double

  real(default), public :: pi = &
    3.1415926535897932384626433832795028841971693994_default
  real(default), public :: zero = 0.0_default
  real(default), public :: one = 1.0_default

  integer, parameter :: MIN_UNIT = 11, MAX_UNIT = 99

  public :: find_free_unit

contains

  subroutine find_free_unit (u, iostat)
    integer, intent(out) :: u
    integer, intent(out), optional :: iostat
    logical :: exists, is_open
    integer :: i, status
    do i = MIN_UNIT, MAX_UNIT
       inquire (unit = i, exist = exists, opened = is_open, &
            iostat = status)
       if (status == 0) then
          if (exists .and. .not. is_open) then
             u = i
             if (present (iostat)) then
                iostat = 0
             end if
             return
          end if
       end if
    end do
    if (present (iostat)) then
       iostat = -1
    end if
    u = -1
  end subroutine find_free_unit
end module util
