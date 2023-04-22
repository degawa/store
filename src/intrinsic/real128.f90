module store_intrinsic_real128
    use, intrinsic :: iso_fortran_env
    implicit none
    private
#if defined(__GFORTRAN__) || defined(__INTEL_COMPILER) || defined(NAGFOR)
    public :: store
    public :: store_a

    interface store
        procedure :: store_real128
        procedure :: store_real128_rank1
        procedure :: store_real128_rank2
        procedure :: store_real128_rank3
        procedure :: store_real128_rank4
        procedure :: store_real128_rank5
        procedure :: store_real128_rank6
        procedure :: store_real128_rank7
        procedure :: store_real128_rank8
        procedure :: store_real128_rank9
        procedure :: store_real128_rank10
        procedure :: store_real128_rank11
        procedure :: store_real128_rank12
        procedure :: store_real128_rank13
        procedure :: store_real128_rank14
        procedure :: store_real128_rank15
    end interface

    interface store_a
        procedure :: store_real128_a
        procedure :: store_real128_rank1_a
        procedure :: store_real128_rank2_a
        procedure :: store_real128_rank3_a
        procedure :: store_real128_rank4_a
        procedure :: store_real128_rank5_a
        procedure :: store_real128_rank6_a
        procedure :: store_real128_rank7_a
        procedure :: store_real128_rank8_a
        procedure :: store_real128_rank9_a
        procedure :: store_real128_rank10_a
        procedure :: store_real128_rank11_a
        procedure :: store_real128_rank12_a
        procedure :: store_real128_rank13_a
        procedure :: store_real128_rank14_a
        procedure :: store_real128_rank15_a
    end interface
contains
    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128(var, value)
        implicit none
        real(real128), intent(out), optional :: var
        real(real128), intent(in) :: value

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank1(var, value)
        implicit none
        real(real128), intent(out), optional :: var(:)
        real(real128), intent(in) :: value(:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank1

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank2(var, value)
        implicit none
        real(real128), intent(out), optional :: var(:,:)
        real(real128), intent(in) :: value(:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank2

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank3(var, value)
        implicit none
        real(real128), intent(out), optional :: var(:,:,:)
        real(real128), intent(in) :: value(:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank3

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank4(var, value)
        implicit none
        real(real128), intent(out), optional :: var(:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank4

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank5(var, value)
        implicit none
        real(real128), intent(out), optional :: var(:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank5

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank6(var, value)
        implicit none
        real(real128), intent(out), optional :: var(:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank6

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank7(var, value)
        implicit none
        real(real128), intent(out), optional :: var(:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank7

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank8(var, value)
        implicit none
        real(real128), intent(out), optional :: var(:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank8

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank9(var, value)
        implicit none
        real(real128), intent(out), optional :: var(:,:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank9

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank10(var, value)
        implicit none
        real(real128), intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank10

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank11(var, value)
        implicit none
        real(real128), intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank11

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank12(var, value)
        implicit none
        real(real128), intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank12

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank13(var, value)
        implicit none
        real(real128), intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank13

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank14(var, value)
        implicit none
        real(real128), intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank14

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank15(var, value)
        implicit none
        real(real128), intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank15

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var
        real(real128), intent(in) :: value

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank1_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var(:)
        real(real128), intent(in) :: value(:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank1_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank2_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var(:,:)
        real(real128), intent(in) :: value(:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank2_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank3_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var(:,:,:)
        real(real128), intent(in) :: value(:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank3_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank4_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var(:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank4_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank5_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var(:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank5_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank6_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var(:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank6_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank7_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var(:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank7_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank8_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank8_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank9_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank9_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank10_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank10_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank11_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank11_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank12_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank12_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank13_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank13_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank14_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank14_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_real128_rank15_a(var, value)
        implicit none
        real(real128), allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        real(real128), intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_real128_rank15_a

#endif
end module store_intrinsic_real128