module store_intrinsic_char
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: store
    public :: store_a

    interface store
        procedure :: store_char
        procedure :: store_char_rank1
        procedure :: store_char_rank2
        procedure :: store_char_rank3
        procedure :: store_char_rank4
        procedure :: store_char_rank5
        procedure :: store_char_rank6
        procedure :: store_char_rank7
        procedure :: store_char_rank8
        procedure :: store_char_rank9
        procedure :: store_char_rank10
        procedure :: store_char_rank11
        procedure :: store_char_rank12
        procedure :: store_char_rank13
        procedure :: store_char_rank14
        procedure :: store_char_rank15
    end interface

    interface store_a
        procedure :: store_char_a
        procedure :: store_char_rank1_a
        procedure :: store_char_rank2_a
        procedure :: store_char_rank3_a
        procedure :: store_char_rank4_a
        procedure :: store_char_rank5_a
        procedure :: store_char_rank6_a
        procedure :: store_char_rank7_a
        procedure :: store_char_rank8_a
        procedure :: store_char_rank9_a
        procedure :: store_char_rank10_a
        procedure :: store_char_rank11_a
        procedure :: store_char_rank12_a
        procedure :: store_char_rank13_a
        procedure :: store_char_rank14_a
        procedure :: store_char_rank15_a
    end interface
contains
    !>stores `value` to `var` if `var` is presented.
    subroutine store_char(var, value)
        implicit none
        character, intent(out), optional :: var
        character, intent(in) :: value

        if (present(var)) then
            var = value
        end if
    end subroutine store_char

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank1(var, value)
        implicit none
        character, intent(out), optional :: var(:)
        character, intent(in) :: value(:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank1

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank2(var, value)
        implicit none
        character, intent(out), optional :: var(:,:)
        character, intent(in) :: value(:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank2

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank3(var, value)
        implicit none
        character, intent(out), optional :: var(:,:,:)
        character, intent(in) :: value(:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank3

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank4(var, value)
        implicit none
        character, intent(out), optional :: var(:,:,:,:)
        character, intent(in) :: value(:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank4

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank5(var, value)
        implicit none
        character, intent(out), optional :: var(:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank5

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank6(var, value)
        implicit none
        character, intent(out), optional :: var(:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank6

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank7(var, value)
        implicit none
        character, intent(out), optional :: var(:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank7

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank8(var, value)
        implicit none
        character, intent(out), optional :: var(:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank8

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank9(var, value)
        implicit none
        character, intent(out), optional :: var(:,:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank9

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank10(var, value)
        implicit none
        character, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank10

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank11(var, value)
        implicit none
        character, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank11

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank12(var, value)
        implicit none
        character, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank12

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank13(var, value)
        implicit none
        character, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank13

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank14(var, value)
        implicit none
        character, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank14

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank15(var, value)
        implicit none
        character, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank15

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var
        character, intent(in) :: value

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank1_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var(:)
        character, intent(in) :: value(:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank1_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank2_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var(:,:)
        character, intent(in) :: value(:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank2_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank3_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var(:,:,:)
        character, intent(in) :: value(:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank3_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank4_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var(:,:,:,:)
        character, intent(in) :: value(:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank4_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank5_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var(:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank5_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank6_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var(:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank6_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank7_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var(:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank7_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank8_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank8_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank9_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank9_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank10_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank10_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank11_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank11_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank12_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank12_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank13_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank13_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank14_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank14_a

    !>stores `value` to `var` if `var` is presented.
    subroutine store_char_rank15_a(var, value)
        implicit none
        character, allocatable, intent(out), optional :: var(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)
        character, intent(in) :: value(:,:,:,:,:,:,:,:,:,:,:,:,:,:,:)

        if (present(var)) then
            var = value
        end if
    end subroutine store_char_rank15_a

end module store_intrinsic_char
