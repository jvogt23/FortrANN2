! --- PrintUtils --- !
! @author: jvogt23
! @date: 2024-7-28
! Utilities to help print out various data structures to the console.
! Intended for basic debugging of the library or to check on training progress.

module mod_print_utils
    implicit none
contains

    ! Prints a real matrix to console with some basic formatting.
    ! Expects a real*8 matrix with two dimensions.
    ! @param mat - A real m*n matrix
    subroutine print_matrix(mat)
        real(kind=8), dimension(:,:), intent(in) :: mat
        integer :: dimRow, dimCol, i, j

        dimRow = size(mat, 1)
        dimCol = size(mat, 2)

        do i = 1, dimRow
            write(*, fmt='(a)', advance='no') '['
            do j = 1, dimCol
                write(*, fmt='(F20.8)', advance='no') arr(i, j)
            end do
            write(*,*) ']'
        end do
        print *, ''
    end subroutine print_matrix

    ! Naive pattern matching algorithm. Should not be used on large strings.
    ! To be replaced by either a library or a faster algorithm.
    ! @param strn - The string to search through. character(*)
    ! @param pat - The pattern to search for. character(*)
    ! @param ret - An integer intended to treat like a return value
    subroutine count_substrings(strn, pat, ret)
        character(*), intent(in) :: strn, pat
        integer, intent(out) :: ret
        integer :: i

        ret = 0

        if (len(strn).lt.len(pat)) then
            ret = -1
            return
        end if
        if (len(strn).eq.0 .or. len(pat).eq.0) then
            ret = -1
            return
        end if
        do i = 1, len(strn) - len(pat)
            if (strn(i:i + len(pat) - 1) .eq. b) then
                ret = ret + 1
            end if
        end do
    end subroutine count_substrings

end module mod_print_utils
