! --- Math Utils --- !
! @author: jvogt23
! @date: 07-08-2024
! Module of functions and common constants needed for the math done in 
! the rest of this library.

! TODO: Add validation to params of the functions here
module mod_math_utils
    
    implicit none
    
    real(kind=8) :: pi = 3.141592653589793
    real(kind=8) :: e  = 2.718281828459045

contains

    ! uses a Box-Muller transform to change a uniform
    ! distribution random number into a normal Gaussian distribution
    ! random number.
    ! @return a real random number within the normal distribution
    function rand_normal() result(rand)
        real(kind=8) :: U_1, U_2
        real(kind=8) :: rand
        call random_number(U_1)
        call random_number(U_2)
        U_1 = abs(U_1)
        U_2 = abs(U_2)
        rand = sqrt(-2 * log(U_1)) * cos(2 * pi * U_2)
    end function rand_normal

    ! Applies normal distribution random numbers to all elements in a matrix.
    ! @param dim1 - Int Number of rows in matrix
    ! @param dim2 - Int Number of columns in matrix
    ! @return rand_normal_mat - Resulting real matrix sized col1 * col2
    function rand_normal_mat(dim1, dim2) result(rand_mat)
        integer, intent(in) :: dim1
        integer, intent(in) :: dim2
        real(kind=8), dimension(:,:), allocatable :: rand_mat
        integer :: i, j

        allocate(rand_mat(dim1, dim2))
        do i = 1, dim1
            do j = 1, dim2
                rand_mat(i, j) = rand_normal()
            end do
        end do
    end function rand_normal_mat

    ! Applies normal distribution random numbers to an array of size arrsize.
    ! @param arrsize - an integer representing the size of the array to create
    ! @return rand_normal_arr - resulting real array of size arrsize
    function rand_normal_arr(arrsize) result(rand_arr)
        integer, intent(in) :: arrsize
        real(kind=8), dimension(:), allocatable :: rand_arr
        integer :: i

        allocate(rand_arr(arrsize))
        do i = 1, arrsize
            rand_arr(i) = rand_normal()
        end do
    end function rand_normal_arr

    ! Performs exponent of e ** all elements in matrix mat.
    ! @param mat - matrix of real numbers to raise
    ! @return e_raise - real matrix after operation is applied
    function e_raise(mat) result(e_raised)
        real(kind=8), dimension(:,:), intent(inout) :: mat
        real(kind=8) :: e_raised(size(mat, 1), size(mat, 2))
        e_raised = e ** mat
    end function e_raise

    ! creates a square matrix with the elements of arr on the diagonal.
    ! @param arr - Real array to be converted, of size n
    ! @return diagflat - Real n*n matrix with arr's elements on the diagonal
    function diagflat(arr) result(diagflat_mat)
        real(kind=8), dimension(:), intent(in) :: arr
        real(kind=8), dimension(:,:), allocatable :: diagflat_mat
        integer :: i

        allocate(diagflat_mat(size(arr), size(arr)))
        do i = 1, size(arr)
            diagflat_mat(i, i) = arr(i)
        end do
    end function diagflat

end module mod_math_utils