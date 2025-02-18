! --- Accuracy --- !
! @author: jvogt23
! @date: 2024-12-22
! Calculates the accuracy of a given probability matrix against a vector
! of actual values.
! Each row of the given matrix is a probability vector.
! The vector of actual values gives the index in each row of the matrix
! where the correct value should be.

module cls_Accuracy
    implicit none

    type :: Accuracy
        real(kind=8), private :: accuracy = 0.0
    contains
        procedure :: calculate
        procedure :: getAccuracy
    end type Accuracy

contains

    ! Calculates the accuracy of a probability matrix against
    ! a vector providing the column for each row that should have the highest
    ! probability. Updates self%accuracy and returns nothing.
    ! @param probabilities - Real m*n matrix of probability row vectors
    ! @param actual - Integer m-vector of indices in each row of probabilities
    !                 where the highest probability should lie
    subroutine calculate(self, probabilities, actual)
        class(Accuracy), intent(inout) :: self
        real(kind=8), dimension(:,:), intent(in) :: probabilities
        integer, dimension(:), intent(in) :: actual
        integer, dimension(:), allocatable :: predictions
        integer :: i
        real(kind=8) :: predEqActual

        if(size(probabilities, 1) .ne. size(actual)) then
            print *, "Accuracy::calculate: An argument constraint was violated."
            print *, "Size of network output probabilities and actual"
            print *, "answers must match."
            stop
        end if

        self%accuracy = 0.0
        predEqActual = 0.0

        allocate(predictions(size(probabilities, 1)))
        predictions = maxloc(probabilities, dim=2)
        predEqActual = 0.0
        do i = 1, size(predictions)
            if (predictions(i).eq.actual(i)) then
                predEqActual = predEqActual + 1.0
            end if
        end do
        self%accuracy = (predEqActual / size(predictions))
    end subroutine calculate

    ! Getter for private field accuracy.
    function getAccuracy(self) result(acc)
        class(Accuracy), intent(in) :: self
        real(kind=8) :: acc
        acc = self%accuracy
    end function getAccuracy

end module cls_Accuracy