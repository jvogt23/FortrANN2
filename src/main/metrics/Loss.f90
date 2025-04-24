module cls_Loss
    implicit none

    type, public :: Loss
        real(kind=8), private :: loss
        real(kind=8), dimension(:,:), allocatable, private :: dInputs
    contains
        procedure, public :: calculateLoss
        procedure, public :: calcDLossDSoftmaxCombined
        procedure, public :: getLoss
        procedure, public :: getDLoss
    end type Loss
    
contains

    subroutine calculateLoss(self, outputs, actual)
        class(Loss), intent(inout) :: self
        real(kind=8), dimension(:,:), intent(in) :: outputs
        integer, dimension(:), intent(in) :: actual
        real(kind=8), dimension(:), allocatable :: sample_losses
        real(kind=8), dimension(:,:), allocatable :: clipped_predictions
        integer :: i, j

        if (size(outputs, 1).ne.size(actual)) then
            print *, "Arguments 'outputs' and 'actual' must have an equal"
            print *, "number of elements."
            stop
        end if

        allocate(sample_losses(size(actual)))
        allocate(clipped_predictions(size(outputs, 1), size(outputs, 2)))

        do i = 1, size(outputs, 1)
            do j = 1, size(outputs, 2)
                if (outputs(i, j).gt.(1.0 - 1.0E-7)) then
                    clipped_predictions(i, j) = (1.0 - 1.0E-7)
                else if (outputs(i, j).lt.1.0E-7) then
                    clipped_predictions(i, j) = 1.0E-7
                else
                    clipped_predictions(i, j) = outputs(i, j)
                end if
            end do
        end do

        do i = 1, size(actual)
            sample_losses(i) = -1.0 * dlog(clipped_predictions(i, actual(i)))
        end do

        self%loss = sum(sample_losses) / size(sample_losses)
    end subroutine calculateLoss

    subroutine calcDLossDSoftmaxCombined(self, dvalues, y_true)
        class(Loss), intent(inout) :: self
        real(kind=8), dimension(:,:), intent(in) :: dvalues
        integer, dimension(:), intent(in) :: y_true
        integer :: samples

        samples = size(dvalues, 1)
        self%dInputs = dvalues

        self%dInputs(:, y_true) = self%dInputs(:, y_true) - 1
        self%dInputs = self%dInputs / samples
    end subroutine calcDLossDSoftmaxCombined


    function getLoss(self) result(lossv)
        real(kind=8) :: lossv
        class(Loss) :: self
        lossv = self%loss
    end function getLoss

    function getDLoss(self) result(dlossv)
        class(Loss), intent(in) :: self
        real(kind=8), dimension(:,:), ALLOCATABLE :: dlossv
        dlossv = self%dInputs
    end function getDLoss

end module cls_Loss 