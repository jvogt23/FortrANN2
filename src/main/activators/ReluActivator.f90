module cls_ReluActivator
    use cls_Activator, only: Activator
    use cls_DenseLayer, only: DenseLayer
    implicit none

    type, public, extends(Activator) :: ReluActivator
        logical, private :: completed_forward = .false.
    contains
        procedure, public :: forward => relu_forward
        procedure, public :: backward => relu_backward
    end type ReluActivator

contains

    subroutine relu_forward(self, layer)
        class(ReluActivator), intent(inout) :: self
        class(DenseLayer), intent(in) :: layer
        real(kind=8), dimension(:,:), allocatable :: tempOutputs
        integer :: i, j

        allocate(tempOutputs(size(layer%getOutputs(), 1), &
            size(layer%getOutputs(), 2)))

        do i = 1, size(tempOutputs, 1)
            do j = 1, size(tempOutputs, 2)
                tempOutputs(i, j) = max(tempOutputs(i, j), 0.0)
            end do
        end do
        call self%setOutputs(tempOutputs)
        call self%setInputs(layer%getOutputs())
        self%completed_forward = .true.
    end subroutine relu_forward

    subroutine relu_backward(self, dvalues)
        class(ReluActivator), intent(inout) :: self
        real(kind=8), dimension(:,:), intent(in) :: dvalues
        real(kind=8), dimension(:,:), allocatable :: tempDInputs, tempInputs
        integer :: i, j

        if (.not.self%completed_forward) then
            print *, "Cannot perform backward pass before forward pass."
            stop
        end if

        allocate(tempDInputs(size(dvalues, 1), size(dvalues, 2)))
        tempDInputs = dvalues
        tempInputs = self%getInputs()
        
        do i = 1, size(tempInputs, 1)
            do j = 1, size(tempInputs, 2)
                if (tempInputs(i, j).lt.0.0) then
                    tempDInputs(i, j) = 0.0
                end if
            end do
        end do
        call self%setDInputs(tempDInputs)
    end subroutine relu_backward
end module cls_ReluActivator