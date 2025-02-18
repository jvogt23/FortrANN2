module cls_SoftmaxActivator
    use cls_DenseLayer, only: DenseLayer
    use cls_Activator, only: Activator
    use mod_math_utils, only: e_raise
    implicit none

    type, public, extends(Activator) :: SoftmaxActivator
        logical, private :: completed_forward = .false.
    contains
        procedure, public :: forward => softmax_forward
        procedure, public :: backward => softmax_backward
    end type SoftmaxActivator

contains

    subroutine softmax_forward(self, layer)
        class(SoftmaxActivator), intent(inout) :: self
        class(DenseLayer), intent(in) :: layer
        real(kind=8), dimension(:,:), allocatable :: maxes, ret, layerOutputs

        layerOutputs = layer%getOutputs()
        allocate(maxes(size(layerOutputs, 1), size(layerOutputs, 2)))
        allocate(ret(size(layerOutputs, 1), size(layerOutputs, 2)))

        maxes = spread(maxval(layerOutputs, 2), 2, size(maxes, 2))
        layerOutputs = layerOutputs - maxes
        ret = e_raise(layerOutputs)
        ret = ret / (spread(sum(ret, 2), 2, size(maxes, 2)))
        call self%setOutputs(ret)
        self%completed_forward = .true.
    end subroutine softmax_forward

    subroutine softmax_backward(self, dvalues)
        class(SoftmaxActivator), intent(inout) :: self
        real(kind=8), dimension(:,:), intent(in) :: dvalues
        real(kind=8), dimension(:,:), allocatable :: tempDInputs
        real(kind=8), dimension(:), allocatable :: single_output, single_dvalue
        real(kind=8), dimension(:,:), allocatable :: diagflat, jacobian
        real(kind=8), dimension(:,:), allocatable :: activator_outputs
        integer :: i, j

        if (.not.self%completed_forward) then
            print *, "Cannot perform a backward pass before a forward pass."
            stop
        end if

        allocate(tempDInputs(size(dvalues, 1), size(dvalues, 2)))
        activator_outputs = self%getOutputs()
        do i = 1, size(activator_outputs, 1)
            single_output = activator_outputs(i, :)
            single_dvalue = dvalues(i, :)
            allocate(diagflat(size(single_output), size(single_output)))
            diagflat = 0
            do j = 1, size(single_output)
                diagflat(j,j) = single_output(j)
            end do
            jacobian = diagflat - dot_product(single_output, single_output)
            tempDInputs(i, :) = matmul(jacobian, single_dvalue)
        end do
        call self%setDInputs(tempDInputs)
    end subroutine softmax_backward
end module cls_SoftmaxActivator
