module cls_DenseLayer
    use mod_math_utils
    use cls_Layer, only: Layer
    implicit none

    type, public, extends(Layer) :: DenseLayer
        real(kind=8), dimension(:,:), private, allocatable :: weights
        real(kind=8), dimension(:), private, allocatable :: biases
        real(kind=8), dimension(:,:), private, allocatable :: inputs
        real(kind=8), dimension(:,:), private, allocatable :: outputs
        real(kind=8), dimension(:,:), private, allocatable :: dweights
        real(kind=8), dimension(:,:), private, allocatable :: dinputs
        real(kind=8), dimension(:), private, allocatable :: dbiases
    contains
        procedure, public :: init => dl_init
        procedure, public :: forward => dl_forward
        procedure, public :: backward => dl_backward
        procedure, public :: getOutputs
        procedure, public :: getWeights
        procedure, public :: getBiases
        procedure, public :: setWeights
        procedure, public :: setBiases
    end type DenseLayer

contains
! TODO: Give all of the subroutines that may fail optional "success" parameter
    subroutine dl_init(self, nInputs, nNeurons)
        class(DenseLayer), intent(inout) :: self
        integer, intent(in) :: nInputs, nNeurons
        real(kind=8), dimension(nInputs, nNeurons) :: tempWeights
        real(kind=8), dimension(nNeurons) :: tempBiases

        tempBiases = 0.0
        tempWeights = rand_normal_mat(nInputs, nNeurons)

        self%weights = tempWeights
        self%biases = tempBiases
    end subroutine dl_init

    subroutine dl_forward(self, input)
        class(DenseLayer), intent(inout) :: self
        real(kind=8), dimension(:,:), intent(in) :: input
        real(kind=8), dimension(:,:), allocatable :: spreadBias

        if (allocated(self%inputs)) then
            deallocate(self%inputs)
        end if
        allocate(self%inputs(size(input, 1), size(input, 2)))
        self%inputs = input

        spreadBias = spread(self%biases, 1, size(input, 1))

        self%outputs = matmul(input, self%weights) + spreadBias
    end subroutine dl_forward

    subroutine dl_backward(self, dActivator)
        class(DenseLayer), intent(inout) :: self
        real(kind=8), dimension(:,:), intent(in) :: dActivator

        if (.not.allocated(self%weights)) then
            print *, "No weights on this layer. May not have been initialized."
            stop
        end if

        if (.not.allocated(self%biases)) then
            print *, "No biases on this layer. May not have been initialized."
            stop
        end if

        if (.not.allocated(self%inputs)) then
            print *, "Cannot run a backward pass without a saved set of inputs"
            print *, "from a previous forward pass."
            stop
        end if

        if (allocated(self%dweights)) then
            deallocate(self%dweights)
        end if
        if (allocated(self%dinputs)) then
            deallocate(self%dinputs)
        end if
        if (allocated(self%dbiases)) then
            deallocate(self%dbiases)
        end if

        allocate(self%dbiases(size(self%biases)))
        allocate(self%dweights(size(transpose(self%inputs), 1), &
            size(dActivator, 2)))
        allocate(self%dinputs(size(dActivator, 1), &
            size(transpose(self%weights), 2)))
        
        self%dbiases = sum(dActivator, 1)
        self%dweights = matmul(transpose(self%inputs), dActivator)
        self%dinputs = matmul(dActivator, transpose(self%weights))
    end subroutine dl_backward

    function getOutputs(self) result(outputs)
        real(kind=8), dimension(:,:), allocatable :: outputs
        class(DenseLayer), intent(in) :: self

        if (.not.allocated(self%outputs)) then
            allocate(outputs(0,0))
        else
            allocate(outputs(size(self%outputs, 1), size(self%outputs, 2)))
            outputs = self%outputs
        end if
    end function getOutputs

    function getWeights(self) result(weights)
        real(kind=8), dimension(:,:), allocatable :: weights
        class(DenseLayer), intent(in) :: self

        if (.not.allocated(self%weights)) then
            allocate(weights(0,0))
        else
            weights = self%weights
        end if
    end function getWeights

    function getBiases(self) result(biases)
        real(kind=8), dimension(:), allocatable :: biases
        class(DenseLayer), intent(in) :: self

        if (.not.allocated(self%biases)) then
            allocate(biases(0))
        else
            biases = self%biases
        end if
    end function getBiases

    subroutine setWeights(self, weights)
        class(DenseLayer), intent(inout) :: self
        real(kind=8), dimension(:,:), intent(in) :: weights
        self%weights = weights
    end subroutine setWeights

    subroutine setBiases(self, biases)
        class(DenseLayer), intent(inout) :: self
        real(kind=8), dimension(:), intent(in) :: biases
        self%biases = biases
    end subroutine setBiases

end module cls_DenseLayer