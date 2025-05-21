module cls_SgdOptimizer
    use cls_DenseLayer, only: DenseLayer
! TODO: Make Learning rate getter, setter
    type, public :: SgdOptimizer
        real(kind=8), private :: momentum, learning_rate
        real(kind=8), dimension(:,:), allocatable :: weight_momenta
        real(kind=8), dimension(:), allocatable :: bias_momenta
    contains
        procedure, public :: init
        procedure, public :: get_momentum
        procedure, public :: set_momentum
        procedure, public :: update_parameters
    end type SgdOptimizer

contains

    subroutine init(self, learning_rate, momentum, weight_rows, weight_cols)
        class(SgdOptimizer), INTENT(INOUT) :: self
        real(kind=8), intent(in) :: momentum
        integer, intent(in) :: weight_rows, weight_cols
        self%momentum = momentum
        self%learning_rate = learning_rate
        allocate(self%weight_momenta(weight_rows, weight_cols))
        allocate(self%bias_momenta(weight_rows))
        self%weight_momenta = 0.0
        self%bias_momenta = 0.0
    end subroutine init

    subroutine set_momentum(self, momentum)
        class(SgdOptimizer), intent(inout) :: self
        real(kind=8), intent(in) :: momentum
        self%momentum = momentum
    end subroutine set_momentum

    function get_momentum(self) result(momentum)
        class(SgdOptimizer), intent(in) :: self
        real(kind=8) :: momentum
        momentum = self%momentum
    end function get_momentum

    subroutine update_parameters(self, layer)
        class(SgdOptimizer), intent(inout) :: self
        class(DenseLayer), intent(inout) :: layer
        real(kind=8), dimension(:,:), allocatable :: weight_update, bias_update

        if (self%momentum.not..eq.0.0) then
            weight_update = ((self%momentum * self%weight_momenta) &
                - self%learning_rate * layer%getDWeights())
            bias_update = ((self%momentum * self%bias_momenta) &
                - self%learning_rate * layer%getDBiases())
            self%bias_momenta = bias_update
            self%weight_momenta = weight_update
        else 
            weight_update = - self%learning_rate * layer%getDWeights()
            bias_update = - self%learning_rate * layer%getDBiases()
        end if 

        call layer%setWeights(layer%getWeights() + weight_update)
        call layer%setBiases(layer%getBiases() + bias_update)
    end subroutine update_parameters


end module cls_SgdOptimizer