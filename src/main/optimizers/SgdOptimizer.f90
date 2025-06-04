module cls_SgdOptimizer
    use cls_DenseLayer, only: DenseLayer
    implicit none
! TODO: Make Learning rate and decay getter, setter
    type, public :: SgdOptimizer
        integer, private :: iterations
        real(kind=8), private :: momentum, learning_rate, decay_rate
        real(kind=8), dimension(:,:), allocatable, private :: weight_momenta
        real(kind=8), dimension(:), allocatable, private :: bias_momenta
    contains
        procedure, public :: init
        procedure, public :: get_momentum
        procedure, public :: set_momentum
        procedure, public :: update_parameters
        procedure, public :: get_learningrate
        procedure, public :: set_learningrate
        procedure, public :: get_decayrate
        procedure, public :: set_decayrate
        procedure, private :: apply_decay
    end type SgdOptimizer

contains

    subroutine init(self, learning_rate, momentum, decay_rate, weight_rows, &
        weight_cols)
        class(SgdOptimizer), INTENT(INOUT) :: self
        real(kind=8), intent(in) :: learning_rate, decay_rate, momentum
        integer, intent(in) :: weight_rows, weight_cols
        self%momentum = momentum
        self%learning_rate = learning_rate
        self%decay_rate = decay_rate
        self%iterations = 0
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

    function get_learningrate(self) result(learning_rate)
        class(SgdOptimizer), intent(in) :: self
        real(kind=8) :: learning_rate
        learning_rate = self%learning_rate
    end function get_learningrate

    subroutine set_learningrate(self, learning_rate)
        class(SgdOptimizer), intent(in) :: self
        real(kind=8), intent(in) :: learning_rate
        if (learning_rate > 0.0) then
            self%learning_rate = learning_rate
        else 
            print *, "Cannot set negative learning rate."
            STOP
        end if
    end subroutine set_learningrate

    function get_decayrate(self) result(decay_rate)
        class(SgdOptimizer), intent(in) :: self
        real(kind=8) :: decay_rate
        decay_rate = self%decay_rate
    end function get_decayrate

    subroutine set_decayrate(self, decay_rate)
        class(SgdOptimizer), intent(in) :: self
        real(kind=8), intent(in) :: decay_rate
        if (decay_rate > 0.0) then
            self%decay_rate = decay_rate
        else 
            print *, "Cannot set negative decay rate."
            STOP
        end if
    end subroutine set_decayrate

    subroutine update_parameters(self, layer)
        class(SgdOptimizer), intent(inout) :: self
        class(DenseLayer), intent(inout) :: layer
        real(kind=8), dimension(:,:), allocatable :: weight_update, bias_update

        call self%apply_decay()
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
        self%iterations = self%iterations + 1
    end subroutine update_parameters

    subroutine apply_decay(self)
        class(SgdOptimizer), intent(inout) :: self

        if (self%decay_rate.not..eq.0.0) then
            self%learning_rate = self%learning rate * &
            (1 / (1 + (self%iterations * self%decay_rate)))
        end if        
    end subroutine apply_decay
end module cls_SgdOptimizer