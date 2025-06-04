module cls_AdaptiveGradientOptimizer
    use cls_DenseLayer, only: DenseLayer
    implicit none
    ! Adaptive gradient means that this optimizer will change the learning rate
    ! for each parameter. This optimizer does not have a momentum and instead
    ! updates the parameters based on a cache of gradients.
    type AdaptiveGradientOptimizer
        integer, private :: iterations
        real(kind=8), private :: learning_rate, epsilon
        real(kind=8), dimension(:,:), ALLOCATABLE :: weight_cache
        real(kind=8), dimension(:), ALLOCATABLE :: bias_cache 
    contains
        procedure, public :: init
        procedure, public :: update_params
    end type AdaptiveGradientOptimizer

contains

    subroutine init(self, learning_rate, epsilon)
        class(AdaptiveGradientOptimizer), intent(inout) :: self
        real(kind=8), intent(in) :: learning_rate
        real(kind=8), intent(in), optional :: epsilon

        self%learning_rate = learning_rate
        if (present(epsilon)) then
            self%epsilon = epsilon
        else 
            self%epsilon = 1.0e-7
        end if
        self%iterations = 0
    end subroutine init

    subroutine update_params(self, layer)
        class(AdaptiveGradientOptimizer), intent(inout) :: self
        class(DenseLayer), intent(inout) :: layer
        real(kind=8), DIMENSION(:, :), ALLOCATABLE :: temp_weights, temp_wcache
        real(kind=8), dimension(:,:), ALLOCATABLE :: temp_dweights
        real(kind=8), dimension(:), ALLOCATABLE :: temp_biases, temp_bcache
        real(kind=8), dimension(:), ALLOCATABLE :: temp_dbiases

        temp_weights = layer%getWeights()
        temp_biases = layer%getBiases()
        temp_dweights = layer%getDWeights()
        temp_dbiases = layer%getDBiases()

        if(.not.allocated(self%weight_cache)) then
            allocate(self%weight_cache(size(temp_dweights,1), &
                size(temp_dweights,2)))
            allocate(self%bias_cache(size(temp_dbiases)))
            self%weight_cache = 0
            self%bias_cache = 0
        end if

        temp_wcache = self%weight_cache + (temp_dweights * temp_dweights)
        temp_bcache = self%bias_cache + (temp_dbiases * temp_dbiases)

        temp_weights = temp_weights - &
            ((learning_rate / sqrt(temp_wcache + self%epsilon)) * temp_dweights)
        temp_biases = temp_biases - &
            ((learning_rate / sqrt(temp_bcache + self%epsilon)) * temp_dbiases)

        call layer%setWeights(temp_weights)
        call layer%setBiases(temp_biases)

        self%weight_cache = temp_wcache
        self%bias_cache = temp_bcache
        
        self%iterations = self%iterations + 1
    end subroutine update_params

end module cls_AdaptiveGradientOptimizer