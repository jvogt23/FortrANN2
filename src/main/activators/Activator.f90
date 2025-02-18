! --- Activator --- !
! @author jvogt23
! @date 2025-1-1
! Interface for an Activator.
! Contains template for any class that represents a type of activation
! function.

module cls_Activator
    use cls_DenseLayer, only: DenseLayer
    implicit none

    type, abstract, public :: Activator
        real(kind=8), dimension(:,:), allocatable, private :: inputs
        real(kind=8), dimension(:,:), allocatable, private :: outputs
        ! DInputs produced by a backward pass
        real(kind=8), dimension(:,:), allocatable, private :: dInputs
    contains
        procedure(forward), public, deferred :: forward
        procedure(backward), public, deferred :: backward
        procedure, public :: getInputs
        procedure, public :: getOutputs
        procedure, public :: getDInputs
        procedure, public :: setInputs
        procedure, public :: setOutputs
        procedure, public :: setDInputs
    end type Activator

    abstract interface
        subroutine forward(self, layer)
            import :: Activator
            import :: DenseLayer
            class(Activator), intent(inout) :: self
            class(DenseLayer), intent(in) :: layer
        end subroutine forward

        subroutine backward(self, dvalues)
            import :: Activator
            class(Activator), intent(inout) :: self
            real(kind=8), dimension(:,:), intent(in) :: dvalues
        end subroutine backward
    end interface

contains
    
    function getInputs(self) result (inputs)
        class(Activator), intent(in) :: self
        real(kind=8), dimension(:,:), allocatable :: inputs

        if(.not.allocated(self%inputs)) then
            allocate(inputs(0,0))
        else
            allocate(inputs(size(self%inputs, 1), size(self%inputs, 2)))
            inputs = self%inputs
        end if
    end function getInputs

    subroutine setInputs(self, inputs)
        class(Activator), intent(inout) :: self
        real(kind=8), dimension(:,:), intent(in) :: inputs

        self%inputs = inputs
    end subroutine setInputs

    function getOutputs(self) result (outputs)
        class(Activator), intent(in) :: self
        real(kind=8), dimension(:,:), allocatable :: outputs

        if(.not.allocated(self%outputs)) then
            allocate(outputs(0,0))
        else
            allocate(outputs(size(self%outputs, 1), size(self%outputs, 2)))
            outputs = self%outputs
        end if
    end function getOutputs

    subroutine setOutputs(self, outputs)
        class(Activator), intent(inout) :: self
        real(kind=8), dimension(:,:), intent(in) :: outputs

        self%outputs = outputs
    end subroutine setOutputs

    function getDInputs(self) result (dInputs)
        class(Activator), intent(in) :: self
        real(kind=8), dimension(:,:), allocatable :: dInputs

        if(.not.allocated(self%dInputs)) then
            allocate(dInputs(0,0))
        else
            allocate(dInputs(size(self%dInputs, 1),  & 
                size(self%dInputs, 2)))
            dInputs = self%dInputs
        end if
    end function getDInputs

    subroutine setDInputs(self, dInputs)
        class(Activator), intent(inout) :: self
        real(kind=8), dimension(:,:), intent(in) :: dInputs

        self%dInputs = dInputs
    end subroutine setDInputs

end module cls_Activator