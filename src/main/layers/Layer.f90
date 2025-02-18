! --- Layer class --- !
! @author jvogt23
! @date 2024-12-31
! Super-class for any type of ANN layer. Bound to interface containing
! a forward and backward method for passing data through the layer.

module cls_Layer
    implicit none

    type, abstract, public :: Layer

    contains

        procedure(init), public, deferred :: init
        procedure(forward), public, deferred :: forward
        procedure(backward), public, deferred :: backward

    end type Layer

    abstract interface
        subroutine init(self, nInputs, nNeurons)
            import :: Layer
            class(Layer), intent(inout) :: self
            integer, intent(in) :: nInputs, nNeurons
        end subroutine init

        subroutine forward(self, input)
            import :: Layer
            class(Layer), intent(inout) :: self
            real(kind=8), dimension(:,:), intent(in) :: input
        end subroutine forward

        subroutine backward(self, dActivator)
            import :: Layer
            class(Layer), intent(inout) :: self
            real(kind=8), dimension(:,:), intent(in) :: dActivator
        end subroutine backward
    end interface

contains


end module cls_Layer