module conv1d_m

    use iso_fortran_env
    implicit none

    private
    public :: conv1d_core, conv1d_simd, conv1d_base_t, conv1d_t, conv1d_pad_t

    ! procedures

    interface
        module subroutine conv1d_core(x, k, y)
            real(real32), intent(in), contiguous :: x(:), k(:)
            real(real32), intent(out), contiguous :: y(:)
        end subroutine

        module subroutine conv1d_simd(x, k, y)
            real(real32), intent(in), contiguous, target :: x(:), k(:)
            real(real32), intent(out), contiguous, target :: y(:)
        end subroutine
    end interface

    ! base class

    type, abstract :: conv1d_base_t
        real(real32), allocatable :: kernel(:)
    contains
        procedure(conv_proto), deferred :: conv
        procedure(set_kernel_proto), deferred :: set_kernel
        procedure(output_shape_proto), deferred :: output_shape
    end type

    abstract interface
        subroutine conv_proto(self, x, y)
            import :: conv1d_base_t, real32
            class(conv1d_base_t), intent(in) :: self
            real(real32), intent(in), contiguous :: x(:)
            real(real32), intent(out), contiguous :: y(:)
        end subroutine

        subroutine set_kernel_proto(self, k)
            import :: conv1d_base_t, real32
            class(conv1d_base_t), intent(inout) :: self
            real(real32), intent(in) :: k(:)
        end subroutine

        function output_shape_proto(self, input_shape) result(output_shape)
            import :: conv1d_base_t
            class(conv1d_base_t), intent(in) :: self
            integer, intent(in) :: input_shape
            integer :: output_shape
        end function
    end interface

    ! simple convolution

    type, extends(conv1d_base_t) :: conv1d_t
    contains
        procedure :: conv => conv1d_conv
        procedure :: set_kernel => conv1d_set_kernel
        procedure :: output_shape => conv1d_output_shape
    end type

    interface
        module subroutine conv1d_set_kernel(self, k)
            class(conv1d_t), intent(inout) :: self
            real(real32), intent(in) :: k(:)
        end subroutine

        module function conv1d_output_shape(self, input_shape) result(output_shape)
            class(conv1d_t), intent(in) :: self
            integer, intent(in) :: input_shape
            integer :: output_shape
        end function

        module subroutine conv1d_conv(self, x, y)
            class(conv1d_t), intent(in) :: self
            real(real32), intent(in), contiguous :: x(:)
            real(real32), intent(out), contiguous :: y(:)
        end subroutine
    end interface

    ! padded convolution

    type, extends(conv1d_base_t) :: conv1d_pad_t
        integer :: pad_modulo = -1
        logical :: trim_pad = .false.
        logical :: use_simd = .false.
        integer, private :: padding = 0, kernel_size = 0
    contains
        procedure :: conv => conv1d_pad_conv
        procedure :: set_kernel => conv1d_pad_set_kernel
        procedure :: output_shape => conv1d_pad_output_shape
    end type

    interface
        module subroutine conv1d_pad_set_kernel(self, k)
            class(conv1d_pad_t), intent(inout) :: self
            real(real32), intent(in) :: k(:)
        end subroutine

        module function conv1d_pad_output_shape(self, input_shape) result(output_shape)
            class(conv1d_pad_t), intent(in) :: self
            integer, intent(in) :: input_shape
            integer :: output_shape
        end function

        module subroutine conv1d_pad_conv(self, x, y)
            class(conv1d_pad_t), intent(in) :: self

            real(real32), intent(in), contiguous :: x(:)
            real(real32), intent(out), contiguous :: y(:)
        end subroutine
    end interface


end module
