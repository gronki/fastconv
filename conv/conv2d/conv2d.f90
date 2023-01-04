module conv2d_m

    use iso_fortran_env
    use conv1d_m, only: conv1d_pad_t
    implicit none
    private

    interface
        module subroutine conv2d_core(x, k, y)
            real(real32), contiguous, intent(in) :: x(:,:), k(:,:)
            real(real32), contiguous, intent(out) :: y(:,:)
        end subroutine

        module subroutine conv2d_simd(x, k, y)
            real(real32), contiguous, intent(in) :: x(:,:), k(:,:)
            real(real32), contiguous, intent(out) :: y(:,:)
        end subroutine
    end interface

    ! base classes

    type, abstract :: conv2d_base_t
    contains
        procedure(conv_proto), deferred :: conv
        procedure(set_kernel_proto), deferred :: set_kernel
        procedure(output_shape_proto), deferred :: output_shape
    end type

    abstract interface
        subroutine conv_proto(self, x, y)
            import :: conv2d_base_t, real32
            class(conv2d_base_t), intent(inout) :: self
            real(real32), intent(in), contiguous :: x(:,:)
            real(real32), intent(out), contiguous :: y(:,:)
        end subroutine

        subroutine set_kernel_proto(self, k)
            import :: conv2d_base_t, real32
            class(conv2d_base_t), intent(inout) :: self
            real(real32), intent(in) :: k(:,:)
        end subroutine

        function output_shape_proto(self, input_shape) result(output_shape)
            import :: conv2d_base_t
            class(conv2d_base_t), intent(in) :: self
            integer, intent(in) :: input_shape(2)
            integer :: output_shape(2)
        end function
    end interface


    type, extends(conv2d_base_t), abstract :: conv2d_pad_base_t
        integer :: pad_modulo = -1
        logical :: trim_pad = .false.
        logical :: use_simd = .false.
    end type

    public :: conv2d_base_t, conv2d_pad_base_t

    ! basic convolution

    type, extends(conv2d_base_t) :: conv2d_t
        logical :: row_by_row = .true.
        real(real32), allocatable, private :: kernel(:,:)
    contains
        procedure :: conv => conv2d_conv
        procedure :: set_kernel => conv2d_set_kernel
        procedure :: output_shape => conv2d_output_shape
    end type

    interface
        module subroutine conv2d_set_kernel(self, k)
            class(conv2d_t), intent(inout) :: self
            real(real32), intent(in) :: k(:,:)
        end subroutine

        module function conv2d_output_shape(self, input_shape) result(output_shape)
            class(conv2d_t), intent(in) :: self
            integer, intent(in) :: input_shape(2)
            integer :: output_shape(2)
        end function

        module subroutine conv2d_conv(self, x, y)
            class(conv2d_t), intent(inout) :: self
            real(real32), intent(in), contiguous :: x(:,:)
            real(real32), intent(out), contiguous :: y(:,:)
        end subroutine
    end interface

    public :: conv2d_t, conv2d_core

    ! padded 2d convolution

    type, extends(conv2d_pad_base_t) :: conv2d_pad_t
        integer, private :: padding = 0, kernel_shape(2) = -1
        real(real32), allocatable, private :: kernel(:,:), kernel_pad(:,:), xpad(:,:)
    contains
        procedure :: conv => conv2d_pad_conv
        procedure :: set_kernel => conv2d_pad_set_kernel
        procedure :: output_shape => conv2d_pad_output_shape
    end type

    interface
        module subroutine conv2d_pad_set_kernel(self, k)
            class(conv2d_pad_t), intent(inout) :: self
            real(real32), intent(in) :: k(:,:)
        end subroutine

        module function conv2d_pad_output_shape(self, input_shape) result(output_shape)
            class(conv2d_pad_t), intent(in) :: self
            integer, intent(in) :: input_shape(2)
            integer :: output_shape(2)
        end function

        module subroutine conv2d_pad_conv(self, x, y)
            class(conv2d_pad_t), intent(inout) :: self
            real(real32), intent(in), contiguous :: x(:,:)
            real(real32), intent(out), contiguous :: y(:,:)
        end subroutine
    end interface

    public :: conv2d_pad_t

    ! line-by-line convolution

    type, extends(conv2d_pad_base_t) :: conv2d_line_t
        type(conv1d_pad_t), allocatable, private :: kernels_1d(:)
        integer, private :: kernel_shape(2) = -1
    contains
        procedure :: conv => conv2d_line_conv
        procedure :: set_kernel => conv2d_line_set_kernel
        procedure :: output_shape => conv2d_line_output_shape
    end type

    interface
        module subroutine conv2d_line_set_kernel(self, k)
            class(conv2d_line_t), intent(inout) :: self
            real(real32), intent(in) :: k(:,:)
        end subroutine

        module function conv2d_line_output_shape(self, input_shape) result(output_shape)
            class(conv2d_line_t), intent(in) :: self
            integer, intent(in) :: input_shape(2)
            integer :: output_shape(2)
        end function

        module subroutine conv2d_line_conv(self, x, y)
            class(conv2d_line_t), intent(inout) :: self
            real(real32), intent(in), contiguous :: x(:,:)
            real(real32), intent(out), contiguous :: y(:,:)
        end subroutine
    end interface

    public :: conv2d_line_t

end module
