module conv2d_m

    use iso_fortran_env
    use conv_base_m
    use conv1d_m, only: conv1d_base_t

    implicit none (type, external)
    private

    ! base classes

    type, extends(conv_base_t), abstract :: conv2d_base_t
    contains
        procedure(set_kernel_proto), deferred :: set_kernel
        procedure(output_shape_proto), deferred :: output_shape
        procedure(conv_proto), deferred :: conv
    end type

    abstract interface
        subroutine set_kernel_proto(self, k)
            import :: conv2d_base_t, real32
            class(conv2d_base_t), intent(inout) :: self
            real(real32), intent(in) :: k(:,:)
        end subroutine

        function output_shape_proto(self, input_shape) result(output_shape)
            import :: conv2d_base_t, size_k
            class(conv2d_base_t), intent(in) :: self
            integer(kind=size_k), intent(in) :: input_shape(2)
            integer(kind=size_k) :: output_shape(2)
        end function

        subroutine conv_proto(self, x, y)
            import :: conv2d_base_t, real32
            class(conv2d_base_t), intent(inout) :: self
            real(real32), intent(in), contiguous :: x(:,:)
            real(real32), intent(inout), contiguous :: y(:,:)
        end subroutine
    end interface


    public :: conv2d_base_t

    ! reference implementation

    type, extends(conv2d_base_t) :: conv2d_ref_t
        real(real32), allocatable, private :: kernel(:,:)
    contains
        procedure :: set_kernel => conv2d_ref_set_kernel
        procedure :: output_shape => conv2d_ref_output_shape
        procedure :: conv => conv2d_ref_conv
    end type

    interface
        module function conv2d_ref_output_shape(self, input_shape) result(output_shape)
            class(conv2d_ref_t), intent(in) :: self
            integer(kind=size_k), intent(in) :: input_shape(2)
            integer(kind=size_k) :: output_shape(2)
        end function

        module subroutine conv2d_ref_conv(self, x, y)
            class(conv2d_ref_t), intent(inout) :: self
            real(real32), intent(in), contiguous :: x(:,:)
            real(real32), intent(inout), contiguous :: y(:,:)
        end subroutine

        module subroutine conv2d_ref_set_kernel(self, k)
            class(conv2d_ref_t), intent(inout) :: self
            real(real32), intent(in) :: k(:,:)
        end subroutine
    end interface

    public :: conv2d_ref_t

    ! line-by-line convolution - optimized implementation

    type, extends(conv2d_base_t) :: conv2d_line_t
        class(conv1d_base_t), allocatable :: conv1d_mold
        class(conv1d_base_t), allocatable, private :: kernels_1d(:)
        integer(kind=size_k), private :: kernel_shape(2) = -1
    contains
        procedure :: set_kernel => conv2d_line_set_kernel
        procedure :: output_shape => conv2d_line_output_shape
        procedure :: conv => conv2d_line_conv
    end type

    interface conv2d_line_t
        module function conv2d_line_t_ctor(conv1d_mold) result(self)
            class(conv1d_base_t), intent(in), optional :: conv1d_mold
            type(conv2d_line_t) :: self
        end function
    end interface

    interface
        module subroutine conv2d_line_set_kernel(self, k)
            class(conv2d_line_t), intent(inout) :: self
            real(real32), intent(in) :: k(:,:)
        end subroutine

        module function conv2d_line_output_shape(self, input_shape) result(output_shape)
            class(conv2d_line_t), intent(in) :: self
            integer(kind=size_k), intent(in) :: input_shape(2)
            integer(kind=size_k) :: output_shape(2)
        end function

        module subroutine conv2d_line_conv(self, x, y)
            class(conv2d_line_t), intent(inout) :: self
            real(real32), intent(in), contiguous :: x(:,:)
            real(real32), intent(inout), contiguous :: y(:,:)
        end subroutine
    end interface

    public :: conv2d_line_t

    ! default conv2d

    type, extends(conv2d_line_t) :: conv2d_t
    end type

    public :: conv2d_t

end module
