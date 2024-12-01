module conv2d_m

    use iso_fortran_env, only: real_k=>real32, size_k=>int64
    use conv1d_m, only: conv1d_base_t

    implicit none (type, external)
    private

    ! base classes

    type, abstract :: conv2d_base_t
        logical :: preserve_shape = .false.
    contains
        procedure(set_kernel_proto), deferred :: set_kernel
        procedure(kernel_shape_proto), deferred :: kernel_shape
        procedure :: output_shape
        procedure(conv_proto), deferred :: conv
        procedure :: apply
    end type

    abstract interface
        subroutine set_kernel_proto(self, k)
            import :: conv2d_base_t, real_k
            class(conv2d_base_t), intent(inout) :: self
            real(real_k), intent(in) :: k(:,:)
        end subroutine

        pure function kernel_shape_proto(self) result(kernel_shape)
            import :: conv2d_base_t, size_k
            class(conv2d_base_t), intent(in) :: self
            integer(kind=size_k) :: kernel_shape(2)
        end function

        subroutine conv_proto(self, x, y)
            import :: conv2d_base_t, real_k
            class(conv2d_base_t), intent(in) :: self
            real(real_k), intent(in), contiguous :: x(:,:)
            real(real_k), intent(inout), contiguous :: y(:,:)
        end subroutine
    end interface

    interface
        pure module function output_shape(self, input_shape)
            class(conv2d_base_t), intent(in) :: self
            integer(kind=size_k), intent(in) :: input_shape(2)
            integer(kind=size_k) :: output_shape(2)
        end function

        module function apply(self, x) result(y)
            class(conv2d_base_t), intent(in) :: self
            real(real_k), intent(in), contiguous :: x(:,:)
            real(real_k), allocatable :: y(:,:)
        end function
    end interface

    public :: conv2d_base_t

    ! reference implementation

    type, extends(conv2d_base_t) :: conv2d_ref_t
        real(real_k), allocatable, private :: kernel(:,:)
    contains
        procedure :: set_kernel => conv2d_ref_set_kernel
        procedure :: kernel_shape => conv2d_ref_kernel_shape
        procedure :: conv => conv2d_ref_conv
    end type

    interface
        module subroutine conv2d_ref_set_kernel(self, k)
            class(conv2d_ref_t), intent(inout) :: self
            real(real_k), intent(in) :: k(:,:)
        end subroutine

        pure module function conv2d_ref_kernel_shape(self) result(kernel_shape)
            class(conv2d_ref_t), intent(in) :: self
            integer(kind=size_k) :: kernel_shape(2)
        end function

        module subroutine conv2d_ref_conv(self, x, y)
            class(conv2d_ref_t), intent(in) :: self
            real(real_k), intent(in), contiguous :: x(:,:)
            real(real_k), intent(inout), contiguous :: y(:,:)
        end subroutine
    end interface

    public :: conv2d_ref_t

    ! line-by-line convolution - optimized implementation

    type, extends(conv2d_base_t) :: conv2d_line_t
        class(conv1d_base_t), allocatable :: conv1d_mold
        class(conv1d_base_t), allocatable, private :: kernels_1d(:)
        integer(kind=size_k), private :: kernel_shape_(2) = 0
    contains
        procedure :: set_kernel => conv2d_line_set_kernel
        procedure :: kernel_shape => conv2d_line_kernel_shape
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
            real(real_k), intent(in) :: k(:,:)
        end subroutine

        pure module function conv2d_line_kernel_shape(self) result(kernel_shape)
            class(conv2d_line_t), intent(in) :: self
            integer(kind=size_k) :: kernel_shape(2)
        end function

        module subroutine conv2d_line_conv(self, x, y)
            class(conv2d_line_t), intent(in) :: self
            real(real_k), intent(in), contiguous :: x(:,:)
            real(real_k), intent(inout), contiguous :: y(:,:)
        end subroutine
    end interface

    public :: conv2d_line_t

    ! default conv2d

    type, extends(conv2d_line_t) :: conv2d_t
    end type

    public :: conv2d_t

end module
