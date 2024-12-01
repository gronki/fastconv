module conv3d_m

    use iso_fortran_env, only: real_k=>real32, size_k=>int64
    use conv1d_m, only: conv1d_base_t

    implicit none (type, external)
    private

    ! base classes

    type, abstract :: conv3d_base_t
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
            import :: conv3d_base_t, real_k
            class(conv3d_base_t), intent(inout) :: self
            real(real_k), intent(in) :: k(:,:,:)
        end subroutine

        pure function kernel_shape_proto(self) result(kernel_shape)
            import :: conv3d_base_t, size_k
            class(conv3d_base_t), intent(in) :: self
            integer(kind=size_k) :: kernel_shape(3)
        end function

        subroutine conv_proto(self, x, y)
            import :: conv3d_base_t, real_k
            class(conv3d_base_t), intent(in) :: self
            real(real_k), intent(in), contiguous :: x(:,:,:)
            real(real_k), intent(inout), contiguous :: y(:,:,:)
        end subroutine
    end interface

    interface
        pure module function output_shape(self, input_shape)
            class(conv3d_base_t), intent(in) :: self
            integer(kind=size_k), intent(in) :: input_shape(3)
            integer(kind=size_k) :: output_shape(3)
        end function

        module function apply(self, x) result(y)
            class(conv3d_base_t), intent(in) :: self
            real(real_k), intent(in), contiguous :: x(:,:,:)
            real(real_k), allocatable :: y(:,:,:)
        end function
    end interface

    public :: conv3d_base_t

    ! reference implementation

    type, extends(conv3d_base_t) :: conv3d_ref_t
        real(real_k), allocatable, private :: kernel(:,:,:)
    contains
        procedure :: set_kernel => conv3d_ref_set_kernel
        procedure :: kernel_shape => conv3d_ref_kernel_shape
        procedure :: conv => conv3d_ref_conv
    end type

    interface
        module subroutine conv3d_ref_set_kernel(self, k)
            class(conv3d_ref_t), intent(inout) :: self
            real(real_k), intent(in) :: k(:,:,:)
        end subroutine

        pure module function conv3d_ref_kernel_shape(self) result(kernel_shape)
            class(conv3d_ref_t), intent(in) :: self
            integer(kind=size_k) :: kernel_shape(3)
        end function

        module subroutine conv3d_ref_conv(self, x, y)
            class(conv3d_ref_t), intent(in) :: self
            real(real_k), intent(in), contiguous :: x(:,:,:)
            real(real_k), intent(inout), contiguous :: y(:,:,:)
        end subroutine
    end interface

    public :: conv3d_ref_t

    ! line-by-line convolution - optimized implementation

    type, extends(conv3d_base_t) :: conv3d_line_t
        class(conv1d_base_t), allocatable :: conv1d_mold
        class(conv1d_base_t), allocatable, private :: kernels_1d(:,:)
        integer(kind=size_k), private :: kernel_shape_(3) = 0
    contains
        procedure :: set_kernel => conv3d_line_set_kernel
        procedure :: kernel_shape => conv3d_line_kernel_shape
        procedure :: conv => conv3d_line_conv
    end type

    interface conv3d_line_t
        module function conv3d_line_t_ctor(conv1d_mold) result(self)
            class(conv1d_base_t), intent(in), optional :: conv1d_mold
            type(conv3d_line_t) :: self
        end function
    end interface

    interface
        module subroutine conv3d_line_set_kernel(self, k)
            class(conv3d_line_t), intent(inout) :: self
            real(real_k), intent(in) :: k(:,:,:)
        end subroutine

        pure module function conv3d_line_kernel_shape(self) result(kernel_shape)
            class(conv3d_line_t), intent(in) :: self
            integer(kind=size_k) :: kernel_shape(3)
        end function

        module subroutine conv3d_line_conv(self, x, y)
            class(conv3d_line_t), intent(in) :: self
            real(real_k), intent(in), contiguous :: x(:,:,:)
            real(real_k), intent(inout), contiguous :: y(:,:,:)
        end subroutine
    end interface

    public :: conv3d_line_t

    ! default conv3d

    type, extends(conv3d_line_t) :: conv3d_t
    end type

    public :: conv3d_t

end module
