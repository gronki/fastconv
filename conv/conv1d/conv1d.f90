module conv1d_m

    use iso_fortran_env
    use conv_base_m
    implicit none (type, external)

    private

    ! procedures

    interface
        module subroutine conv1d_core(x, k, y)
            real(real32), intent(in), contiguous :: x(:), k(:)
            real(real32), intent(inout), contiguous :: y(:)
        end subroutine

        module subroutine conv1d_simd(x, k, y)
            real(real32), intent(in), contiguous, target :: x(:), k(:)
            real(real32), intent(out), contiguous, target :: y(:)
        end subroutine
    end interface

    public :: conv1d_core, conv1d_simd

    ! base class

    type, extends(conv_base_t), abstract :: conv1d_base_t
        real(real32), allocatable :: kernel(:)
    contains
        procedure(set_kernel_proto), deferred :: set_kernel
        procedure(output_shape_proto), deferred :: output_shape
        procedure(conv_proto), deferred :: conv
    end type

    abstract interface
        subroutine conv_proto(self, x, y)
            import :: conv1d_base_t, real32
            class(conv1d_base_t), intent(in) :: self
            real(real32), intent(in), contiguous :: x(:)
            real(real32), intent(inout), contiguous :: y(:)
        end subroutine

        subroutine set_kernel_proto(self, k)
            import :: conv1d_base_t, real32
            class(conv1d_base_t), intent(inout) :: self
            real(real32), intent(in) :: k(:)
        end subroutine

        pure function output_shape_proto(self, input_shape) result(output_shape)
            import :: conv1d_base_t, size_k
            class(conv1d_base_t), intent(in) :: self
            integer(kind=size_k), intent(in) :: input_shape
            integer(kind=size_k) :: output_shape
        end function
    end interface

    public :: conv1d_base_t

    ! reference implementation

    type, extends(conv1d_base_t) :: conv1d_ref_t
    contains
        procedure :: set_kernel => conv1d_ref_set_kernel
        procedure :: output_shape => conv1d_ref_output_shape
        procedure :: conv => conv1d_ref_conv
    end type

    interface
        module subroutine conv1d_ref_set_kernel(self, k)
            class(conv1d_ref_t), intent(inout) :: self
            real(real32), intent(in) :: k(:)
        end subroutine

        pure module function conv1d_ref_output_shape(self, input_shape) result(output_shape)
            class(conv1d_ref_t), intent(in) :: self
            integer(kind=size_k), intent(in) :: input_shape
            integer(kind=size_k) :: output_shape
        end function

        module subroutine conv1d_ref_conv(self, x, y)
            class(conv1d_ref_t), intent(in) :: self
            real(real32), intent(in), contiguous :: x(:)
            real(real32), intent(inout), contiguous :: y(:)
        end subroutine
    end interface

    public :: conv1d_ref_t

    ! padded convolution - optimized implementation

    type, extends(conv1d_base_t) :: conv1d_pad_t
        integer(kind=size_k) :: pad_modulo = 4
        logical :: use_simd = .false.
        integer(kind=size_k), private :: padding = 0, kernel_shape = 0
    contains
        procedure :: set_kernel => conv1d_pad_set_kernel
        procedure :: output_shape => conv1d_pad_output_shape
        procedure :: conv => conv1d_pad_conv
    end type

    interface
        module subroutine conv1d_pad_set_kernel(self, k)
            class(conv1d_pad_t), intent(inout) :: self
            real(real32), intent(in) :: k(:)
        end subroutine

        pure module function conv1d_pad_output_shape(self, input_shape) result(output_shape)
            class(conv1d_pad_t), intent(in) :: self
            integer(kind=size_k), intent(in) :: input_shape
            integer(kind=size_k) :: output_shape
        end function

        module subroutine conv1d_pad_conv(self, x, y)
            class(conv1d_pad_t), intent(in) :: self
            real(real32), intent(in), contiguous :: x(:)
            real(real32), intent(inout), contiguous :: y(:)
        end subroutine
    end interface

    public :: conv1d_pad_t

    ! default convolution

    type, extends(conv1d_pad_t) :: conv1d_t
    end type

    public :: conv1d_t

end module
