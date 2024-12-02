module conv1d_m

    use iso_fortran_env, only: real_k=>real32, size_k=>int64
    implicit none (type, external)

    private

    ! procedures

    interface

        pure module subroutine conv1d_core(x, k, y)
            real(real_k), intent(in), contiguous :: x(:), k(:)
            real(real_k), intent(out), contiguous :: y(:)
        end subroutine

        pure module subroutine conv1d_pad_core(x, k, kernel_size, y)
            real(real_k), intent(in), contiguous :: x(:), k(:)
            integer(size_k), intent(in) :: kernel_size
            real(real_k), intent(out), contiguous :: y(:)
        end subroutine

        elemental module function padded_dimension(kernel_size, pad_modulo)
            integer(kind=size_k), intent(in) :: kernel_size, pad_modulo
            integer(kind=size_k) :: padded_dimension
        end function
        
        pure module function padded_1d_kernel(k, pad_modulo) result(padded_kernel)
            real(real_k), intent(in) :: k(:)
            integer(kind=size_k), intent(in) :: pad_modulo
            real(kind=real_k), allocatable :: padded_kernel(:)
        end function

    end interface

    public :: conv1d_core, conv1d_pad_core, padded_1d_kernel, padded_dimension

    ! base class

    type, abstract :: conv1d_base_t
        logical :: preserve_shape = .false.    
    contains
        procedure(set_kernel_proto), deferred :: set_kernel
        procedure(output_size_proto), deferred :: output_size
        procedure(conv_proto), deferred :: conv
        procedure, non_overridable :: apply
    end type

    abstract interface
        pure subroutine conv_proto(self, x, y)
            import :: conv1d_base_t, real_k
            class(conv1d_base_t), intent(in) :: self
            real(real_k), intent(in), contiguous :: x(:)
            real(real_k), intent(inout), contiguous :: y(:)
        end subroutine

        pure subroutine set_kernel_proto(self, k)
            import :: conv1d_base_t, real_k
            class(conv1d_base_t), intent(inout) :: self
            real(real_k), intent(in) :: k(:)
        end subroutine

        pure function output_size_proto(self, input_size) result(output_size)
            import :: conv1d_base_t, size_k
            class(conv1d_base_t), intent(in) :: self
            integer(kind=size_k), intent(in) :: input_size
            integer(kind=size_k) :: output_size
        end function
    end interface

    public :: conv1d_base_t

    ! reference implementation

    type, extends(conv1d_base_t) :: conv1d_ref_t
        real(real_k), allocatable, private :: kernel(:)
    contains
        procedure :: set_kernel => conv1d_ref_set_kernel
        procedure :: output_size => conv1d_ref_output_size
        procedure :: conv => conv1d_ref_conv
    end type

    interface
        pure module subroutine conv1d_ref_set_kernel(self, k)
            class(conv1d_ref_t), intent(inout) :: self
            real(real_k), intent(in) :: k(:)
        end subroutine

        pure module function conv1d_ref_output_size(self, input_size) result(output_size)
            class(conv1d_ref_t), intent(in) :: self
            integer(kind=size_k), intent(in) :: input_size
            integer(kind=size_k) :: output_size
        end function

        pure module subroutine conv1d_ref_conv(self, x, y)
            class(conv1d_ref_t), intent(in) :: self
            real(real_k), intent(in), contiguous :: x(:)
            real(real_k), intent(inout), contiguous :: y(:)
        end subroutine
    end interface

    public :: conv1d_ref_t

    ! padded convolution - optimized implementation

    type, extends(conv1d_base_t) :: conv1d_pad_t
        integer(kind=size_k) :: pad_modulo = 4
        real(real_k), allocatable, private :: kernel(:)
        integer(kind=size_k), private :: kernel_size = 0
    contains
        procedure :: set_kernel => conv1d_pad_set_kernel
        procedure :: output_size => conv1d_pad_output_size
        procedure :: conv => conv1d_pad_conv
    end type

    interface
        pure module subroutine conv1d_pad_set_kernel(self, k)
            class(conv1d_pad_t), intent(inout) :: self
            real(real_k), intent(in) :: k(:)
        end subroutine

        pure module function conv1d_pad_output_size(self, input_size) result(output_size)
            class(conv1d_pad_t), intent(in) :: self
            integer(kind=size_k), intent(in) :: input_size
            integer(kind=size_k) :: output_size
        end function

        pure module subroutine conv1d_pad_conv(self, x, y)
            class(conv1d_pad_t), intent(in) :: self
            real(real_k), intent(in), contiguous :: x(:)
            real(real_k), intent(inout), contiguous :: y(:)
        end subroutine
    end interface

    public :: conv1d_pad_t

    ! default convolution

    type, extends(conv1d_pad_t) :: conv1d_t
    end type

    public :: conv1d_t

contains

    pure function apply(self, x) result(y)
        class(conv1d_base_t), intent(in) :: self
        real(real_k), intent(in), contiguous :: x(:)
        real(real_k), allocatable :: y(:)
        integer(kind=size_k) :: output_size

        output_size = self % output_size(size(x, kind=size_k))
        allocate(y(output_size))

        call self % conv(x, y)

    end function

end module
