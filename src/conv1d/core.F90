submodule (conv1d_m) c1d_core

    use iso_c_binding
    use iso_fortran_env, only: error_unit

    implicit none (type, external)

contains

    !> Compute convolution for kernel which is padded with zeros.
    !> This is good for performance reasons, as such kernels can
    !> be better vectorized into SIMD instructions.
    pure module subroutine conv1d_pad_core(x, k, kernel_size, y)
        !> Input vector. 
        real(real_k), intent(in), contiguous :: x(:)
        !> Reversed kernel padded with zeros.
        real(real_k), intent(in), contiguous :: k(:)
        !> Width of the padding. For zero this will be a normal convolution.
        integer(size_k), intent(in) :: kernel_size
        !> Output.
        real(real_k), intent(out), contiguous :: y(:)

        integer(size_k) :: input_size, padding, output_size_raw
        
        input_size = size(x, kind=size_k)
        padding = size(k, kind=size_k) - kernel_size
        output_size_raw = input_size - kernel_size + 1_size_k

#       ifndef NDEBUG
        if (padding < 0) error stop "conv1d_pad_core: padding parameter must be >= 0"
        if (size(y) /= output_size_raw) &
            error stop 'incorrect output shape for 1D convolution'
#       endif

        call conv1d_core(x, k, y(:output_size_raw - padding))

        if (padding > 0) then
            call conv1d_core(x(output_size_raw - padding + 1_size_k:), &
                k(:kernel_size), &
                y(output_size_raw - (padding - 1_size_k) : output_size_raw))
        end if

    end subroutine
    
    !> basic subroutine for 1D convolution, pure Fortran
    pure module subroutine conv1d_core(x, k, y)
        !> vector to be convolved
        real(real_k), intent(in), contiguous :: x(:)
        !> convolution kernel (should be reversed beforehand)
        real(real_k), intent(in), contiguous :: k(:)
        !> output vector, length size(x) + 1 - size(k)
        real(real_k), intent(out), contiguous :: y(:)
        
        integer(kind=size_k) :: input_size, kernel_size, output_size_raw

        input_size = size(x, kind=size_k)
        kernel_size = size(k, kind=size_k)
        output_size_raw = input_size - kernel_size + 1_size_k

#       ifndef NDEBUG
        if (size(y, kind=size_k) /= output_size_raw) &
            error stop 'incorrect output shape for 1D convolution'
#       endif

        if (modulo(kernel_size, 16) == 0) then
            call conv1d_k16(x, k, y)
        else if (modulo(kernel_size, 8) == 0) then
            call conv1d_k8(x, k, y)
        else if (modulo(kernel_size, 4) == 0) then
            call conv1d_k4(x, k, y)
        else if (kernel_size == 1) then
            y(:) = x * k(1)
        else
            call conv1d_general(x, k, y)
        end if

    end subroutine

    !> compute the convolution for any kernel length
    pure subroutine conv1d_general(x, k, y)
        !> vector to be convolved
        real(real_k), intent(in), contiguous :: x(:)
        !> convolution kernel (should be reversed beforehand)
        real(real_k), intent(in), contiguous :: k(:)
        !> output vector, length size(x) + 1 - size(k)
        real(real_k), intent(out), contiguous :: y(:)
        integer(kind=size_k) :: i, j, kernel_size, output_size
        real(real_k) :: total

        kernel_size = size(k, kind=size_k)
        output_size = size(x, kind=size_k) - kernel_size + 1_size_k

        do i = 1, output_size
            total = 0
            do j = 1, kernel_size
                total = total + k(j) * x(i + j - 1)
            end do
            y(i) = total
        end do

    end subroutine

    !> specific implementation for multiplies of 8
    pure subroutine conv1d_k16(x, k, y)
        !> vector to be convolved
        real(real_k), intent(in), contiguous :: x(:)
        !> convolution kernel (should be reversed beforehand)
        real(real_k), intent(in), contiguous :: k(:)
        !> output vector, length size(x) + 1 - size(k)
        real(real_k), intent(out), contiguous :: y(:)
        integer(kind=size_k) :: i, j, kernel_size_16, output_size
        real(real_k) :: total

#       ifndef NDEBUG
        if (modulo(size(k), 16) /= 0) error stop "size of kernel must be multiply of 16"
#       endif

        kernel_size_16 = size(k) / 16
        output_size = size(x) - 16 * kernel_size_16 + 1

        do i = 1, output_size
            total = 0
            do j = 1, kernel_size_16 * 16
                total = total + k(j) * x(i + j - 1)
            end do
            y(i) = total
        end do

    end subroutine

    !> specific implementation for multiplies of 8
    pure subroutine conv1d_k8(x, k, y)
        !> vector to be convolved
        real(real_k), intent(in), contiguous :: x(:)
        !> convolution kernel (should be reversed beforehand)
        real(real_k), intent(in), contiguous :: k(:)
        !> output vector, length size(x) + 1 - size(k)
        real(real_k), intent(out), contiguous :: y(:)
        integer(kind=size_k) :: i, j, kernel_size_8, output_size
        real(real_k) :: total

#       ifndef NDEBUG
        if (modulo(size(k), 8) /= 0) error stop "size of kernel must be multiply of 8"
#       endif

        kernel_size_8 = size(k) / 8
        output_size = size(x) - 8 * kernel_size_8 + 1

        do i = 1, output_size
            total = 0
            do j = 1, kernel_size_8 * 8
                total = total + k(j) * x(i + j - 1)
            end do
            y(i) = total
        end do

    end subroutine

    !> specific implementation for multiplies of 4
    pure subroutine conv1d_k4(x, k, y)
        !> vector to be convolved
        real(real_k), intent(in), contiguous :: x(:)
        !> convolution kernel (should be reversed beforehand)
        real(real_k), intent(in), contiguous :: k(:)
        !> output vector, length size(x) + 1 - size(k)
        real(real_k), intent(out), contiguous :: y(:)
        integer(kind=size_k) :: i, j, kernel_size_4, output_size
        real(real_k) :: total

#       ifndef NDEBUG
        if (modulo(size(k), 4) /= 0) error stop "size of kernel must be multiply of 4"
#       endif

        kernel_size_4 = size(k) / 4
        output_size = size(x) - 4 * kernel_size_4 + 1

        do i = 1, output_size
            total = 0
            do j = 1, kernel_size_4 * 4
                total = total + k(j) * x(i + j - 1)
            end do
            y(i) = total
        end do

    end subroutine

end submodule
