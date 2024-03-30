submodule (conv1d_m) c1d_core

    use iso_c_binding
    use iso_fortran_env, only: error_unit
    use conv_base_m, only: size_k

    implicit none (type, external)

    interface
        subroutine convolution_simd(input, output, output_length, &
            kernel, kernel_length, status) bind(C, name='convolution_simd')
            import :: c_ptr, c_size_t, c_int
            type(c_ptr), value :: input, output
            integer(c_size_t), value :: output_length
            type(c_ptr), value :: kernel
            integer(c_size_t), value :: kernel_length
            integer(c_int) :: status
        end subroutine
    end interface

contains

    !> basic subroutine for 1D convolution, pure Fortran
    module subroutine conv1d_core(x, k, y)
        !> vector to be convolved
        real(real32), intent(in), contiguous :: x(:)
        !> convolution kernel (should be reversed beforehand)
        real(real32), intent(in), contiguous :: k(:)
        !> output vector, length size(x) + 1 - size(k)
        real(real32), intent(out), contiguous :: y(:)
        integer(kind=size_k) :: kernel_size

        kernel_size = size(k)

#       ifndef NDEBUG
        if (size(y) /= size(x) - size(k) + 1) &
            error stop 'incorrect output shape for 1D convolution'
#       endif

        if (modulo(kernel_size, 16) == 0) then
            call conv1d_k16(x, k, y)
        else if (modulo(kernel_size, 8) == 0) then
            call conv1d_k8(x, k, y)
        else if (modulo(kernel_size, 4) == 0) then
            call conv1d_k4(x, k, y)
        else
            call conv1d_general(x, k, y)
        end if

    end subroutine

    !> compute the convolution for any kernel length
    subroutine conv1d_general(x, k, y)
        !> vector to be convolved
        real(real32), intent(in), contiguous :: x(:)
        !> convolution kernel (should be reversed beforehand)
        real(real32), intent(in), contiguous :: k(:)
        !> output vector, length size(x) + 1 - size(k)
        real(real32), intent(out), contiguous :: y(:)
        integer(kind=size_k) :: i, j, kernel_size, output_size
        real(real32) :: total

        kernel_size = size(k)
        output_size = size(x) - kernel_size + 1

        do i = 1, output_size
            total = 0
            do j = 1, kernel_size
                total = total + k(j) * x(i + j - 1)
            end do
            y(i) = total
        end do

    end subroutine

    !> specific implementation for multiplies of 8
    subroutine conv1d_k16(x, k, y)
        !> vector to be convolved
        real(real32), intent(in), contiguous :: x(:)
        !> convolution kernel (should be reversed beforehand)
        real(real32), intent(in), contiguous :: k(:)
        !> output vector, length size(x) + 1 - size(k)
        real(real32), intent(out), contiguous :: y(:)
        integer(kind=size_k) :: i, j, kernel_size_16, output_size
        real(real32) :: total

        ! this subroutine is only called internally, so we do not check
        ! whether modulo(kernel_size, 8) == 0
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
    subroutine conv1d_k8(x, k, y)
        !> vector to be convolved
        real(real32), intent(in), contiguous :: x(:)
        !> convolution kernel (should be reversed beforehand)
        real(real32), intent(in), contiguous :: k(:)
        !> output vector, length size(x) + 1 - size(k)
        real(real32), intent(out), contiguous :: y(:)
        integer(kind=size_k) :: i, j, kernel_size_8, output_size
        real(real32) :: total

        ! this subroutine is only called internally, so we do not check
        ! whether modulo(kernel_size, 8) == 0
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
    subroutine conv1d_k4(x, k, y)
        !> vector to be convolved
        real(real32), intent(in), contiguous :: x(:)
        !> convolution kernel (should be reversed beforehand)
        real(real32), intent(in), contiguous :: k(:)
        !> output vector, length size(x) + 1 - size(k)
        real(real32), intent(out), contiguous :: y(:)
        integer(kind=size_k) :: i, j, kernel_size_4, output_size
        real(real32) :: total

        ! this subroutine is only called internally, so we do not check
        ! whether modulo(kernel_size, 4) == 0
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

    !> compute the convolution explicitly implemented using SIMD instruction
    module subroutine conv1d_simd(x, k, y)
        !> vector to be convolved
        real(real32), intent(in), contiguous, target :: x(:)
        !> convolution kernel (should be reversed beforehand)
        real(real32), intent(in), contiguous, target :: k(:)
        !> output vector, length size(x) + 1 - size(k)
        real(real32), intent(out), contiguous, target :: y(:)
        integer(c_size_t) :: kernel_size, output_size
        integer(c_int) :: status

        kernel_size = size(k, kind=c_size_t)
        output_size = size(x, kind=c_size_t) - kernel_size + 1

        call convolution_simd(c_loc(x), c_loc(y), output_size, c_loc(k), kernel_size, status)

        ! appropriate simd procedure was not executed so, run the loop version
        if (status == 0) then
            block
                logical, save :: warn = .true.

                if (warn) then
                    write (error_unit, "(a, i0, a)") &
                        "Warning: SIMD convolution with kernel width ", &
                        kernel_size, " failed; using native Fortran version"
                    warn = .false.
                end if
            end block
            call conv1d_core(x, k, y)
        end if

    end subroutine

end submodule
