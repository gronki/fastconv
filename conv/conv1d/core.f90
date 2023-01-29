submodule (conv1d_m) core

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

    module subroutine conv1d_core(x, k, y)
        real(real32), intent(in), contiguous :: x(:), k(:)
        real(real32), intent(inout), contiguous :: y(:)
        integer(kind=size_k) :: kernel_shape

        kernel_shape = size(k)

        if (modulo(kernel_shape, 8) == 0) then
            call conv1d_k8(x, k, y)
        else if (modulo(kernel_shape, 4) == 0) then
            call conv1d_k4(x, k, y)
        else
            call conv1d_general(x, k, y)
        end if

    end subroutine

    module subroutine conv1d_general(x, k, y)
        real(real32), intent(in), contiguous :: x(:), k(:)
        real(real32), intent(inout), contiguous :: y(:)
        integer(kind=size_k) :: i, j, kernel_shape, output_size
        real :: total

        kernel_shape = size(k)
        output_size = size(x) - kernel_shape + 1

#       if defined(CHECKS)
        if (size(y) /= output_size) &
            error stop 'incorrect output shape for 1D convolution'
#       endif

        do i = 1, output_size
            total = 0
            do j = 1, kernel_shape
                total = total + k(j) * x(i + j - 1)
            end do
            y(i) = total
        end do

    end subroutine

    module subroutine conv1d_k8(x, k, y)
        real(real32), intent(in), contiguous :: x(:), k(:)
        real(real32), intent(inout), contiguous :: y(:)
        integer(kind=size_k) :: i, j, kernel_size_8, output_size
        real :: total

        kernel_size_8 = size(k) / 8
        output_size = size(x) - kernel_size_8 * 8 + 1

#       if defined(CHECKS)
        if (size(y) /= output_size) &
            error stop 'incorrect output shape for 1D convolution'
#       endif

        do i = 1, output_size
            total = 0
            do j = 1, kernel_size_8 * 8
                total = total + k(j) * x(i + j - 1)
            end do
            y(i) = total
        end do

    end subroutine

    module subroutine conv1d_k4(x, k, y)
        real(real32), intent(in), contiguous :: x(:), k(:)
        real(real32), intent(inout), contiguous :: y(:)
        integer(kind=size_k) :: i, j, kernel_size_4, output_size
        real :: total

        kernel_size_4 = size(k) / 4
        output_size = size(x) - kernel_size_4 * 4 + 1

#       if defined(CHECKS)
        if (size(y) /= output_size) &
            error stop 'incorrect output shape for 1D convolution'
#       endif

        do i = 1, output_size
            total = 0
            do j = 1, kernel_size_4 * 4
                total = total + k(j) * x(i + j - 1)
            end do
            y(i) = total
        end do

    end subroutine


    module subroutine conv1d_simd(x, k, y)
        real(real32), intent(in), contiguous, target :: x(:), k(:)
        real(real32), intent(out), contiguous, target :: y(:)
        integer(c_size_t) :: kernel_shape, output_size
        integer(c_int) :: status

        kernel_shape = size(k, kind=c_size_t)
        output_size = size(x, kind=c_size_t) - kernel_shape + 1

        call convolution_simd(c_loc(x), c_loc(y), output_size, c_loc(k), kernel_shape, status)

        ! appropriate simd procedure was not executed so, run the loop version
        if (status == 0) &
            call conv1d_core(x, k, y)

    end subroutine


end submodule
