submodule (conv1d_m) core

    use iso_c_binding
    use iso_fortran_env, only: error_unit

    implicit none

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
        real(real32), intent(out), contiguous :: y(:)
        integer(int64) :: i, j, kernel_size, output_size
        real(real32) :: total

        kernel_size = size(k)
        output_size = size(x) - kernel_size + 1

        if (size(y) /= output_size) &
            error stop 'incorrect output shape for 1D convolution'

        if (modulo(kernel_size, 8_int64) == 0) then
            call conv1d_core_8(x, k, y)
            return
        end if

        if (modulo(kernel_size, 4_int64) == 0) then
            call conv1d_core_4(x, k, y)
            return
        end if

        do i = 1, output_size
            total = 0
            do j = 1, kernel_size
                total = total + k(j) * x(i + j - 1)
            end do
            y(i) = total
            ! y(i) = sum(k * x(i : i + kernel_size - 1))
        end do

    end subroutine

    module subroutine conv1d_core_8(x, k, y)
        real(real32), intent(in), contiguous :: x(:), k(:)
        real(real32), intent(out), contiguous :: y(:)
        integer(int64) :: i, j, l, kernel_size, output_size
        real(real32) :: total(8)

        kernel_size = size(k)
        output_size = size(x) - kernel_size + 1

        do i = 1, output_size
            total = 0
            do j = 1, kernel_size, 8
                do l = 1, 8
                    total(l) = total(l) + k((j - 1) + l) * x((i + j - 2) + l)
                end do
            end do
            y(i) = sum(total)
        end do
    end subroutine

    module subroutine conv1d_core_4(x, k, y)
        real(real32), intent(in), contiguous :: x(:), k(:)
        real(real32), intent(out), contiguous :: y(:)
        integer(int64) :: i, j, l, kernel_size, output_size
        real(real32) :: total(4)

        kernel_size = size(k)
        output_size = size(x) - kernel_size + 1

        do i = 1, output_size
            total = 0
            do j = 1, kernel_size, 4
                do l = 1, 4
                    total(l) = total(l) + k((j - 1) + l) * x((i + j - 2) + l)
                end do
            end do
            y(i) = sum(total)
        end do
    end subroutine

    module subroutine conv1d_simd(x, k, y)
        real(real32), intent(in), contiguous, target :: x(:), k(:)
        real(real32), intent(out), contiguous, target :: y(:)
        integer(c_size_t) :: kernel_size, output_size
        integer(c_int) :: status

        kernel_size = size(k)
        output_size = size(x) - kernel_size + 1

        call convolution_simd(c_loc(x), c_loc(y), output_size, c_loc(k), kernel_size, status)

        if (status == 0) &
            call conv1d_core(x, k, y)

    end subroutine


end submodule
