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
        real :: total

        kernel_size = size(k)
        output_size = size(x) - kernel_size + 1

        if (size(y) /= output_size) &
            error stop 'incorrect output shape for 1D convolution'

        do i = 1, output_size
            total = 0
            do j = 1, kernel_size
                total = total + k(j) * x(i + j - 1)
            end do
            y(i) = total
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

        ! appropriate simd procedure was not executed so, run the loop version
        if (status == 0) &
            call conv1d_core(x, k, y)

    end subroutine


end submodule
