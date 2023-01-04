submodule (conv2d_m) core

    use iso_fortran_env
    implicit none

contains

    subroutine conv2d_ref(x, k, y)
        real(real32), intent(in), contiguous :: x(:,:), k(:,:)
        real(real32), intent(out), contiguous :: y(:,:)

        integer(int64) :: ix, ik, jx, jk, input_shape(2), kernel_shape(2), output_shape(2)
        real(real32) :: total

        input_shape = shape(x)
        kernel_shape = shape(k)
        output_shape = input_shape - kernel_shape + 1

        if (any(input_shape - kernel_shape + 1 /= shape(y))) &
            error stop 'incorrect output shape for 2D convolution'

        do jx = 1, output_shape(2)
            do ix = 1, output_shape(1)
                total = 0
                do jk = 1, kernel_shape(2)
                    do ik = 1, kernel_shape(1)
                        total = total + k(ik, jk) * x(ix + ik - 1, jx + jk - 1)
                    end do
                end do
                y(ix, jx) = total
            end do
        end do

    end subroutine


    subroutine conv2d_core(x, k, y)
        use conv1d_m, only: conv1d_core
        real(real32), contiguous, intent(in) :: x(:,:), k(:,:)
        real(real32), contiguous, intent(out) :: y(:,:)
        integer(int64) :: i, j, input_shape(2), output_shape(2)
        real(real32), allocatable :: line_buf(:)
        integer(int32) :: kernel_shape(2)

        input_shape = shape(x)
        kernel_shape = shape(k)
        output_shape = input_shape - kernel_shape + 1

        if (any(input_shape - kernel_shape + 1 /= shape(y))) &
            error stop 'incorrect output shape for 2D convolution'

        allocate(line_buf(output_shape(1)))

        do i = 1, output_shape(2)
            do j = 1, kernel_shape(2)
                call conv1d_core(x(:, i + j - 1), k(:, j), line_buf)
                if (j == 1) then
                    y(:, i) = line_buf
                else
                    y(:, i) = y(:, i) + line_buf
                end if
            end do
        end do

    end subroutine


    subroutine conv2d_simd(x, k, y)
        use conv1d_m, only: conv1d_simd
        real(real32), contiguous, intent(in) :: x(:,:), k(:,:)
        real(real32), contiguous, intent(out) :: y(:,:)
        integer(int64) :: i, j, input_shape(2), output_shape(2)
        real(real32), allocatable :: line_buf(:)
        integer(int32) :: kernel_shape(2)

        input_shape = shape(x)
        kernel_shape = shape(k)
        output_shape = input_shape - kernel_shape + 1

        if (any(input_shape - kernel_shape + 1 /= shape(y))) &
            error stop 'incorrect output shape for 2D convolution'

        allocate(line_buf(output_shape(1)))

        do i = 1, output_shape(2)
            do j = 1, kernel_shape(2)
                call conv1d_simd(x(:, i + j - 1), k(:, j), line_buf)
                if (j == 1) then
                    y(:, i) = line_buf
                else
                    y(:, i) = y(:, i) + line_buf
                end if
            end do
        end do

    end subroutine


    subroutine conv2d_set_kernel(self, k)
        class(conv2d_t), intent(inout) :: self
        real(real32), intent(in) :: k(:,:)

        if (allocated(self % kernel)) &
            deallocate(self % kernel)

        allocate(self % kernel, &
            source=k(size(k, 1) : 1 : -1, size(k, 2) : 1 : -1))

    end subroutine


    function conv2d_output_shape(self, input_shape) result(output_shape)
        class(conv2d_t), intent(in) :: self
        integer, intent(in) :: input_shape(2)
        integer :: output_shape(2)

        if (.not. allocated(self % kernel)) &
            error stop '2D convolution kernel not initialized'

        output_shape = input_shape - shape(self % kernel) + 1

    end function


    subroutine conv2d_conv(self, x, y)
        class(conv2d_t), intent(inout) :: self
        real(real32), intent(in), contiguous :: x(:,:)
        real(real32), intent(out), contiguous :: y(:,:)

        if (.not. allocated(self % kernel)) &
            error stop '2D convolution kernel not initialized'

        if (self % row_by_row) then
            call conv2d_core(x, self % kernel, y)
        else
            call conv2d_ref(x, self % kernel, y)
        end if

    end subroutine

end submodule
