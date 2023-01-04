submodule (conv2d_m) line

    use iso_fortran_env
    implicit none

contains

    subroutine conv2d_line_set_kernel(self, k)
        class(conv2d_line_t), intent(inout) :: self
        real(real32), intent(in) :: k(:,:)
        integer :: i

        if (allocated(self % kernels_1d)) deallocate(self % kernels_1d)

        self % kernel_shape = shape(k)

        allocate(self % kernels_1d(self % kernel_shape(2)), source=conv1d_pad_t( &
            pad_modulo = self % pad_modulo, &
            trim_pad   = self % trim_pad,   &
            use_simd   = self % use_simd    ))

        do i = 1, self % kernel_shape(2)
            call self % kernels_1d(i) % set_kernel(k(:, self % kernel_shape(2) - i + 1))
        end do

    end subroutine


    function conv2d_line_output_shape(self, input_shape) result(output_shape)
        class(conv2d_line_t), intent(in) :: self
        integer, intent(in) :: input_shape(2)
        integer :: output_shape(2)

        if (.not. allocated(self % kernels_1d)) &
            error stop '2D convolution kernel not initialized'

        output_shape = input_shape - self % kernel_shape + 1
        output_shape(1) = self % kernels_1d(1) % output_shape(input_shape(1))

    end function


    subroutine conv2d_line_conv(self, x, y)
        class(conv2d_line_t), intent(inout) :: self
        real(real32), intent(in), contiguous :: x(:,:)
        real(real32), intent(out), contiguous :: y(:,:)

        real(real32), allocatable :: line_buf(:)
        integer :: output_shape(2), i, j

        if (.not. allocated(self % kernels_1d)) &
            error stop '2D convolution kernel not initialized'

        output_shape = self % output_shape(shape(x))

        allocate(line_buf(output_shape(1)))

        do i = 1, output_shape(2)
            do j = 1, self % kernel_shape(2)
                call self % kernels_1d(j) % conv(x(:, i + j - 1), line_buf)
                if (j == 1) then
                    y(:, i) = line_buf
                else
                    y(:, i) = y(:, i) + line_buf
                end if
            end do
        end do

    end subroutine

end submodule
