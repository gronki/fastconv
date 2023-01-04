submodule (conv2d_m) pad
    use iso_fortran_env

    implicit none

contains


    subroutine conv2d_pad_set_kernel(self, k)
        class(conv2d_pad_t), intent(inout) :: self
        real(real32), intent(in) :: k(:,:)
        integer :: padding, kernel_shape(2), pad_modulo

        kernel_shape = shape(k)
        self % kernel_shape = kernel_shape

        pad_modulo = merge(4, 8, kernel_shape(1) == 3)
        if (self % pad_modulo >= 1) &
            pad_modulo = self % pad_modulo

        padding = modulo(-kernel_shape(1), pad_modulo)
        self % padding = padding

        if (allocated(self % kernel)) &
            deallocate(self % kernel)

        allocate(self % kernel, &
            source = k(kernel_shape(1) : 1 : -1, kernel_shape(2) : 1 : -1))

        if (allocated(self % kernel_pad)) &
            deallocate(self % kernel_pad)

        allocate(self % kernel_pad(kernel_shape(1) + padding, &
            kernel_shape(2)))

        self % kernel_pad(1 : kernel_shape(1), :) = self % kernel

        if (padding > 0) &
            self % kernel_pad(kernel_shape(1) + 1 :, :) = 0

    end subroutine


    function conv2d_pad_output_shape(self, input_shape) result(output_shape)
        class(conv2d_pad_t), intent(in) :: self
        integer, intent(in) :: input_shape(2)
        integer :: output_shape(2)

        if (.not. allocated(self % kernel_pad)) &
            error stop '2D convolution kernel not initialized'

        output_shape = input_shape - self % kernel_shape + 1
        if (self % trim_pad) &
            output_shape(1) = output_shape(1) - self % padding

    end function


    subroutine conv2d_pad_conv(self, x, y)
        class(conv2d_pad_t), intent(inout) :: self
        real(real32), intent(in), contiguous :: x(:,:)
        real(real32), intent(out), contiguous :: y(:,:)

        integer :: input_shape(2), output_shape_full(2)
        integer :: kernel_shape(2), xpad_shape(2), padding

        if (.not. allocated(self % kernel_pad)) &
            error stop '2D convolution kernel not initialized'

        input_shape = shape(x)
        kernel_shape = self % kernel_shape
        output_shape_full = input_shape - self % kernel_shape + 1
        padding = self % padding

        xpad_shape = [input_shape(1) + padding, input_shape(2)]

        if (padding > 0 .and. .not. self % trim_pad) then
            if (allocated(self % xpad)) then
                if (any(shape(self % xpad) /= xpad_shape)) &
                    deallocate(self % xpad)
            end if

            if (.not. allocated(self % xpad)) &
                allocate(self % xpad(xpad_shape(1), xpad_shape(2)))

            self % xpad(:input_shape(1), :) = x
            self % xpad(input_shape(1) + 1:, :) = 0

            call conv2d_core_or_simd(self % xpad, self % kernel_pad, y, self % use_simd)
        else
            call conv2d_core_or_simd(x, self % kernel_pad, y, self % use_simd)
        end if

    end subroutine


    subroutine conv2d_core_or_simd(x, k, y, use_simd)
        real(real32), contiguous, intent(in) :: x(:,:), k(:,:)
        real(real32), contiguous, intent(out) :: y(:,:)
        logical :: use_simd

        if (use_simd) then
            call conv2d_simd(x, k, y)
        else
            call conv2d_core(x, k, y)
        end if
    end subroutine

end submodule

