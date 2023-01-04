submodule (conv1d_m) pad

    implicit none

contains

    module subroutine conv1d_pad_set_kernel(self, k)
        class(conv1d_pad_t), intent(inout) :: self
        real(real32), intent(in) :: k(:)
        integer :: nk, padding, pad_modulo

        if (allocated(self % kernel)) deallocate(self % kernel)

        nk = size(k)

        pad_modulo = merge(4, 8, nk == 3)
        if (self % pad_modulo >= 1) &
            pad_modulo = self % pad_modulo

        padding = modulo(-nk, pad_modulo)

        self % padding = padding
        self % kernel_size = nk

        allocate(self % kernel(nk + padding))
        self % kernel(1:nk) = k(nk:1:-1)
        if (padding > 0) self % kernel(nk + 1 :) = 0

    end subroutine


    module function conv1d_pad_output_shape(self, input_shape) result(output_shape)
        class(conv1d_pad_t), intent(in) :: self
        integer, intent(in) :: input_shape
        integer :: output_shape

        if (.not. allocated(self % kernel)) &
            error stop '1D convolution kernel not initialized'

        output_shape = input_shape - self % kernel_size + 1

        if (self % trim_pad) &
            output_shape = output_shape - self % padding

    end function


    module subroutine conv1d_pad_conv(self, x, y)
        class(conv1d_pad_t), intent(in) :: self

        real(real32), intent(in), contiguous :: x(:)
        real(real32), intent(out), contiguous :: y(:)
        integer :: padding, nk, nx, ny

        if (.not. allocated(self % kernel)) &
            error stop '1D convolution kernel not initialized'

        nx = size(x)

        if (size(y) /= self % output_shape(nx)) &
            error stop 'conv1d_pad: wrong output dimension'

        nk = self % kernel_size
        ny = nx - nk + 1
        padding = self % padding

        if (any(modulo(size(self % kernel), [4, 8]) == 0) .and. self % use_simd) then
            call conv1d_simd(x, self % kernel, y(:ny - padding))
        else
            call conv1d_core(x, self % kernel, y(:ny - padding))
        end if

        if (padding > 0 .and. (.not. self % trim_pad)) then
            call conv1d_core(x(ny - padding + 1:), &
                self % kernel(:nk), &
                y(ny - padding + 1:))
        end if

    end subroutine

end submodule
