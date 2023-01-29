submodule (conv1d_m) pad

    use conv_base_m, only: size_k
    implicit none (type, external)

contains

    module subroutine conv1d_pad_set_kernel(self, k)
        class(conv1d_pad_t), intent(inout) :: self
        real(real32), intent(in) :: k(:)
        integer(kind=size_k) :: kernel_shape, padding

        if (allocated(self % kernel)) deallocate(self % kernel)

        kernel_shape = size(k)

        padding = modulo(-kernel_shape, self % pad_modulo)

        self % padding = padding
        self % kernel_shape = kernel_shape

        allocate(self % kernel(kernel_shape + padding))
        self % kernel(1:kernel_shape) = k(kernel_shape:1:-1)
        if (padding > 0) self % kernel(kernel_shape + 1 :) = 0

    end subroutine


    pure module function conv1d_pad_output_shape(self, input_shape) result(output_shape)
        class(conv1d_pad_t), intent(in) :: self
        integer(kind=size_k), intent(in) :: input_shape
        integer(kind=size_k) :: output_shape

#       if defined(CHECKS)
        if (.not. allocated(self % kernel)) &
            error stop '1D convolution kernel not initialized'
#       endif

        output_shape = input_shape + merge(0_size_k, &
            1_size_k - self % kernel_shape, &
            self % preserve_shape)

    end function


    module subroutine conv1d_pad_conv(self, x, y)
        class(conv1d_pad_t), intent(in) :: self
        real(real32), intent(in), contiguous :: x(:)
        real(real32), intent(inout), contiguous :: y(:)
        integer(kind=size_k) :: padding, kernel_shape, input_shape
        integer(kind=size_k) :: output_shape, output_shape_raw, offset

        input_shape = size(x)

#       if defined(CHECKS)
        if (.not. allocated(self % kernel)) &
            error stop '1D convolution kernel not initialized'
#       endif

        output_shape = self % output_shape(input_shape)

#       if defined(CHECKS)
        if (size(y) /= output_shape) then
            block
                character(len=256) :: errmsg
                write(errmsg, '(a, a, i0, a, i0, a)') &
                    "incorrect shape for 1D convolution output; ", &
                    "expected [", output_shape, "], but got [", size(y), "]"
                error stop trim(errmsg)
            end block
        end if
#       endif

        kernel_shape = self % kernel_shape
        output_shape_raw = input_shape - kernel_shape + 1
        offset = merge((kernel_shape - 1_size_k) / 2, 0_size_k, self % preserve_shape)
        padding = self % padding

        if (self % use_simd) then
            call conv1d_simd(x, self % kernel, y(1 + offset : output_shape_raw - padding + offset))
        else
            call conv1d_core(x, self % kernel, y(1 + offset : output_shape_raw - padding + offset))
        end if

        if (padding > 0) then
            call conv1d_core(x(output_shape_raw - padding + 1:), &
                self % kernel(:kernel_shape), &
                y(output_shape_raw - padding + offset + 1 : output_shape_raw + offset))
        end if

    end subroutine

end submodule
