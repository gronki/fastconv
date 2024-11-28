submodule (conv1d_m) pad

    use conv_base_m, only: size_k
    implicit none (type, external)

contains

    pure module subroutine conv1d_pad_set_kernel(self, k)
        class(conv1d_pad_t), intent(inout) :: self
        real(real32), intent(in) :: k(:)
        integer(kind=size_k) :: kernel_size, padding

        if (allocated(self % kernel)) deallocate(self % kernel)

        kernel_size = size(k)

        padding = modulo(-kernel_size, self % pad_modulo)

        self % padding = padding
        self % kernel_size = kernel_size

        allocate(self % kernel(kernel_size + padding))
        self % kernel(1:kernel_size) = k(kernel_size:1:-1)
        if (padding > 0) self % kernel(kernel_size + 1 :) = 0

    end subroutine


    pure module function conv1d_pad_output_size(self, input_size) result(output_size)
        class(conv1d_pad_t), intent(in) :: self
        integer(kind=size_k), intent(in) :: input_size
        integer(kind=size_k) :: output_size

#       ifndef NDEBUG
        if (.not. allocated(self % kernel)) &
            error stop '1D convolution kernel not initialized'
#       endif

        output_size = input_size + merge(0_size_k, &
            1_size_k - self % kernel_size, &
            self % preserve_shape)

    end function


    pure module subroutine conv1d_pad_conv(self, x, y)
        class(conv1d_pad_t), intent(in) :: self
        real(real32), intent(in), contiguous :: x(:)
        real(real32), intent(inout), contiguous :: y(:)
        integer(kind=size_k) :: padding, kernel_size, input_size
        integer(kind=size_k) :: output_size, output_size_raw, offset

        input_size = size(x)

#       ifndef NDEBUG
        if (.not. allocated(self % kernel)) &
            error stop '1D convolution kernel not initialized'
#       endif

        output_size = self % output_size(input_size)

#       ifndef NDEBUG
        if (size(y) /= output_size) then
            block
                character(len=256) :: errmsg
                write(errmsg, '(a, a, i0, a, i0, a)') &
                    "incorrect shape for 1D convolution output; ", &
                    "expected [", output_size, "], but got [", size(y), "]"
                error stop trim(errmsg)
            end block
        end if
#       endif

        kernel_size = self % kernel_size
        output_size_raw = input_size - kernel_size + 1
        offset = merge((kernel_size - 1_size_k) / 2, 0_size_k, self % preserve_shape)
        padding = self % padding

        call conv1d_core(x, self % kernel, y(1 + offset : output_size_raw - padding + offset))

        if (padding > 0) then
            call conv1d_core(x(output_size_raw - padding + 1:), &
                self % kernel(:kernel_size), &
                y(output_size_raw - padding + offset + 1 : output_size_raw + offset))
        end if

    end subroutine

end submodule
