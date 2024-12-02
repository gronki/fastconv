submodule (conv1d_m) pad

    implicit none (type, external)

contains
    
    pure module subroutine conv1d_pad_set_kernel(self, k)
        class(conv1d_pad_t), intent(inout) :: self
        real(real_k), intent(in) :: k(:)
        integer(kind=size_k) :: kernel_size, padding
        
        self % kernel_size = size(k, kind=size_k)
        self % kernel = padded_1d_kernel(k, self % pad_modulo)

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
        real(real_k), intent(in), contiguous :: x(:)
        real(real_k), intent(inout), contiguous :: y(:)
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

        call conv1d_pad_core(x, self % kernel, kernel_size, y(1 + offset : output_size_raw + offset))

    end subroutine

end submodule
