submodule (conv1d_m) simple

    use conv_base_m, only: size_k
    implicit none (type, external)

contains


    module subroutine conv1d_ref_set_kernel(self, k)
        class(conv1d_ref_t), intent(inout) :: self
        real(real32), intent(in) :: k(:)

        if (allocated(self % kernel)) &
            deallocate(self % kernel)

        allocate(self % kernel, source=k(size(k):1:-1))

    end subroutine


    pure module function conv1d_ref_output_shape(self, input_shape) result(output_shape)
        class(conv1d_ref_t), intent(in) :: self
        integer(kind=size_k), intent(in) :: input_shape
        integer(kind=size_k) :: output_shape

#       if defined(CHECKS)
        if (.not. allocated(self % kernel)) &
            error stop '1D convolution kernel not initialized'
#       endif

        output_shape = input_shape + merge(0_size_k, &
            1 - size(self % kernel, kind=size_k), &
            self % preserve_shape)

    end function


    module subroutine conv1d_ref_conv(self, x, y)
        class(conv1d_ref_t), intent(in) :: self
        real(real32), intent(in), contiguous :: x(:)
        real(real32), intent(inout), contiguous :: y(:)
        integer(kind=size_k) :: input_shape, output_shape, output_shape_raw, offset, kernel_shape

        input_shape = size(x, kind=size_k)

#       if defined(CHECKS)
        if (.not. allocated(self % kernel)) &
            error stop '1D convolution kernel not initialized'
#       endif

        output_shape = self % output_shape(input_shape)

#       if defined(CHECKS)
        if (size(y, kind=size_k) /= output_shape) then
            block
                character(len=256) :: errmsg
                write(errmsg, '(a, a, i0, a, i0, a)') &
                    "incorrect shape for 1D convolution output; ", &
                    "expected [", output_shape, "], but got [", size(y), "]"
                error stop trim(errmsg)
            end block
        end if
#       endif

        kernel_shape = size(self % kernel)
        output_shape_raw = input_shape - kernel_shape + 1
        offset = merge((kernel_shape - 1) / 2, 0_size_k, self % preserve_shape)

        call conv1d_core(x, self % kernel, y(1 + offset : output_shape_raw + offset))

    end subroutine

end submodule
