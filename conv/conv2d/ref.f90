submodule (conv2d_m) core

    use iso_fortran_env
    use conv_base_m, only: size_k
    implicit none (type, external)

contains

    subroutine conv2d_ref_set_kernel(self, k)
        class(conv2d_ref_t), intent(inout) :: self
        real(real32), intent(in) :: k(:,:)

        if (allocated(self % kernel)) &
            deallocate(self % kernel)

        allocate(self % kernel, &
            source=k(size(k, 1) : 1 : -1, size(k, 2) : 1 : -1))

    end subroutine


    function conv2d_ref_output_shape(self, input_shape) result(output_shape)
        class(conv2d_ref_t), intent(in) :: self
        integer(kind=size_k), intent(in) :: input_shape(2)
        integer(kind=size_k) :: output_shape(2)

#       if defined(CHECKS)
        if (.not. allocated(self % kernel)) &
            error stop '2D convolution kernel not initialized'
#       endif

        output_shape = input_shape + merge(0_size_k, &
            1_size_k - shape(self % kernel, kind=size_k), &
            self % preserve_shape)

    end function


    subroutine conv2d_ref_conv(self, x, y)
        class(conv2d_ref_t), intent(inout) :: self
        real(real32), intent(in), contiguous :: x(:,:)
        real(real32), intent(inout), contiguous :: y(:,:)

        integer(kind=size_k) :: ix, ik, jx, jk, input_shape(2), kernel_shape(2)
        integer(kind=size_k) :: output_shape_raw(2), output_shape(2), offset(2)
        real(real32) :: total

#       if defined(CHECKS)
        if (.not. allocated(self % kernel)) &
            error stop '2D convolution kernel not initialized'
#       endif

        input_shape = shape(x, kind=size_k)
        kernel_shape = shape(self % kernel, kind=size_k)
        offset = merge((kernel_shape - 1) / 2, 0_size_k, self % preserve_shape)
        output_shape = self % output_shape(input_shape)
        output_shape_raw = input_shape - kernel_shape + 1

#       if defined(CHECKS)
        if (any(shape(y) /= output_shape)) then
            block
                character(len=256) :: errmsg
                write(errmsg, '(a, a, i0, ", ", i0, a, i0, ", ", i0, a)') &
                    "incorrect shape for 2D convolution output; ", &
                    "expected [", output_shape, "], but got [", shape(y), "]"
                error stop trim(errmsg)
            end block
        end if
#       endif

        associate (k => self % kernel)
            do jx = 1, output_shape_raw(2)
                do ix = 1, output_shape_raw(1)
                    total = 0
                    do jk = 1, kernel_shape(2)
                        do ik = 1, kernel_shape(1)
                            total = total + k(ik, jk) * x(ix + ik - 1, jx + jk - 1)
                        end do
                    end do
                    y(ix + offset(1), jx + offset(2)) = total
                end do
            end do
        end associate

    end subroutine

end submodule
