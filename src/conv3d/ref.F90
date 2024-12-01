submodule (conv3d_m) c3d_core

    use iso_fortran_env
    implicit none (type, external)

contains

    module subroutine conv3d_ref_set_kernel(self, k)
        class(conv3d_ref_t), intent(inout) :: self
        real(real_k), intent(in) :: k(:,:,:)

        if (allocated(self % kernel)) &
            deallocate(self % kernel)

        allocate(self % kernel, &
            source=k(size(k, 1) : 1 : -1, size(k, 2) : 1 : -1, size(k, 3) : 1 : -1))

    end subroutine


    pure module function conv3d_ref_kernel_shape(self) result(kernel_shape)
        class(conv3d_ref_t), intent(in) :: self
        integer(kind=size_k) :: kernel_shape(3)

#       ifndef NDEBUG
        if (.not. allocated(self % kernel)) &
            error stop '3D convolution kernel not initialized'
#       endif

        kernel_shape = shape(self % kernel)
    end function


    module subroutine conv3d_ref_conv(self, x, y)
        class(conv3d_ref_t), intent(in) :: self
        real(real_k), intent(in), contiguous :: x(:,:,:)
        real(real_k), intent(inout), contiguous :: y(:,:,:)

        integer(kind=size_k) :: ix, ik, jx, jk, kx, kk, input_shape(3), kernel_shape(3)
        integer(kind=size_k) :: output_shape_raw(3), output_shape(3), offset(3)
        real(real_k) :: total

#       ifndef NDEBUG
        if (.not. allocated(self % kernel)) &
            error stop '2D convolution kernel not initialized'
#       endif

        input_shape = shape(x, kind=size_k)
        kernel_shape = self % kernel_shape()
        offset = merge((kernel_shape - 1) / 2, 0_size_k, self % preserve_shape)
        output_shape = self % output_shape(input_shape)
        output_shape_raw = input_shape - kernel_shape + 1

#       ifndef NDEBUG
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
            do kx = 1, output_shape_raw(3)
                do jx = 1, output_shape_raw(2)
                    do ix = 1, output_shape_raw(1)
                        total = 0
                        do kk = 1, kernel_shape(3)
                            do jk = 1, kernel_shape(2)
                                do ik = 1, kernel_shape(1)
                                    total = total + k(ik, jk, kk) * x(ix + ik - 1, jx + jk - 1, kx + kk - 1)
                                end do
                            end do
                        end do
                        y(ix + offset(1), jx + offset(2), kx + offset(3)) = total
                    end do
                end do
            end do
        end associate

    end subroutine

end submodule
