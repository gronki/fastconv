submodule (conv2d_m) line

    use iso_fortran_env
    use conv_base_m, only: size_k
    implicit none (type, external)

contains

    module function conv2d_line_t_ctor(conv1d_mold) result(self)
        class(conv1d_base_t), intent(in), optional :: conv1d_mold
        type(conv2d_line_t) :: self

        if (present(conv1d_mold)) then
            allocate(self % conv1d_mold, source=conv1d_mold)
        end if
    end function


    module subroutine conv2d_line_set_kernel(self, k)
        use conv1d_m, only: conv1d_t
        class(conv2d_line_t), intent(inout) :: self
        real(real32), intent(in) :: k(:,:)
        integer(kind=size_k) :: ix

        if (allocated(self % kernels_1d)) deallocate(self % kernels_1d)

        self % kernel_shape = shape(k)

        if (.not. allocated(self % conv1d_mold)) &
            allocate(conv1d_t :: self % conv1d_mold)

        allocate(self % kernels_1d(self % kernel_shape(2)), &
            source=self % conv1d_mold)

        self % kernels_1d % preserve_shape = .false.

        do ix = 1, self % kernel_shape(2)
            call self % kernels_1d(ix) % set_kernel(k(:, self % kernel_shape(2) - ix + 1))
        end do

    end subroutine


    module function conv2d_line_output_shape(self, input_shape) result(output_shape)
        class(conv2d_line_t), intent(in) :: self
        integer(kind=size_k), intent(in) :: input_shape(2)
        integer(kind=size_k) :: output_shape(2)

#       if defined(CHECKS)
        if (.not. allocated(self % kernels_1d)) &
            error stop '2D convolution kernel not initialized'
#       endif

        output_shape(1) = self % kernels_1d(1) % output_shape(input_shape(1))
        output_shape(2) = input_shape(2) &
            + merge(0_size_k, 1 - self % kernel_shape(2), self % preserve_shape)

    end function


    module subroutine conv2d_line_conv(self, x, y)
        class(conv2d_line_t), intent(inout) :: self
        real(real32), intent(in), contiguous :: x(:,:)
        real(real32), intent(inout), contiguous :: y(:,:)

        real(real32), allocatable :: line_buf(:)
        integer(kind=size_k) :: input_shape(2), output_shape_raw(2), output_shape(2)
        integer(kind=size_k) :: ix, ik, offset(2), kernel_shape(2)

#       if defined(CHECKS)
        if (.not. allocated(self % kernels_1d)) &
            error stop '2D convolution kernel not initialized'
#       endif

        input_shape = shape(x, kind=size_k)
        output_shape = self % output_shape(input_shape)

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

        kernel_shape = self % kernel_shape
        output_shape_raw = input_shape + 1 - kernel_shape
        offset = merge((kernel_shape - 1) / 2, 0_size_k, self % preserve_shape)

        allocate(line_buf(output_shape_raw(1)))

        do ix = 1, output_shape_raw(2)
            do ik = 1, kernel_shape(2)
                call self % kernels_1d(ik) % conv(x(:, ix + ik - 1), line_buf)
                associate(y_row => y(1 + offset(1) : output_shape_raw(1) + offset(1), ix + offset(2)))
                    if (ik == 1) then
                        y_row = line_buf
                    else
                        y_row = y_row + line_buf
                    end if
                end associate
            end do
        end do

    end subroutine

end submodule
