submodule (conv2d_m) c2d_line

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
        integer(kind=size_k) :: ix, kernel_shape(2)

        if (allocated(self % kernels_1d)) deallocate(self % kernels_1d)

        kernel_shape = shape(k)
        self % kernel_shape_ = kernel_shape

        if (.not. allocated(self % conv1d_mold)) &
            allocate(conv1d_t :: self % conv1d_mold)

        allocate(self % kernels_1d(kernel_shape(2)), &
            source=self % conv1d_mold)

        self % kernels_1d % preserve_shape = .false.

        do ix = 1, kernel_shape(2)
            call self % kernels_1d(ix) % set_kernel(k(:, kernel_shape(2) - ix + 1))
        end do

    end subroutine



    pure module function conv2d_line_kernel_shape(self) result(kernel_shape)
        class(conv2d_line_t), intent(in) :: self
        integer(kind=size_k) :: kernel_shape(2)

#       ifndef NDEBUG
        if (.not. allocated(self % kernels_1d)) &
            error stop '2D convolution kernel not initialized'
#       endif

        kernel_shape = self % kernel_shape_
    end function


    module subroutine conv2d_line_conv(self, x, y)
        class(conv2d_line_t), intent(in) :: self
        real(real32), intent(in), contiguous :: x(:,:)
        real(real32), intent(inout), contiguous :: y(:,:)

        real(real32), allocatable :: line_buf(:)
        integer(kind=size_k) :: input_shape(2), output_shape_raw(2), output_shape(2)
        integer(kind=size_k) :: ix, ik, offset(2), kernel_shape(2)

#       ifndef NDEBUG
        if (.not. allocated(self % kernels_1d)) &
            error stop '2D convolution kernel not initialized'
#       endif

        input_shape = shape(x, kind=size_k)
        output_shape = self % output_shape(input_shape)

#       ifndef NDEBUG
        if (any(shape(y) /= output_shape)) then
            block
                character(len=256) :: errmsg
                write (errmsg, '(a, a, i0, ", ", i0, a, i0, ", ", i0, a)') &
                    "incorrect shape for 2D convolution output; ", &
                    "expected [", output_shape, "], but got [", shape(y), "]"
                error stop trim(errmsg)
            end block
        end if
#       endif

        kernel_shape = self % kernel_shape()
        output_shape_raw = input_shape + 1 - kernel_shape
        offset = merge((kernel_shape - 1) / 2, 0_size_k, self % preserve_shape)

        allocate(line_buf(output_shape_raw(1)))

        !!$omp parallel do private(ik, line_buf)
        do ix = 1, output_shape_raw(2)
            associate(y_row => y(1 + offset(1) : output_shape_raw(1) + offset(1), ix + offset(2)))
                do ik = 1, kernel_shape(2)
                    call self % kernels_1d(ik) % conv(x(:, ix + ik - 1), line_buf)
                    if (ik == 1) then
                        y_row(:) = line_buf
                    else
                        y_row(:) = y_row + line_buf
                    end if
                end do
            end associate
        end do
        !!$omp end parallel do
    end subroutine

end submodule
