submodule (conv1d_m) simple

    implicit none

contains


    module subroutine conv1d_set_kernel(self, k)
        class(conv1d_t), intent(inout) :: self
        real(real32), intent(in) :: k(:)

        if (allocated(self % kernel)) &
            deallocate(self % kernel)

        allocate(self % kernel, source=k(size(k):1:-1))

    end subroutine


    module function conv1d_output_shape(self, input_shape) result(output_shape)
        class(conv1d_t), intent(in) :: self
        integer, intent(in) :: input_shape
        integer :: output_shape

        if (.not. allocated(self % kernel)) &
            error stop '1D convolution kernel not initialized'

        output_shape = input_shape - size(self % kernel) + 1

    end function


    module subroutine conv1d_conv(self, x, y)
        class(conv1d_t), intent(in) :: self
        real(real32), intent(in), contiguous :: x(:)
        real(real32), intent(out), contiguous :: y(:)

        if (.not. allocated(self % kernel)) &
            error stop '1D convolution kernel not initialized'

        if (size(y) /= self % output_shape(size(x))) &
            error stop 'conv1d: wrong output dimension'

        call conv1d_core(x, self % kernel, y)

    end subroutine

end submodule
