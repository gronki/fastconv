submodule (conv2d_m) base

    use conv_base_m, only: size_k
    implicit none (type, external)

contains

    pure module function output_shape(self, input_shape)
        class(conv2d_base_t), intent(in) :: self
        integer(kind=size_k), intent(in) :: input_shape(2)
        integer(kind=size_k) :: output_shape(2)

        output_shape = input_shape + merge(0_size_k, &
            1_size_k - self % kernel_shape(), &
            self % preserve_shape)

    end function


    module function apply(self, x) result(y)
        class(conv2d_base_t), intent(in) :: self
        real(real32), intent(in), contiguous :: x(:,:)
        real(real32), allocatable :: y(:,:)
        integer(kind=size_k) :: output_shape(2)

        output_shape = self % output_shape(shape(x, kind=size_k))
        allocate(y(output_shape(1), output_shape(2)))

        call self % conv(x, y)

    end function

end submodule
