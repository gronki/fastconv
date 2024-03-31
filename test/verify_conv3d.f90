program verify_conv3d

    use conv3d_m
    use conv1d_m
    use, intrinsic :: iso_fortran_env, only: real32

    implicit none

    real(real32) :: x(12,15,17), k(3,3,5), y1(10,13,13), y2(10,13,13)

    call random_seed()

    call random_number(x)
    call random_number(k)

    block
        type(conv3d_ref_t) :: conv
        call conv % set_kernel(k)
        call conv % conv(x, y1)
    end block

    block
        type(conv3d_line_t) :: conv
        conv = conv3d_line_t(conv1d_ref_t())
        call conv % set_kernel(k)
        call conv % conv(x, y2)

        associate (err => sum(abs(y1-y2)) / size(y1))
            print *, err
            if (err > 2e-6) error stop
        end associate
    end block


    block
        type(conv3d_line_t) :: conv
        conv = conv3d_line_t(conv1d_pad_t(pad_modulo=8))
        call conv % set_kernel(k)
        call conv % conv(x, y2)

        associate (err => sum(abs(y1-y2)) / size(y1))
            print *, err
            if (err > 2e-6) error stop
        end associate
    end block

end program
