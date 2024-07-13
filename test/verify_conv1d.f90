program verify_conv3d

    use conv1d_m
    use, intrinsic :: iso_fortran_env, only: real32

    implicit none

    real(real32) :: x(284), k(25), y1(260), y2(260)

    print *, 'test: conv1d'

    call random_seed()

    call random_number(x)
    call random_number(k)

    block
        type(conv1d_ref_t) :: conv
        call conv % set_kernel(k)
        call conv % conv(x, y1)
    end block

    block
        type(conv1d_pad_t) :: conv
        conv = conv1d_pad_t(pad_modulo=8)
        call conv % set_kernel(k)
        call conv % conv(x, y2)
    end block

    associate (err => sum(abs(y1-y2)) / size(y1))
        print *, 'error pad-ref=', err
        if (err > 2e-6) error stop
    end associate

end program
