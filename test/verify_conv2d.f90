program verify_conv3d

    use conv2d_m
    use conv1d_m
    use iso_fortran_env, only: real_k=>real32, size_k=>int64

    implicit none

    real(real_k) :: x(75,36), k(9,5), y1(67,32), y2(67,32)

    print *, 'test: conv2d'
    
    call random_seed()

    call random_number(x)
    call random_number(k)

    block
        type(conv2d_ref_t) :: conv
        call conv % set_kernel(k)
        call conv % conv(x, y1)
    end block

    block
        type(conv2d_line_t) :: conv
        conv = conv2d_line_t(conv1d_ref_t())
        call conv % set_kernel(k)
        call conv % conv(x, y2)
        associate (err => sum(abs(y1-y2)) / size(y1))
            print *, 'error conv2d_line(conv1d_ref)-conv2d_ref', err
            if (err > 2e-6) error stop
        end associate
    end block

    block
        type(conv2d_line_t) :: conv
        conv = conv2d_line_t(conv1d_pad_t(pad_modulo=8))
        call conv % set_kernel(k)
        call conv % conv(x, y2)
        associate (err => sum(abs(y1-y2)) / size(y1))
            print *, 'error conv2d_line(conv1d_pad)-conv2d_ref', err
            if (err > 2e-6) error stop
        end associate
    end block


end program
