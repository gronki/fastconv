program test_conv2d

    use testing_m
    use iso_fortran_env
    use conv2d_m
    use conv1d_m

    implicit none

#   ifdef __GFORTRAN__
#       define QUOTE(v) "v"
#   else
#       define QUOTE(v) #v
#   endif
#   define run_test(x) run_test_1(QUOTE(x), (x))

    open(unit=77, file='test_conv2d_stats.txt', action='write')

    call run_test(conv2d_t(row_by_row=.false.))
    call run_test(conv2d_t(row_by_row=.true.))

    call run_test(conv2d_pad_t())
    call run_test(conv2d_pad_t(use_simd=.true.))
    call run_test(conv2d_pad_t(trim_pad=.true.))
    call run_test(conv2d_pad_t(pad_modulo=4, trim_pad=.true.))
    call run_test(conv2d_pad_t(pad_modulo=8, trim_pad=.true.))
    call run_test(conv2d_pad_t(pad_modulo=8, trim_pad=.true., use_simd=.true.))

    call run_test(conv2d_line_t())
    call run_test(conv2d_line_t(use_simd=.true.))
    call run_test(conv2d_line_t(trim_pad=.true.))
    call run_test(conv2d_line_t(pad_modulo=4, trim_pad=.true.))
    call run_test(conv2d_line_t(pad_modulo=8, trim_pad=.true.))
    call run_test(conv2d_line_t(pad_modulo=8, trim_pad=.true., use_simd=.true.))

    close(unit=77)

contains

    function diagsum(x)
        real(real32) :: x(:,:)
        real(real32) :: diagsum
        integer :: i

        diagsum = sum([(x(i, i), i = 1, minval(shape(x)))])
    end function


    subroutine run_test_1(test_title, conv)

        character(len=*) :: test_title
        class(conv2d_base_t) :: conv
        real(real32), allocatable :: x(:,:), y(:,:), k(:,:)
        integer :: i, j, l, reps
        real(real64) :: time_total, t1, t2, verif_x, verif_y
        integer, parameter :: array_sizes(*,*) = reshape([ &
            768, 1024, &
            911, 1337, &
            2421, 777, &
            1698, 2142], [4, 2], order=[2, 1])
        integer, parameter :: kernel_sizes(*,*) = reshape([ &
            3,  3,  &
            5,  3,  &
            3,  5,  &
            5,  5,  &
            3,  7,  &
            5,  7,  &
            7,  5,  &
            7,  7,  &
            7,  9,  &
            9,  7,  &
            9,  9,  &
            11, 9,  &
            11, 11, &
            15, 15, &
            17, 17, &
            27, 21, &
            21, 27, &
            27, 27  ], [18, 2], order=[2, 1])
        integer :: output_shape(2)

        time_total = 0
        verif_x = 0
        verif_y = 0

        iter_array_sizes: do i = 1, size(array_sizes, 1)
            iter_kernel_sizes: do j = 1, size(kernel_sizes, 1)

                allocate( &
                    x(array_sizes(i, 1), array_sizes(i, 2)), &
                    k(kernel_sizes(j, 1), kernel_sizes(j, 2)))

                call set_seed(1337)

                call random_number(k)
                call conv % set_kernel(k)
                output_shape = conv % output_shape(array_sizes(i,:))
                allocate(y(output_shape(1), output_shape(2)))

                reps = max(1, nint(1e8 / (product(real(array_sizes(i,:))) &
                    * sqrt(product(real(kernel_sizes(j,:)))))))

                do l = 1, reps
                    call random_number(x)

                    call cpu_time(t1)
                    call conv % conv(x, y)
                    call cpu_time(t2)

                    time_total = time_total + (t2 - t1)
                    verif_x = verif_x + diagsum(x)
                    verif_y = verif_y + diagsum(y)
                end do

                write (77, *) array_sizes(i,:), kernel_sizes(j,:), reps, t2 - t1

                deallocate(x, y, k)

            end do iter_kernel_sizes
        end do iter_array_sizes

        print *, test_title, ' --> '
        print '(a,f8.2,2x,a,f12.0,2x,a,f12.0)', 'time = ', time_total, &
            'verif_x = ', verif_x, 'verif_y = ', verif_y

        write (77, *) '# verif_x =', verif_x, ' verif_y =', verif_y
        write (77, *) '# total =', time_total
        write(77, *) '#'

    end subroutine
end program
