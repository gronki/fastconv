program fastconv_test

    use iso_fortran_env
    use testing_m
    use conv1d_m

    implicit none

    integer :: i
    integer(int64), parameter :: array_sizes(*) = [(nint(100 * 1.9**real(i), kind=int64), i = 0, 12)]
    integer(int64), parameter :: kernel_sizes(*) = [3, 4, 5, 6, 7, 8, 9, 11, 13, 15] !, 17, 19, 21, 24, 27, 33]
    integer, parameter :: fp = real32

#   ifdef __GFORTRAN__
#       define QUOTE(v) "v"
#   else
#       define QUOTE(v) #v
#   endif
#   define run_test(x) run_test_1(QUOTE(x), (x))

    call run_test(conv1d_ref_t())
    call run_test(conv1d_ref_t(preserve_shape=.true.))
    call run_test(conv1d_t())
    call run_test(conv1d_t(preserve_shape=.true.))

    call run_test(conv1d_pad_t(pad_modulo=4))
    call run_test(conv1d_pad_t(pad_modulo=4, use_simd=.true.))
    call run_test(conv1d_pad_t(pad_modulo=8))
    call run_test(conv1d_pad_t(pad_modulo=8, use_simd=.true.))
    call run_test(conv1d_pad_t(pad_modulo=16))

contains

    subroutine run_test_1(test_title, conv)

        class(conv1d_base_t) :: conv
        character(len=*) :: test_title
        real(fp), allocatable :: x(:), y(:), k(:)
        integer :: i, j, l, reps
        real(real64) :: time_total, t1, t2, verif_x, verif_y

        time_total = 0
        verif_x = 0
        verif_y = 0

        iter_array_sizes: do i = 1, size(array_sizes)

            allocate(x(array_sizes(i)))

            iter_kernel_sizes: do j = 1, size(kernel_sizes)

                allocate(k(kernel_sizes(j)))

                call set_seed(1337)

                call random_number(k)
                call conv % set_kernel(k)

                allocate(y(conv % output_shape(size(x, kind=int64))), source=0.)

                reps = max(1, nint(1e8 / (real(array_sizes(i)) * sqrt(real(kernel_sizes(j))))))

                do l = 1, reps
                    call random_number(x)

                    call cpu_time(t1)
                    call conv % conv(x, y)
                    call cpu_time(t2)

                    time_total = time_total + (t2 - t1)
                    verif_x = verif_x + sum(real(x, kind=real64))
                    verif_y = verif_y + sum(real(y, kind=real64))
                end do


                deallocate(y, k)

            end do iter_kernel_sizes

            deallocate(x)
            
        end do iter_array_sizes

        print *, test_title, ' --> '
        print '(a,f8.2,2x,a,f17.0,2x,a,f17.0)', 'time = ', time_total, &
            'verif_x = ', verif_x, 'verif_y = ', verif_y
            
    end subroutine

end program
