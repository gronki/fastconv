program test_conv3d

    use testing_m
    use iso_fortran_env
    use conv3d_m
    use conv1d_m

    implicit none

#   ifdef __GFORTRAN__
#       define QUOTE(v) "v"
#   else
#       define QUOTE(v) #v
#   endif
#   define run_test(x) run_test_1(QUOTE(x), (x))

    open(unit=77, file='test_conv3d_stats.txt', action='write')

    call run_test(conv3d_ref_t())
    call run_test(conv3d_line_t(conv1d_ref_t()))
    call run_test(conv3d_line_t(conv1d_pad_t(pad_modulo=4)))
    call run_test(conv3d_line_t(conv1d_pad_t(pad_modulo=8)))
    call run_test(conv3d_line_t(conv1d_pad_t(pad_modulo=16)))

    close(unit=77)

contains

    subroutine run_test_1(test_title, conv)

        character(len=*) :: test_title
        class(conv3d_base_t) :: conv
        real(real32), allocatable :: x(:,:,:), y(:,:,:), k(:,:,:)
        integer :: i, j, l, reps
        real(real64) :: time_total, t1, t2, verif_x, verif_y
        integer(int64), parameter :: array_sizes(*,*) = reshape([ &
            64, 55, 33, &
            99, 77, 22, &
            45, 41, 65, &
            128, 111, 121, &
            167, 173, 211 &
        ], [5, 3], order=[2, 1])
        integer(int64), parameter :: kernel_sizes(*,*) = reshape([ &
            3,  3,  3,  &
            3,  5,  3,  &
            5,  3,  3,  &
            5,  7,  7,  &
            11, 13, 11, &
            19, 13, 17  &
        ], [6, 3], order=[2, 1])
        integer(int64) :: output_shape(3)

        time_total = 0
        verif_x = 0
        verif_y = 0

        iter_array_sizes: do i = 1, size(array_sizes, 1)

            allocate(x(array_sizes(i, 1), array_sizes(i, 2), array_sizes(i, 3)))

            iter_kernel_sizes: do j = 1, size(kernel_sizes, 1)

                allocate(k(kernel_sizes(j, 1), kernel_sizes(j, 2), kernel_sizes(j, 3)))

                call set_seed(1337)

                call random_number(k)
                call conv % set_kernel(k / sum(k))
                
                output_shape = conv % output_shape(array_sizes(i,:))
                allocate(y(output_shape(1), output_shape(2), output_shape(3)), source=0.)

                reps = max(1, nint(3e7 / (product(real(array_sizes(i,:))) &
                    * sqrt(product(real(kernel_sizes(j,:)))))))

                do l = 1, reps
                    call random_number(x)

                    call cpu_time(t1)
                    call conv % conv(x, y)
                    call cpu_time(t2)

                    time_total = time_total + (t2 - t1)
                    verif_x = verif_x + sum(real(x, kind=real64))
                    verif_y = verif_y + sum(real(y, kind=real64)) * 1e-2
                end do

                write (77, *) array_sizes(i,:), kernel_sizes(j,:), reps, t2 - t1

                deallocate(y, k)

            end do iter_kernel_sizes

            deallocate(x)
        end do iter_array_sizes

        print *, test_title, ' --> '
        print '(a,f8.2,2x,a,f12.0,2x,a,f12.0)', 'time = ', time_total, &
            'verif_x = ', verif_x, 'verif_y = ', verif_y

        write (77, *) '# verif_x =', verif_x, ' verif_y =', verif_y
        write (77, *) '# total =', time_total
        write(77, *) '#'

    end subroutine
end program
