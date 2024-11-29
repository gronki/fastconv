program fastconv_test

   use iso_fortran_env
   use testing_m
   use conv1d_m

   implicit none

   integer :: i
   integer(int64), parameter :: array_sizes(*) = [10000_int64, 100000_int64, 1000000_int64, 10000000_int64]
   integer(int64), parameter :: kernel_sizes(*) = [3, 4, 5, 6, 7, 8, 9, 11, 13, 15, 19, 25, 31]
   integer, parameter :: fp = real32
   logical :: extra_output

#   ifdef __GFORTRAN__
#       define QUOTE(v) "v"
#   else
#       define QUOTE(v) #v
#   endif
#   define run_test(x) run_test_1(QUOTE(x), (x))

   call read_env

   print '(a, *(i0,:,", "))', "array sizes: ", array_sizes
   print '(a, *(i0,:,", "))', "kernel sizes: ", kernel_sizes

   call run_test(conv1d_ref_t())
   call run_test(conv1d_pad_t(pad_modulo=4))
   call run_test(conv1d_pad_t(pad_modulo=8))
   call run_test(conv1d_pad_t(pad_modulo=16))

contains

   subroutine read_env
      character(len=4) :: buf
      integer :: i
      call get_environment_variable("EXTRA_OUTPUT", value=buf)
      if (buf == "") then
         extra_output = .false.
         return
      end if
      read (buf, *) i
      extra_output = i /= 0
   end subroutine

   subroutine run_test_1(test_title, conv)

      class(conv1d_base_t) :: conv
      character(len=*) :: test_title
      real(fp), allocatable :: x(:), y(:), k(:)
      integer :: i, j, l, reps
      real(real64) :: time_total, time_onesize, t1, t2, verif_x, verif_y, time_avg
      integer(int64) :: array_size, kernel_size

      time_total = 0
      verif_x = 0
      verif_y = 0
      time_avg = 0

      iter_array_sizes: do i = 1, size(array_sizes)
         array_size = array_sizes(i)
         allocate(x(array_size))

         iter_kernel_sizes: do j = 1, size(kernel_sizes)

            kernel_size = kernel_sizes(j)
            allocate(k(kernel_size))

            call fill_array(k)
            call conv % set_kernel(k)

            allocate(y(conv % output_size(size(x, kind=int64))), source=0.)

            reps = max(1, nint(&
               7 * (real(maxval(array_sizes))**0.8 * real(maxval(kernel_sizes))**0.5) &
               / (real(array_size)**0.8 * real(kernel_size)**0.5)&
               ))

            time_onesize = 0

            do l = 1, reps
               call fill_array(x)

               call cpu_time(t1)
               call conv % conv(x, y)
               call cpu_time(t2)

               time_total = time_total + (t2 - t1)
               time_onesize = time_onesize + (t2 - t1)
               verif_x = verif_x + sum(real(x, kind=real64))
               verif_y = verif_y + sum(real(y, kind=real64))
            end do

            time_onesize = time_onesize / reps * 1e9_real64 / array_size
            time_avg = time_avg + time_onesize

            if (extra_output) then
               print '("array = ",I0,"; kernel = ",I0,"; reps = ",I0,";  t =",f8.5)', &
                  array_size, kernel_size, reps, time_onesize
            end if

            deallocate(y, k)

         end do iter_kernel_sizes

         deallocate(x)

      end do iter_array_sizes

      time_avg = time_avg / size(array_sizes) / size(kernel_sizes)

      print '(a,a,a,f5.2,a,f8.5,a,f17.0,a,f17.0)', test_title, ' --> ', &
         'time = ', time_total, '; sec/GOps = ', time_avg, &
         '; verif_x = ', verif_x, '; verif_y = ', verif_y

   end subroutine

   pure subroutine fill_array(x)
      real, intent(inout) :: x(:)
      integer(int64) :: i

      do i = 1, size(x)
         x(i) = sin(0.072 * (i - 1)) + sin(0.0013 * (i - 1)) + sin(0.02 * (i - 1))
      end do
   end subroutine
end program
