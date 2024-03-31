module testing_m

    use iso_fortran_env
    implicit none


contains

    subroutine set_seed(seed)
        integer, allocatable :: seed_arr(:)
        integer :: n, seed
        call random_seed(size = n)
        allocate(seed_arr(n))
        seed_arr(:) = seed
        call random_seed(put=seed_arr)
    end subroutine

end module
