module conv_base_m

    use iso_fortran_env

    implicit none (type, external)
    private

    type, abstract :: conv_base_t
        logical :: preserve_shape = .false.
    end type

    integer, parameter :: size_k = int64

    public :: conv_base_t, size_k

end module
