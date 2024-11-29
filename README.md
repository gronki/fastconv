# fastconv

A simple library for 1D and 2D convolutions in Modern Fortran, compatible with [fpm](https://fpm.fortran-lang.org/). Focused on speed in image processing. Optimized for convolutions of 1D, 2D and 3D tensors with small kernels. Currently only implemented for ``real32``.

## Installation

The simplest way to use ``fastconv`` is to add it as dependency in ``fpm.toml``:

```toml
[dependencies]
fastconv = { git = "https://github.com/gronki/fastconv.git" }
```

## Examples

### Convolution 1D

```fortran
program fctest

    use conv1d_m, only: conv1d_t
    use iso_fortran_env, only: fp => real32

    type(conv1d_t) :: conv1d
    real(fp) :: inp(4), out1(2)
    real(fp), allocatable :: out2(:)

    inp(:) = [0, -1, 2, 0]
    call conv1d % set_kernel ([-1.0_fp, 2.0_fp, -1.0_fp])
    conv1d % preserve_shape = .false.

    call conv1d % conv(inp, out1)
    out2 = conv1d % apply(inp)

    print *, "out1 = ", out1
    print *, "shape(out1) = ", shape(out1)
    print *, "out1 == out2 = ", out1 == out2

    conv1d % preserve_shape = .true.
    out2 = conv1d % apply(inp)
    print *, "out2 = ", out2
    print *, "shape(out2) = ", shape(out2)

end program
```

Output:

```
 out1 =   -4.00000000       5.00000000    
 shape(out1) =            2
 out1 == out2 =  T T
 out2 =    5.54353672E-42  -4.00000000       5.00000000       0.00000000    
 shape(out2) =            4
```

## Benchmarks

Benchmarks are available that allow to scan through setting space and choose the most optimal convolution parameters. Can be run either by

```sh
./run.sh gcc
./run.sh oneapi
```

To print more detailed statistics:

``export EXTRA_OUTPUT=1``

or by building and executing Docker image:

```
docker build -t fastconv --build-arg FFLAGS="-O3 -funsafe-math-optimizations -funroll-loops -ffree-line-length-none -march=native" https://github.com/gronki/fastconv.git
docker run -it fastconv
```

You should get the output like:

```
 conv1d_ref_t() --> 
time =    12.62  verif_x =       3142758373.  verif_y =       3117590248.
 conv1d_pad_t(pad_modulo=4) --> 
time =     9.13  verif_x =       3142758373.  verif_y =       3117590248.
 conv1d_pad_t(pad_modulo=8) --> 
time =     9.47  verif_x =       3142758373.  verif_y =       3117590248.
...
```
