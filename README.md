# fastconv

A simple library for 1D and 2D convolutions in Modern Fortran.

## Benchmarks

Performed on ``11th Gen Intel(R) Core(TM) i5-1135G7 @ 2.40GHz``:

```
 conv1d_ref_t() --> 
time =    11.38  verif_x =       2492705612.  verif_y =       9192495419.
 conv1d_ref_t(preserve_shape=.true.) --> 
time =    11.38  verif_x =       2492705612.  verif_y =       9192495419.
 conv1d_t() --> 
time =     7.18  verif_x =       2492705612.  verif_y =       9192495419.
 conv1d_t(preserve_shape=.true.) --> 
time =     7.14  verif_x =       2492705612.  verif_y =       9192495419.
 conv1d_pad_t(pad_modulo=4) --> 
time =     7.19  verif_x =       2492705612.  verif_y =       9192495419.
 conv1d_pad_t(pad_modulo=4, use_simd=.true.) --> 
time =     7.93  verif_x =       2492705612.  verif_y =       9192495419.
 conv1d_pad_t(pad_modulo=8) --> 
time =     7.13  verif_x =       2492705612.  verif_y =       9192495419.
 conv1d_pad_t(pad_modulo=8, use_simd=.true.) --> 
time =     8.34  verif_x =       2492705612.  verif_y =       9192495419.
 conv1d_pad_t(pad_modulo=16) --> 
time =     8.21  verif_x =       2492705612.  verif_y =       9192495419.
 conv2d_ref_t() --> 
time =    22.24  verif_x =   507344962.  verif_y =   180885359.
 conv2d_ref_t(preserve_shape=.true.) --> 
time =    22.37  verif_x =   507344962.  verif_y =   180885359.
 conv2d_t() --> 
time =     9.90  verif_x =   507344962.  verif_y =   180885359.
 conv2d_t(preserve_shape=.true.) --> 
time =     9.85  verif_x =   507344962.  verif_y =   180885359.
 conv2d_line_t(conv1d_pad_t(pad_modulo=4)) --> 
time =     9.81  verif_x =   507344962.  verif_y =   180885359.
 conv2d_line_t(conv1d_pad_t(pad_modulo=4, use_simd=.true.)) --> 
time =    11.31  verif_x =   507344962.  verif_y =   180885359.
 conv2d_line_t(conv1d_pad_t(pad_modulo=8)) --> 
time =     9.71  verif_x =   507344962.  verif_y =   180885359.
 conv2d_line_t(conv1d_pad_t(pad_modulo=8, use_simd=.true.)) --> 
time =    11.30  verif_x =   507344962.  verif_y =   180885359.
```

## Run benchmarks on your computer

```bash
docker build -t fastconv https://github.com/gronki/fastconv.git
docker run -it fastconv
```