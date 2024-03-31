FROM debian:bookworm-slim

RUN apt-get update && apt-get install -y --no-install-recommends gfortran && apt-get clean

WORKDIR /fpm
ADD https://github.com/fortran-lang/fpm/releases/download/v0.10.0/fpm-0.10.0.F90 fpm.F90
RUN gfortran -O0 fpm.F90 -o fpm && install fpm /usr/local/bin/

COPY . /src/fastconv

WORKDIR /src/fastconv

ENV FPM_FC=gfortran
ARG FPM_FFLAGS="-O3 -funsafe-math-optimizations -ffree-line-length-none -march=native"
ENV FPM_FFLAGS="${FPM_FFLAGS}"
RUN fpm build --tests --verbose

ENTRYPOINT fpm run --example test_conv1d \
     && fpm run --example test_conv2d \
     && fpm run --example test_conv3d 
