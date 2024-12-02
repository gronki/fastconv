VENDOR=${1}

if [ "$VENDOR" == "gcc" ]; then
	# gcc version
	fpm run --example test_conv1d --compiler gfortran --flag '-O3 -flto -march=native -ffast-math -g -DNDEBUG'
elif [ "$VENDOR" == "oneapi" ]; then
	# Intel oneApi version
	fpm run --example test_conv1d --compiler ifx --flag '-g -O3 -fp-model=fast -xHost -DNDEBUG'
else
	echo "usage: $0 [gcc/oneapi]"
fi
