#include <stdint.h>
#include <stdio.h>
#include <assert.h>

#if defined(__x86_64__)
#include <x86intrin.h>
#endif

#if defined(__ARM_NEON__)
#include <arm_neon.h>
#endif

#if defined(__AVX2__)
void convolution_avx2(const float *input, float *output, size_t output_length,
                      const float *kernel, size_t kernel_length)
{
    assert((kernel_length % 8) == 0);

    for (size_t i = 0; i < output_length; ++i)
    {
        __m256 sum = _mm256_setzero_ps();
        for (size_t j = 0; j < kernel_length; j += 8)
        {
            __m256 input_vec = _mm256_loadu_ps(input + i + j);
            __m256 kernel_vec = _mm256_load_ps(kernel + j);
            sum = _mm256_fmadd_ps(input_vec, kernel_vec, sum);
        }

        // Horizontally add the 8 floats in the sum vector
        __m128 sum_low = _mm256_extractf128_ps(sum, 0);
        __m128 sum_high = _mm256_extractf128_ps(sum, 1);
        __m128 result = _mm_add_ps(sum_low, sum_high);
        result = _mm_hadd_ps(result, result);
        result = _mm_hadd_ps(result, result);

        output[i] = _mm_cvtss_f32(result);
    }
}
#endif

#if defined(__SSE3__)
void convolution_sse(const float *input, float *output, size_t output_length,
                     const float *kernel, size_t kernel_length)
{
    assert((kernel_length % 4) == 0);

    for (size_t i = 0; i < output_length; ++i)
    {
        __m128 sum = _mm_setzero_ps();
        for (size_t j = 0; j < kernel_length; j += 4)
        {
            __m128 vx = _mm_loadu_ps(&input[i + j]);
            __m128 vk = _mm_load_ps(&kernel[j]);
            sum = _mm_fmadd_ps(vx, vk, sum);
        }
        sum = _mm_hadd_ps(sum, sum);
        sum = _mm_hadd_ps(sum, sum);
        _mm_store_ss(&output[i], sum);
    }
}
#endif

#if defined(__ARM_NEON__)
void convolution_a53(const float *x, float *y, size_t n, const float *kernel, size_t kernel_length)
{
    assert((kernel_length % 4) == 0);
    for (size_t i = 0; i < n; ++i)
    {
        float32x4_t sum = vdupq_n_f32(0.0f);
        for (size_t j = 0; j < kernel_length; j += 4)
        {
            float32x4_t vx = vld1q_f32(&x[i + j]);
            float32x4_t vk = vld1q_f32(&kernel[j]);
            sum = vmlaq_f32(sum, vx, vk);
        }
        y[i] = vaddvq_f32(sum);
    }
}
#endif

void convolution_simd(const float *input, float *output, size_t output_length,
                      const float *kernel, size_t kernel_length, int *status)
{

#if defined(__AVX2__)
    if ((kernel_length % 8) == 0 && __builtin_cpu_supports("avx2"))
    {
        convolution_avx2(input, output, output_length, kernel, kernel_length);
        *status = 2;
        return;
    }
#endif

#if defined(__SSE3__)
    if ((kernel_length % 4) == 0 && __builtin_cpu_supports("sse3"))
    {
        convolution_sse(input, output, output_length, kernel, kernel_length);
        *status = 1;
        return;
    }
#endif

#if defined(__ARM_NEON__)
    if ((kernel_length % 4) == 0)
    {
        convolution_a53(input, output, output_length, kernel, kernel_length);
        *status = 51;
        return;
    }
#endif

    *status = 0;
}
