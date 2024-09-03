__kernel void matrix_mult(__global const float *a,
                          __global const float *b,
                          __global float *c,
                          const int k,
                          const int n,
                          const int m)
{
    uint x = get_global_id(0);
    uint y = get_global_id(1);


    float sum = 0.0f;

    for (uint i = 0; i < k; i++)
    {
        sum += a[k * y + i] * b[i * n + x];
    }

    c[y * n + x] = sum;
}