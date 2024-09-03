#define TILE 16
/*
kernel void matrix_mult(global const float *a,
                          global const float *b,
                          global float *c,
                           const int k,
                           const int n,
                           const int m)
{

    int x = get_global_id(0);
    int y = get_global_id(1);

    local float lb[TILE][TILE];
    local float la[TILE][TILE];

    int loc_x = get_local_id(0);
    int loc_y = get_local_id(1);

    float sum = 0.0f;

    for (int i = 0; i < k; i += TILE)
    {

        la[loc_y][loc_x] = a[k * y + (loc_x + i)];
        lb[loc_y][loc_x] = b[n * (loc_y + i) + x];


        barrier(CLK_LOCAL_MEM_FENCE);

        #pragma unroll
        for (int z = 0; z < TILE; z++)
        {
            sum += la[loc_y][z] * lb[z][loc_x];
        }

        barrier(CLK_LOCAL_MEM_FENCE);
    }


     c[y * n + x] = sum;
}*/

kernel void matrix_mult(global const float *a,
                          global const float *b,
                          global float *c,
                           const int k,
                           const int n,
                           const int m)
{

    int x = get_global_id(0);
    int y = get_global_id(1);

    local float lb[TILE][TILE];
    local float la[TILE][TILE];

    int loc_x = get_local_id(0);
    int loc_y = get_local_id(1);

    float sum = 0.0f;

    for (int i = 0; i < k; i += TILE)
    {

        la[loc_y][loc_x] = a[k * y + (loc_x + i)];
        lb[loc_y][loc_x] = b[n * (loc_y + i) + x];


        barrier(CLK_LOCAL_MEM_FENCE);

        #pragma unroll
        for (int z = 0; z < TILE; z++)
        {
            sum += la[loc_y][z] * lb[z][loc_x];
        }

        barrier(CLK_LOCAL_MEM_FENCE);
    }


     c[y * n + x] = sum;
}