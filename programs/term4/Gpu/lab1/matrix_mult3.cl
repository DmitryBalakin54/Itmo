#define TILE 8
#define X 4
#define vloadX vload4
#define vstoreX vstore4
#define floatX float4

__kernel void matrix_mult(__global const float *a,
                          __global const float *b,
                          __global float *c,
                          const int k,
                          const int n,
                          const int m)
{
    int x = get_global_id(0) * X;
    int y = get_global_id(1) * X;
    int loc_x = get_local_id(0);
    int loc_y = get_local_id(1);

    __local floatX la[TILE][X + 1][TILE + 1];
    __local floatX lb[TILE][X][TILE];

    //float sum[X][X] = {0.0f};
    //float sum[X][X] = {0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f};
    float sum[X][X];

    #pragma unroll
    for (int ix = 0; ix < X; ix++)
    {
        #pragma unroll
        for(int iy = 0; iy < X; iy++)
        {
            sum[ix][iy] = 0.0f;
        }
    }

    for (int i = 0; i < k; i += TILE * X)
    {

        #pragma unroll
        for (int v = 0; v < X; v++)
        {
            la[loc_x][v][loc_y] = vloadX(0, a + (k * (y + v) + loc_x * X + i));
        }

        #pragma unroll
        for (int v = 0; v < X; v++)
        {
            lb[loc_y][v][loc_x] = vloadX(0, b + (k * (x + v) + loc_y * X + i));
        }

        barrier(CLK_LOCAL_MEM_FENCE);

        #pragma unroll
        for (int z = 0; z < TILE; z++)
        {
                #pragma unroll
                for (int ix = 0; ix < X; ix++)
                {
                    #pragma unroll
                    for (int iy = 0; iy < X; iy++)
                    {
                        floatX a_vec = la[z][iy][loc_y];
                        floatX b_vec = lb[z][ix][loc_x];
                        sum[iy][ix] += a_vec.x * b_vec.x + a_vec.y * b_vec.y + a_vec.z * b_vec.z + a_vec.w * b_vec.w;
                    }
                }
        }

        barrier(CLK_LOCAL_MEM_FENCE);

    }

    #pragma unroll
    for (int iy = 0; iy < X; iy++)
    {
        vstoreX((floatX)(sum[iy][0], sum[iy][1], sum[iy][2], sum[iy][3]), 0, c + (y + iy) * n + x);
    }
}