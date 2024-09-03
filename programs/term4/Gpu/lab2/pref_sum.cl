#define TILE 256

__kernel void first(__global float *arr, const int n, const int offset)
{

    int x = get_global_id(0);
    int loc_x = get_local_id(0);

    __local float loc_arr[TILE * 2];

    int i1 = (2 * x + 1) * offset - 1;
    int i2 = (2 * x + 2) * offset - 1;

    if (i2 < n && i2 > 0)
    {
        loc_arr[2 * loc_x] = arr[i1];
        loc_arr[2 * loc_x + 1] = arr[i2];
    }
    else if (i1 < n && i1 >= 0)
    {
        loc_arr[2 * loc_x] = arr[i1];
        loc_arr[2 * loc_x + 1] = 0.0f;
    }
    else
    {
        loc_arr[2 * loc_x] = 0.0f;
        loc_arr[2 * loc_x + 1] = 0.0f;
    }

    #pragma unroll
    for (int v = 2; v < TILE * 2 + 1; v *= 2)
    {
        barrier(CLK_LOCAL_MEM_FENCE);
        int l = v / 2 - 1 + (loc_x / (v / 2)) * v;
        int r = l + loc_x % (v / 2) + 1;

        loc_arr[r] += loc_arr[l];
    }

    barrier(CLK_LOCAL_MEM_FENCE);


    if (i2 < n && i2 > 0)
    {
        arr[i1] = loc_arr[2 * loc_x];
        arr[i2] = loc_arr[2 * loc_x + 1];
    }
    else if (i1 < n && i1 >= 0)
    {
        arr[i1] = loc_arr[2 * loc_x];
    }
}




__kernel void second(__global float *arr, const int n, const int offset)
{
    int x = get_global_id(0);
    int loc_x = get_local_id(0);

    int i1 = (2 * x + 1) * offset - 1;
    int i2 = (2 * x + 2) * offset - 1;
    int tl = TILE * 2 * offset;
    int pind = i1 / tl * tl - 1;

    if (i2 > tl - 1 && i2 > 0 && tl > 0 && pind >= 0 && pind < n)
    {
        float prev = arr[pind];

        if (i2 < n && loc_x < TILE - 1 && i2 > 0)
        {
            arr[i1] += prev;
            arr[i2] += prev;
        }
        else if (i1 < n && i1 >= 0)
        {
            arr[i1] += prev;
        }
    }
}