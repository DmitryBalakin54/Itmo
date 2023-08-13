#include "return_codes.h"

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

typedef float fc;

fc delta_;

fc filter(const fc value)
{
	if (-delta_ <= value && value <= delta_)
	{
		return 0;
	}
	return value;
}

fc final_filter(fc value)
{
	if (!filter(roundf(value) - value))
	{
		value = roundf(value);
	}
	return value;
}

size_t index_of_matrix(size_t x, size_t y, size_t size)
{
	return y * size + x;
}

void print_matrix(fc *matrix, size_t n, char *name)
{
	printf("-----\n%s:\n", name);
	for (size_t y = 0; y < n; y++)
	{
		for (size_t x = 0; x < n; x++)
		{
			printf("%f ", matrix[index_of_matrix(x, y, n)]);
		}
		printf("\n");
	}
	printf("-----\n");
}

void shift(fc *A, size_t n, fc shift)
{
	for (size_t i = 0; i < n; i++)
	{
		A[index_of_matrix(i, i, n)] += shift;
	}
}

void Givens_multiply_R(fc *A, size_t n, const fc *G, size_t str)
{
	for (size_t i = 0; i < n; i++)
	{
		fc first = G[index_of_matrix(0, 0, 2)] * A[index_of_matrix(i, str, n)] +
				   G[index_of_matrix(1, 0, 2)] * A[index_of_matrix(i, str + 1, n)];
		fc second = G[index_of_matrix(0, 1, 2)] * A[index_of_matrix(i, str, n)] +
					G[index_of_matrix(1, 1, 2)] * A[index_of_matrix(i, str + 1, n)];
		A[index_of_matrix(i, str, n)] = first;
		A[index_of_matrix(i, str + 1, n)] = second;
	}
}

void Givens_algorithm_R(fc *A, size_t n, fc *G_arrays)
{
	for (size_t i = 0; i < n - 1; i++)
	{
		size_t j = i + 1;
		fc G[4];
		fc a = A[index_of_matrix(i, i, n)];
		fc c = A[index_of_matrix(i, j, n)];

		fc sin = c / sqrtf(a * a + c * c);
		fc cos = a / sqrtf(a * a + c * c);

		if (!a && !c)
		{
			sin = 0;
			cos = 1;
		}

		G[index_of_matrix(0, 0, 2)] = cos;
		G[index_of_matrix(1, 0, 2)] = sin;
		G[index_of_matrix(0, 1, 2)] = -sin;
		G[index_of_matrix(1, 1, 2)] = cos;

		G_arrays[2 * i] = G[index_of_matrix(0, 0, 2)];
		G_arrays[2 * i + 1] = G[index_of_matrix(0, 1, 2)];
		G_arrays[2 * (n - 1) + 2 * i] = G[index_of_matrix(1, 0, 2)];
		G_arrays[2 * (n - 1) + 2 * i + 1] = G[index_of_matrix(1, 1, 2)];

		Givens_multiply_R(A, n, G, i);
	}
}

void Givens_multiply_Q(fc *A, size_t n, const fc *G, size_t col)
{
	for (size_t i = 0; i < n; i++)
	{
		fc first = G[index_of_matrix(0, 0, 2)] * A[index_of_matrix(col, i, n)] +
				   G[index_of_matrix(0, 1, 2)] * A[index_of_matrix(col + 1, i, n)];
		fc second = G[index_of_matrix(1, 0, 2)] * A[index_of_matrix(col, i, n)] +
					G[index_of_matrix(1, 1, 2)] * A[index_of_matrix(col + 1, i, n)];
		A[index_of_matrix(col, i, n)] = first;
		A[index_of_matrix(col + 1, i, n)] = second;
	}
}

void Givens_algorithm_Q(fc *A, size_t n, const fc *G_arrays)
{
	for (size_t i = 0; i < n - 1; i++)
	{
		fc G[4];
		G[index_of_matrix(0, 0, 2)] = G_arrays[2 * i];
		G[index_of_matrix(1, 0, 2)] = G_arrays[2 * i + 1];
		G[index_of_matrix(0, 1, 2)] = G_arrays[2 * (n - 1) + 2 * i];
		G[index_of_matrix(1, 1, 2)] = G_arrays[2 * (n - 1) + 2 * i + 1];

		Givens_multiply_Q(A, n, G, i);
	}
}

void make_start_matrix(fc *A, size_t n)
{
	fc cos;
	fc sin;
	fc tmp;

	for (size_t k = 0; k < n - 2; k++)
	{
		for (size_t i = k + 2; i < n; i++)
		{
			fc denominator =
				sqrtf(A[index_of_matrix(k, k + 1, n)] * A[index_of_matrix(k, k + 1, n)] +
					  A[index_of_matrix(k, i, n)] * A[index_of_matrix(k, i, n)]);

			if (filter(denominator))
			{
				cos = A[index_of_matrix(k, k + 1, n)] / denominator;
				sin = A[index_of_matrix(k, i, n)] / denominator;
			}
			else
			{
				cos = 1;
				sin = 0;
			}

			for (size_t j = k; j < n; j++)
			{
				tmp = -sin * A[index_of_matrix(j, k + 1, n)] + cos * A[index_of_matrix(j, i, n)];
				A[index_of_matrix(j, k + 1, n)] = cos * A[index_of_matrix(j, k + 1, n)] + sin * A[index_of_matrix(j, i, n)];
				A[index_of_matrix(j, i, n)] = tmp;
			}

			for (size_t j = 0; j < n; j++)
			{
				tmp = -sin * A[index_of_matrix(k + 1, j, n)] + cos * A[index_of_matrix(i, j, n)];
				A[index_of_matrix(k + 1, j, n)] = cos * A[index_of_matrix(k + 1, j, n)] + sin * A[index_of_matrix(i, j, n)];
				A[index_of_matrix(i, j, n)] = tmp;
			}
		}
	}
}

void Givens_QR_algorithm(fc *A, size_t n, size_t max_iteration, fc *G)
{
	make_start_matrix(A, n);
	for (size_t iteration = 0; iteration < max_iteration; iteration++)
	{
		if (iteration < 3)
		{
			Givens_algorithm_R(A, n, G);
			Givens_algorithm_Q(A, n, G);
			continue;
		}
		fc value = A[index_of_matrix(n - 1, n - 1, n)];
		shift(A, n, -value);
		Givens_algorithm_R(A, n, G);
		Givens_algorithm_Q(A, n, G);
		shift(A, n, value);
	}
}

int main(int argc, char *argv[])
{
	if (argc != 3)
	{
		fprintf(stderr, "Invalid arguments parameter\n expected: 3\n actual: %d\n", argc);
		return ERROR_PARAMETER_INVALID;
	}

	FILE *in = fopen(argv[1], "r");

	if (!in)
	{
		fprintf(stderr, "File %s is not found\n", argv[1]);
		return ERROR_CANNOT_OPEN_FILE;
	}

	size_t n;
	fscanf(in, "%zi", &n);
	fc min;
	fc max;

	fc *matrix = malloc(sizeof(fc) * n * n);

	if (!matrix)
	{
		fprintf(stderr, "Not enough memory\n");
		fclose(in);
		return ERROR_OUT_OF_MEMORY;
	}

	for (size_t y = 0; y < n; y++)
	{
		for (size_t x = 0; x < n; x++)
		{
			fscanf(in, "%f", &matrix[index_of_matrix(x, y, n)]);
			if (!y && !x)
			{
				max = matrix[index_of_matrix(x, y, n)];
				min = max;
			}
			else
			{
				max = matrix[index_of_matrix(x, y, n)] > max ? matrix[index_of_matrix(x, y, n)] : max;
				min = matrix[index_of_matrix(x, y, n)] < min ? matrix[index_of_matrix(x, y, n)] : min;
			}
		}
	}
	fclose(in);

	delta_ = fabsf(min / 100) + 0.000001f;

	fc *G = malloc(sizeof(fc) * (n - 1) * 4);	 // 2(n - 1) * 2
	if (!G)
	{
		fprintf(stderr, "Not enough memory\n");
		free(matrix);
		return ERROR_OUT_OF_MEMORY;
	}

	size_t iterations = n >= 500 ? 250 : 500;
	Givens_QR_algorithm(matrix, n, iterations, G);

	free(G);

	FILE *out = fopen(argv[2], "w");
	if (!out)
	{
		fprintf(stderr, "couldn't create or open output file: %s\n", argv[2]);
		free(matrix);
		return ERROR_CANNOT_OPEN_FILE;
	}

	for (size_t i = 0; i < n; i++)
	{
		if (i < n - 1)
		{
			if (filter(matrix[index_of_matrix(i, i + 1, n)]))
			{
				//                |a - t  b    |
				//                |c      d - t|
				//                (a - t)(d - t) - cb = t^2 + t(-a - d) + (ad - cb) = 0
				//                x_(1,2) = ( a + d +- sqrt(4(ad - cb) - (a + d)^2 )i ) / 2 = (a + d) / 2 +-  i sqrt((ad
				//                - cb) - ((a + d) / 2)^2)i

				fc a = matrix[index_of_matrix(i, i, n)];
				fc b = matrix[index_of_matrix(i + 1, i, n)];
				fc c = matrix[index_of_matrix(i, i + 1, n)];
				fc d = matrix[index_of_matrix(i + 1, i + 1, n)];

				fc re = (a + d) / 2;
				fc im = sqrtf((a * d - c * b) - re * re);

				re = final_filter(re);
				im = final_filter(im);

				if (isnan(im) || !im)
				{
					goto print;
				}

				fprintf(out, "%g +%gi\n", re, im);
				fprintf(out, "%g -%gi\n", re, im);
				i++;

				continue;
			}
		}
print:;
		fc re = matrix[index_of_matrix(i, i, n)];
		re = final_filter(re);
		fprintf(out, "%g\n", re);
	}
	fclose(out);
	free(matrix);
	return SUCCESS;
}