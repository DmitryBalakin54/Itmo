#include "return_codes.h"

#include <malloc.h>
#include <stdio.h>

#define IHDR 0
#define make_IHDR_array                                                                                                \
	name[0] = 73;                                                                                                      \
	name[1] = 72;                                                                                                      \
	name[2] = 68;                                                                                                      \
	name[3] = 82;

#define PLTE 10
#define make_PLTE_array                                                                                                \
	name[0] = 80;                                                                                                      \
	name[1] = 76;                                                                                                      \
	name[2] = 84;                                                                                                      \
	name[3] = 69;

#define IDAT 20
#define make_IDAT_array                                                                                                \
	name[0] = 73;                                                                                                      \
	name[1] = 68;                                                                                                      \
	name[2] = 65;                                                                                                      \
	name[3] = 84;

#define IEND 30
#define make_IEND_array                                                                                                \
	name[0] = 73;                                                                                                      \
	name[1] = 69;                                                                                                      \
	name[2] = 78;                                                                                                      \
	name[3] = 68;

#define PNG_signature { 137, 80, 78, 71, 13, 10, 26, 10 };

#define true 1
#define false 0

#if defined ZLIB

#include "zlib.h"

#elif defined ISAL

#include <include/igzip_lib.h>

#elif defined LIBDEFLATE

#include <libdeflate.h>

#else

#error unsupported lib, you can use zlib, libdeflate or isa-l.

#endif

typedef unsigned int uint;
typedef unsigned char byte;

typedef struct chunk
{
	uint length;
	byte type[4];
	uint crc;
	byte data[];
} chunk;

int OUT_OF_MEMORY_ERROR = false;

void print_chunk(chunk *chunk, char *name)
{
	printf("chunk %s---\n", name);
	printf("length = %u\n", chunk->length);
	printf("type = [%d, %d, %d, %d]\n", chunk->type[0], chunk->type[1], chunk->type[2], chunk->type[3]);
	printf("data: ");
	for (uint i = 0; i < chunk->length; i++)
	{
		printf("%d ", chunk->data[i]);
	}
	printf("\n");
	printf("crc = %u\n-------------\n", chunk->crc);
}

void fprint_chunk(chunk *chunk, char *name, FILE *out)
{
	fprintf(out, "chunk %s---\n", name);
	fprintf(out, "length = %u\n", chunk->length);
	fprintf(out, "type = [%d, %d, %d, %d]\n", chunk->type[0], chunk->type[1], chunk->type[2], chunk->type[3]);
	fprintf(out, "data: ");
	for (uint i = 0; i < chunk->length; i++)
	{
		fprintf(out, "%d ", chunk->data[i]);
	}
	fprintf(out, "\n");
	fprintf(out, "crc = %u\n-------------\n", chunk->crc);
}

uint make_uint(const byte *data, size_t len)
{
	uint res = 0;
	uint val = 1;
	for (size_t i = 0; i < len; i++)
	{
		res += data[(len - 1) - i] * val;
		val *= 256;
	}
	return res;
}

int compare_chunk_name(chunk *chunk, int code)
{
	byte name[4];
	switch (code)
	{
	case IHDR:
		make_IHDR_array break;
	case PLTE:
		make_PLTE_array break;
	case IDAT:
		make_IDAT_array break;
	case IEND:
		make_IEND_array break;
	default:
		return false;
	}

	for (size_t i = 0; i < 4; i++)
	{
		if (chunk->type[i] != name[i])
		{
			return false;
		}
	}
	return true;
}

int read_type(FILE *in)
{
	byte name[8];

	if (fread(&name, sizeof(*name), 8, in) < 8)
	{
		return false;
	}

	byte signature[8] = PNG_signature for (size_t i = 0; i < 8; i++)
	{
		if (name[i] != signature[i])
		{
			return false;
		}
	}
	return true;
}

chunk *read_chunk(FILE *in)
{
	byte tmp[4];

	fread(&tmp, sizeof(*tmp), 4, in);
	uint len = make_uint(tmp, 4);

	chunk *res = malloc(sizeof(chunk) + sizeof(byte) * len);

	if (!res)
	{
		// error
		OUT_OF_MEMORY_ERROR = true;
		return NULL;
	}

	res->length = len;
	fread(&(res->type), sizeof(*res->type), 4, in);

	fread(res->data, sizeof(byte), len, in);

	byte crc_tmp[4];
	if (fread(&crc_tmp, sizeof(byte), 4, in) < 4)
	{
		return NULL;
	}
	res->crc = make_uint(crc_tmp, 4);

	return res;
}

int check_IHDR(chunk *chunk)
{
	if (chunk->length != 13 || chunk->data[10] || chunk->data[11] || chunk->data[12])
	{
		return false;
	}

	if ((!chunk->data[0] || !chunk->data[1] || !chunk->data[2] || !chunk->data[3]) &&
		(!chunk->data[4] || !chunk->data[5] || !chunk->data[6] || !chunk->data[7]) && (chunk->data[8] == 8) &&
		(!chunk->data[9] || chunk->data[9] == 2 || chunk->data[9] == 3))
	{
		return true;
	}

	return false;
}

void get_size(chunk *chunk, byte *width, byte *height)
{
	for (size_t i = 0; i < 4; i++)
	{
		width[i] = chunk->data[i];
	}

	for (size_t i = 4; i < 8; i++)
	{
		height[i - 4] = chunk->data[i];
	}
}

void fill_array(byte *data, const byte *new_data, const uint size, const uint new_data_size)
{
	for (uint i = size; i < size + new_data_size; i++)
	{
		data[i] = new_data[i - size];
	}
}

int decompress(byte *data, uint data_size, byte *picture, uint picture_size)
{
#ifdef ZLIB

	if (uncompress(picture, (uLongf *)&picture_size, data, data_size) != Z_OK)
	{
		return false;
	}

	return true;

#endif
#ifdef ISAL

	struct inflate_state uncompress;

	isal_inflate_init(&uncompress);

	uncompress.crc_flag = IGZIP_ZLIB;
	uncompress.next_in = data;
	uncompress.avail_in = data_size;
	uncompress.next_out = picture;
	uncompress.avail_out = picture_size;

	if (isal_inflate(&uncompress) != ISAL_DECOMP_OK)
	{
		return false;
	}

	return true;

#endif
#ifdef LIBDEFLATE

	struct libdeflate_decompressor *uncompress = libdeflate_alloc_decompressor();

	if (!uncompress)
	{
		return false;
	}

	if (libdeflate_zlib_decompress(uncompress, data, data_size, picture, picture_size, NULL) != LIBDEFLATE_SUCCESS)
	{
		return false;
	}

	return true;

#endif
}

void print_head(uint width, uint height, int is_gray, FILE *out)
{
	byte head[3] = { 'P', '5', '\n' };
	if (!is_gray)
	{
		head[1] = '6';
	}
	fwrite(&head, sizeof(byte), 3, out);
	fprintf(out, "%d %d\n%d\n", width, height, 255);
}

byte paeth(uint a, uint b, uint c)
{
	uint p = a + b < c ? 0 : a + b - c;
	uint pa = p > a ? p - a : a - p;
	uint pb = p > b ? p - b : b - p;
	uint pc = p > c ? p - c : c - p;
	if (pa <= pb && pa <= pc)
	{
		return a;
	}
	else if (pb <= pc)
	{
		return b;
	}
	else
	{
		return c;
	}
}

byte filter_pixel(byte *data, uint s, uint c, uint color, uint new_w, uint filter_type)
{
	byte val = 0;
	if (!filter_type)
	{
		val = 0;
	}
	else if (filter_type == 1 && c > color)
	{
		val = data[new_w * s + c - color];
	}
	else if (filter_type == 2 && s > 0)
	{
		val = data[new_w * (s - 1) + c];
	}
	else if (filter_type == 3 && color < c && s > 0)
	{
		val = (data[new_w * (s - 1) + c] + data[new_w * s + c - color]) / 2;
	}
	else if (filter_type == 3 && color >= c && s > 0)
	{
		val = data[new_w * (s - 1) + c] / 2;
	}
	else if (filter_type == 4 && color < c && s > 0)
	{
		val = paeth(data[new_w * s + c - color], data[new_w * (s - 1) + c], data[new_w * (s - 1) + c - color]);
	}
	else if (filter_type == 4 && color >= c && s > 0)
	{
		val = paeth(0, data[new_w * (s - 1) + c], 0);
	}
	else if (filter_type == 4)
	{
		val = paeth(data[new_w * s + c - color], 0, 0);
	}
	return val;
}

void print_data(byte *data, byte *plt, uint picture_size, uint w, uint h, uint plt_size, int is_grey, uint color_type, FILE *out)
{
	uint color = (color_type == 2 ? 3 : 1);
	uint new_w = w * color + 1;

	size_t three = (color_type == 2 || (color == 1 && plt_size && !is_grey)) ? 3 : 1;
	byte *result = malloc(sizeof(byte) * w * h * three);
	size_t index = 0;

	if (!result)
	{
		// error
		OUT_OF_MEMORY_ERROR = true;
		return;
	}

	for (uint s = 0; s < h; s++)
	{
		uint filter_type = data[s * new_w];
		for (uint c = 1; c < new_w; c++)
		{
			data[new_w * s + c] += filter_pixel(data, s, c, color, new_w, filter_type);

			if (color == 1)
			{
				if (!plt_size)
				{
					result[index++] = data[new_w * s + c];
				}
				else if (is_grey)
				{
					result[index++] = plt[data[new_w * s + c] * 3];
				}
				else
				{
					result[index++] = plt[data[new_w * s + c] * 3];
					result[index++] = plt[data[new_w * s + c] * 3 + 1];
					result[index++] = plt[data[new_w * s + c] * 3 + 2];
				}
			}
			else
			{
				result[index++] = data[new_w * s + c];
			}
		}
	}
	fwrite(result, sizeof(byte), index, out);
	free(result);
}

int is_gray_plt(const byte *plt, uint plt_size)
{
	for (uint i = 0; i < plt_size / 3; i++)
	{
		if (!(plt[i] == plt[i + 1] && plt[i] == plt[i + 2]))
		{
			return false;
		}
	}
	return true;
}

int main(int argc, char *argv[])
{
	if (argc != 3)
	{
		// error
		fprintf(stderr,
				"invalid parameter (argc)\n"
				"excepted: 3\n"
				"actual: %d\n",
				argc);
		return ERROR_PARAMETER_INVALID;
	}

	FILE *in = fopen(argv[1], "rb");

	if (!in)
	{
		// error
		fprintf(stderr, "couldn't open input file %s\n", argv[1]);
		return ERROR_CANNOT_OPEN_FILE;
	}

	if (!read_type(in))
	{
		// error
		fclose(in);
		fprintf(stderr, "is not a PNG\n");
		return ERROR_DATA_INVALID;
	}

	int PLTE_contains = false;
	int IDAT_contains = false;
	int IDAT_hole = false;
	int is_grey = false;

	chunk *head = read_chunk(in);

	if (!head)
	{
		// error
		fclose(in);

		if (OUT_OF_MEMORY_ERROR)
		{
			fprintf(stderr, "out of memory\n");
			return ERROR_OUT_OF_MEMORY;
		}
		else
		{
			fprintf(stderr, "incorrect IHDR\n");
			return ERROR_DATA_INVALID;
		}
	}

	if (!check_IHDR(head))
	{
		fclose(in);
		free(head);
		fprintf(stderr, "incorrect IHDR\n");
		return ERROR_DATA_INVALID;
	}

	byte width[4], height[4];
	get_size(head, width, height);
	uint w = make_uint(width, 4);
	uint h = make_uint(height, 4);
	byte color_type = head->data[9];
	is_grey = color_type == 0 ? true : false;

	free(head);

	byte *data = NULL;
	uint data_size = 0;
	byte *plt = NULL;
	uint plt_size = 0;

	while (true)
	{
		chunk *chunk = read_chunk(in);
		if (!chunk)
		{
			// error
			fclose(in);
			free(data);
			free(plt);

			if (OUT_OF_MEMORY_ERROR)
			{
				fprintf(stderr, "out of memory\n");
				return ERROR_OUT_OF_MEMORY;
			}
			else
			{
				fprintf(stderr, "contains incorrect chunk\n");
				return ERROR_DATA_INVALID;
			}
		}
		if (compare_chunk_name(chunk, PLTE))
		{
			if (!color_type)
			{
				// error
				fclose(in);
				free(data);
				free(plt);
				free(chunk);
				fprintf(stderr, "contains an invalid chunk: PLTE\n");
				return ERROR_DATA_INVALID;
			}

			if (PLTE_contains || IDAT_contains)
			{
				// error
				fclose(in);
				free(data);
				free(plt);
				free(chunk);
				fprintf(stderr, "PLTE chunk in incorrect place\n");
				return ERROR_DATA_INVALID;
			}

			if (!(!(chunk->length % 3) && color_type == 3))
			{
				// error
				fclose(in);
				free(data);
				free(plt);
				free(chunk);
				fprintf(stderr, "PLTE is the wrong length\n");
				return ERROR_DATA_INVALID;
			}

			PLTE_contains = true;

			if (color_type == 2)
			{
				free(chunk);
				continue;
			}

			plt = malloc(sizeof(byte) * chunk->length);

			if (!plt)
			{
				fclose(in);
				free(data);
				free(chunk);
				fprintf(stderr, "out of memory\n");
				return ERROR_OUT_OF_MEMORY;
			}

			fill_array(plt, chunk->data, 0, chunk->length);
			plt_size = chunk->length;

			is_grey = is_gray_plt(plt, plt_size);
		}
		else if (compare_chunk_name(chunk, IDAT))
		{
			if (IDAT_hole)
			{
				// error
				fclose(in);
				free(data);
				free(plt);
				free(chunk);
				fprintf(stderr, "IDAT chunks should haven't holes between them\n");
				return ERROR_DATA_INVALID;
			}

			if (IDAT_contains)
			{
				byte *new_data = realloc(data, sizeof(byte) * (data_size + chunk->length));
				if (new_data)
				{
					data = new_data;
					new_data = NULL;
				}
				else
				{
					// error
					fclose(in);
					free(data);
					free(plt);
					free(chunk);
					fprintf(stderr, "out of memory\n");
					return ERROR_OUT_OF_MEMORY;
				}

				fill_array(data, chunk->data, data_size, chunk->length);
				data_size += chunk->length;
			}
			else
			{
				data = malloc(sizeof(byte) * chunk->length);

				if (!data)
				{
					// error
					fclose(in);
					free(plt);
					free(chunk);
					fprintf(stderr, "out of memory\n");
					return ERROR_OUT_OF_MEMORY;
				}
				fill_array(data, chunk->data, 0, chunk->length);
				data_size = chunk->length;
			}
			IDAT_contains = true;
		}
		else if (compare_chunk_name(chunk, IEND))
		{
			free(chunk);
			break;
		}
		else if (compare_chunk_name(chunk, IHDR))
		{
			fclose(in);
			free(data);
			free(plt);
			free(chunk);
			fprintf(stderr, "incorrect PNG file: IHDR chunk should be only one\n");
			return ERROR_DATA_INVALID;
		}
		else if (IDAT_contains)
		{
			IDAT_hole = true;
		}

		free(chunk);
	}

	fclose(in);

	if ((color_type == 3 && !plt) || !data)
	{
		// error
		free(data);
		free(plt);
		fprintf(stderr, "The PNG file does not contain the required chunks\n");
		return ERROR_DATA_INVALID;
	}

	uint picture_size = w * h * (color_type == 2 ? 3 : 1);
	byte *picture = malloc(sizeof(byte) * (picture_size + h));

	if (!picture)
	{
		// error
		free(data);
		free(plt);
		fprintf(stderr, "out of memory\n");
		return ERROR_OUT_OF_MEMORY;
	}

	if (!decompress(data, data_size, picture, picture_size + h))
	{
		// error
		free(data);
		free(plt);
		free(picture);
		fprintf(stderr, "decompress error\n");
		return ERROR_DATA_INVALID;
	}

	free(data);

	FILE *out = fopen(argv[2], "wb");

	if (!out)
	{
		// error
		free(plt);
		free(picture);
		fprintf(stderr, "couldn't open output file %s", argv[2]);
		return ERROR_CANNOT_OPEN_FILE;
	}

	print_head(w, h, is_grey, out);
	print_data(picture, plt, picture_size, w, h, plt_size, is_grey, color_type, out);

	if (OUT_OF_MEMORY_ERROR)
	{
		free(plt);
		free(picture);
		fclose(out);
		fprintf(stderr, "out of memory\n");
		return ERROR_OUT_OF_MEMORY;
	}

	free(plt);
	free(picture);
	fclose(out);
	return SUCCESS;
}
