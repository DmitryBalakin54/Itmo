#include "return_codes.h"
#include <CL/cl.h>

#include <chrono>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>
#include <vector>

#define TILE2 16
#define TILE3 8

#define X 4

size_t MAX_SOURCE_SIZE = 1024 * 1024;

void free_mat(float *mat)
{
	if (mat)
	{
		free(mat);
	}
}

void print_help(bool is_err)
{
	fprintf(
		is_err ? stderr : stdout,
		"lab1.exe < --input file_name > \n"
		"         < --output file_name > \n"
		"         < --realization {0 | 1 | 2 | 3} > \n"
		"         [ --device-type { dgpu | igpu | gpu | cpu | all } ]\n"
		"         [ --device-index index ]\n");
}

enum device_type_enum
{
	dgpu = 1,
	igpu = 2,
	gpu = 3,
	cpu = 128,
	all = 256,
	deflt = 1023
};

struct device
{
	std::string platform_name;
	cl_device_id device_id;
	std::string device_name;
	device_type_enum type;
	device_type_enum real_type;

	explicit device(std::string p_name = "", cl_device_id d_id = nullptr, std::string d_name = "", device_type_enum t = all, device_type_enum g = all) :
		platform_name(std::move(p_name)), device_id(d_id), device_name(std::move(d_name)), type(t), real_type(g)
	{
	}
};

struct input_data
{
	std::string in;
	std::string out;
	cl_device_type dev_type = CL_DEVICE_TYPE_DEFAULT;
	int dev_id = -1;
	int realization = -1;
	device_type_enum type = deflt;
};

size_t ind(size_t str, size_t col, size_t max_col, size_t max_str, bool trans)
{
	if (!trans)
	{
		return str * max_col + col;
	}
	else
	{
		return col * max_str + str;
	}
}

int check_args(int argc, char **argv, input_data &data)
{
	if (argc == 1)
	{
		fprintf(stderr, "Args must be not empty\n");
		print_help(true);
		return ERROR_INCORRECT_ARGUMENTS;
	}

	int index = 1;
	while (index + 1 < argc)
	{

		auto op = argv[index];
		if (!strcmp(op, "--input"))
		{
			if (data.in.empty())
			{
				data.in = argv[index + 1];
			}
			else
			{
				fprintf(stderr, "Key --input must only be contained once\n");
				return ERROR_INCORRECT_ARGUMENTS;
			}
		}
		else if (!strcmp(op, "--output"))
		{
			if (data.out.empty())
			{
				data.out = argv[index + 1];
			}
			else
			{
				fprintf(stderr, "Key --output must only be contained once\n");
				return ERROR_INCORRECT_ARGUMENTS;
			}
		}
		else if (!strcmp(op, "--device-type"))
		{
			if (data.dev_type == CL_DEVICE_TYPE_DEFAULT)
			{
				auto type = argv[index + 1];
				if (!strcmp(type, "dgpu"))
				{
					data.dev_type = CL_DEVICE_TYPE_GPU;
					data.type = dgpu;
				}
				else if (!strcmp(type, "igpu"))
				{
					data.dev_type = CL_DEVICE_TYPE_GPU;
					data.type = igpu;
				}
				else if (!strcmp(type, "gpu"))
				{
					data.dev_type = CL_DEVICE_TYPE_GPU;
					data.type = gpu;
				}
				else if (!strcmp(type, "cpu"))
				{
					data.dev_type = CL_DEVICE_TYPE_CPU;
					data.type = cpu;
				}
				else if (!strcmp(type, "all"))
				{
					data.dev_type = CL_DEVICE_TYPE_ALL;
					data.type = all;
				}
				else
				{
					fprintf(stderr, "Key --device-type must be { dgpu | igpu | gpu | cpu | all }\n");
					return ERROR_INCORRECT_ARGUMENTS;
				}
			}
			else
			{
				fprintf(stderr, "Key --device-type must only be contained once\n");
				return ERROR_INCORRECT_ARGUMENTS;
			}
		}
		else if (!strcmp(op, "--device-index"))
		{
			if (data.dev_id == -1)
			{
				try
				{
					int res = std::stoi(argv[index + 1]);
					if (res >= 0)
					{
						data.dev_id = res;
					}
					else
					{
						fprintf(stderr, "Key --device-index must be natural num\n");
						return ERROR_INCORRECT_ARGUMENTS;
					}
				} catch (std::invalid_argument &e)
				{
					fprintf(stderr, "Key --device-index must be natural num\n");
					return ERROR_INCORRECT_ARGUMENTS;
				} catch (std::out_of_range &e)
				{
					fprintf(stderr, "Key --device-index is too large\n");
					return ERROR_INCORRECT_ARGUMENTS;
				}
			}
			else
			{
				fprintf(stderr, "Key --device-index must only be contained once\n");
				return ERROR_INCORRECT_ARGUMENTS;
			}
		}
		else if (!strcmp(op, "--realization"))
		{
			if (data.realization == -1)
			{
				try
				{
					int res = std::stoi(argv[index + 1]);
					if (res >= 0 and res <= 3)
					{
						data.realization = res;
					}
					else
					{
						fprintf(stderr, "Key --realization must be {0 | 1 | 2 | 3}\n");
						return ERROR_INCORRECT_ARGUMENTS;
					}
				} catch (std::invalid_argument &e)
				{
					fprintf(stderr, "Key --realization must be natural num\n");
					return ERROR_INCORRECT_ARGUMENTS;
				} catch (std::out_of_range &e)
				{
					fprintf(stderr, "Key --realization is too large\n");
					return ERROR_INCORRECT_ARGUMENTS;
				}
			}
			else
			{
				fprintf(stderr, "Key --realization must only be contained once\n");
				return ERROR_INCORRECT_ARGUMENTS;
			}
		}
		else
		{
			fprintf(stderr, "Key %s must only is incorrect\n", op);
			return ERROR_INCORRECT_ARGUMENTS;
		}
		index += 2;
	}

	if (data.dev_id == -1)
	{
		data.dev_id = 0;
	}

	if (data.in.empty())
	{
		fprintf(stderr, "Key --input must be contained\n");
		print_help(true);
		return ERROR_INCORRECT_ARGUMENTS;
	}

	if (data.out.empty())
	{
		fprintf(stderr, "Key --output must be contained\n");
		print_help(true);
		return ERROR_INCORRECT_ARGUMENTS;
	}

	if (data.realization == -1)
	{
		fprintf(stderr, "Key --realization must be contained\n");
		print_help(true);
		return ERROR_INCORRECT_ARGUMENTS;
	}

	return SUCCESS;
}

void resize(int &n, int &k, int &m, int tile)
{
	n = n % tile == 0 ? n : (n / tile + 1) * tile;
	m = m % tile == 0 ? m : (m / tile + 1) * tile;
	k = k % tile == 0 ? k : (k / tile + 1) * tile;
}

int read_mat(FILE *file, float **mat, int str, int col, int real_str, int real_col, bool trans)
{
	auto *new_mat = (float *)malloc(sizeof(float) * str * col);
	if (!new_mat)
	{
		fprintf(stderr, "Out of memory\n");
		return ERROR_OUT_OF_MEMORY;
	}

	for (size_t i = real_str; i < str; i++)
	{
		for (size_t j = 0; j < col; j++)
		{
			new_mat[ind(i, j, col, str, trans)] = 0.0f;
		}
	}

	for (size_t j = real_col; j < col; j++)
	{
		for (size_t i = 0; i < str; i++)
		{
			new_mat[ind(i, j, col, str, trans)] = 0.0f;
		}
	}

	int err = 0;
	for (size_t i = 0; i < real_str; i++)
	{
		for (size_t j = 0; j < real_col; j++)
		{
			err = fscanf(file, "%f", &new_mat[ind(i, j, col, str, trans)]);
			if (err == EOF)
			{
				fclose(file);
				fprintf(stderr, "EOF while reading data\n");
				return ERROR_WITH_FILE;
			}
			else if (err != 1)
			{
				fclose(file);
				fprintf(stderr, "Incorrect data in file\n");
				return ERROR_WITH_FILE;
			}
		}
	}

	(*mat) = new_mat;
	return SUCCESS;
}

int compare_devices(device &a, device &b)
{
	if (a.type == b.type)
	{
		return a.real_type - b.real_type;
	}

	return a.type - b.type;
}

void bubble(std::vector< device > &vec)
{
	for (size_t i = 0; i < vec.size() - 1; i++)
	{
		for (size_t j = i + 1; j < vec.size(); j++)
		{
			if (compare_devices(vec[i], vec[j]) > 0)
			{
				device tmp = vec[i];
				vec[i] = vec[j];
				vec[j] = tmp;
			}
		}
	}
}

std::vector< device > filter_vector(std::vector< device > &vec, device_type_enum type)
{
	std::vector< device > new_vec;
	for (size_t i = 0; i < vec.size(); i++)
	{
		if ((vec[i].type & type) && (vec[i].type <= type))
		{
			new_vec.push_back(vec[i]);
		}
	}

	return new_vec;
}

int get_device(const input_data &data, device &res)
{
	cl_uint num_platforms;
	cl_int err = clGetPlatformIDs(0, nullptr, &num_platforms);
	if (err)
	{
		fprintf(stderr, "Error while trying get platform ids\n");
		return ERROR_CL_GET_PLATFORM_IDS;
	}

	cl_platform_id platforms[num_platforms];
	err = clGetPlatformIDs(num_platforms, platforms, nullptr);
	if (err)
	{
		fprintf(stderr, "Error while trying get platform ids\n");
		return ERROR_CL_GET_PLATFORM_IDS;
	}

	std::vector< device > list;
	cl_device_type tp;
	if (data.dev_type == CL_DEVICE_TYPE_DEFAULT)
	{
		tp = CL_DEVICE_TYPE_ALL;
	}
	else
	{
		tp = data.dev_type;
	}

	for (cl_uint i = 0; i < num_platforms; i++)
	{
		cl_uint num_devices;
		err = clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ALL, 0, nullptr, &num_devices);
		if (err)
		{
			fprintf(stderr, "Error while trying get device ids\n");
			return ERROR_CL_GET_DEVICE_IDS;
		}

		cl_device_id devices[num_devices];
		err = clGetDeviceIDs(platforms[i], CL_DEVICE_TYPE_ALL, num_devices, devices, nullptr);
		if (err)
		{
			fprintf(stderr, "Error while trying get device ids\n");
			return ERROR_CL_GET_DEVICE_IDS;
		}

		char platform_info[128];
		err = clGetPlatformInfo(platforms[i], CL_PLATFORM_NAME, sizeof(platform_info), platform_info, nullptr);
		if (err)
		{
			fprintf(stderr, "Error while trying get platform info\n");
			return ERROR_CL_GET_PLATFORM_INFO;
		}

		std::string platform_name(platform_info);

		for (cl_uint j = 0; j < num_devices; j++)
		{
			cl_int param_val;
			err = clGetDeviceInfo(devices[j], CL_DEVICE_HOST_UNIFIED_MEMORY, sizeof(cl_int), &param_val, nullptr);
			char device_info[128];
			err |= clGetDeviceInfo(devices[j], CL_DEVICE_NAME, sizeof(device_info), device_info, nullptr);
			std::string device_name(device_info);
			cl_device_type cur_type;
			err |= clGetDeviceInfo(devices[j], CL_DEVICE_TYPE, sizeof(cur_type), &cur_type, nullptr);
			if (err)
			{
				fprintf(stderr, "Error while trying get device info\n");
				return ERROR_CL_GET_DEVICE_INFO;
			}

			if (cur_type && tp)
			{
				if (param_val == CL_TRUE && cur_type == CL_DEVICE_TYPE_GPU)
				{
					list.emplace_back(platform_name, devices[j], device_name, igpu, igpu);
					list.emplace_back(platform_name, devices[j], device_name, all, igpu);
				}
				else if (param_val == CL_FALSE && cur_type == CL_DEVICE_TYPE_GPU)
				{
					list.emplace_back(platform_name, devices[j], device_name, dgpu, dgpu);
					list.emplace_back(platform_name, devices[j], device_name, all, dgpu);
				}
				else if (cur_type & CL_DEVICE_TYPE_CPU)
				{
					list.emplace_back(platform_name, devices[j], device_name, cpu, cpu);
					list.emplace_back(platform_name, devices[j], device_name, all, cpu);
				}
			}
		}
	}

	std::vector< device > new_list;

	bubble(list);
	new_list = filter_vector(list, data.type);

	if (new_list.empty())
	{
		fprintf(stderr, "No required device\n");
		return ERROR_CL_NO_REQUIRED_DEVICE;
	}

	cl_uint ind;
	if (data.dev_id < new_list.size())
	{
		ind = data.dev_id;
	}
	else
	{
		ind = 0;
	}

	res = new_list[ind];
	return SUCCESS;
}

int read_data(const input_data &data, float **mat1, float **mat2, int &n, int &k, int &m, int &real_n, int &real_k, int &real_m, device &res_dev, int &tile)
{
	FILE *file;
	file = fopen(data.in.c_str(), "r");
	int err;
	if (!file)
	{
		fprintf(stderr, "Couldn't open input file\n");
		return ERROR_WITH_FILE;
	}

	err = fscanf(file, "%d %d %d", &n, &k, &m);
	if (err == EOF)
	{
		fclose(file);
		fprintf(stderr, "EOF while reading data\n");
		return ERROR_WITH_FILE;
	}
	else if (err != 3)
	{
		fclose(file);
		fprintf(stderr, "Incorrect data in file\n");
		return ERROR_WITH_FILE;
	}

	real_m = m;
	real_n = n;
	real_k = k;
	if (data.realization >= 1)
	{
		err = get_device(data, res_dev);
		if (err)
		{
			fclose(file);
			return err;
		}

		if (data.realization == 2)
		{
			tile = TILE2;
			resize(n, k, m, tile);
		}
		else if (data.realization == 3)
		{
			tile = TILE3;
			resize(n, k, m, tile * X);
		}
	}

	err = read_mat(file, mat1, m, k, real_m, real_k, false);
	if (err)
	{
		fclose(file);
		return err;
	}

	// NOTE: set false -> data.realization == 3 for trans mat2
	err = read_mat(file, mat2, k, n, real_k, real_n, data.realization == 3);
	if (err)
	{
		fclose(file);
		return err;
	}

	fclose(file);

	return SUCCESS;
}

int print_matrix(float **mat, int str, int col, int real_str, int real_col, input_data &data)
{
	FILE *file = fopen(data.out.c_str(), "w");
	if (!file)
	{
		fprintf(stderr, "Couldn't open output file\n");
		return ERROR_WITH_FILE;
	}

	if (fprintf(file, "%i %i\n", real_col, real_str) < 0)
	{
		fclose(file);
		fprintf(stderr, "Error while write to output file\n");
		return ERROR_WITH_FILE;
	}

	for (int i = 0; i < real_str; i++)
	{
		for (int j = 0; j < real_col; j++)
		{
			if (fprintf(file, j == real_col - 1 ? "%f" : "%f ", (*mat)[ind(i, j, col, str, false)]) < 0)
			{
				fclose(file);
				fprintf(stderr, "Error while write to output file\n");
				return ERROR_WITH_FILE;
			}
		}
		if (fprintf(file, "\n") < 0)
		{
			fclose(file);
			fprintf(stderr, "Error while write to output file\n");
			return ERROR_WITH_FILE;
		}
	}

	fclose(file);

	return SUCCESS;
}

int read_source(const std::string &file_name, char **code, size_t &source_size)
{
	FILE *fp = fopen(file_name.c_str(), "r");
	if (!fp)
	{
		fprintf(stderr, "Failed to load kernel.\n");
		return ERROR_WITH_FILE;
	}

	(*code) = (char *)malloc(MAX_SOURCE_SIZE * sizeof(char));
	if (!*code)
	{
		fprintf(stderr, "Out of memory\n");
		return ERROR_OUT_OF_MEMORY;
	}

	source_size = fread(*code, 1, MAX_SOURCE_SIZE, fp);
	fclose(fp);
	return SUCCESS;
}

int get_time(cl_event &event, double &res)
{
	cl_ulong time_start, time_end;
	cl_uint err;
	err = clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_START, sizeof(time_start), &time_start, nullptr);
	err |= clGetEventProfilingInfo(event, CL_PROFILING_COMMAND_END, sizeof(time_end), &time_end, nullptr);
	if (err)
	{
		fprintf(stderr, "Error while trying get profiling info\n");
		return ERROR_CL_GET_PROFILING_INFO;
	}

	res = (double)(time_end - time_start) * 1e-6;
	return SUCCESS;
}

int matrix_multiply_gpu(float *mat1, float *mat2, int n, int k, int m, float **res, input_data &data, device dev, int tile)
{
	cl_int err = 0;
	cl_context context = nullptr;
	cl_command_queue command_queue = nullptr;
	cl_program program = nullptr;
	cl_kernel kernel = nullptr;
	cl_mem a_mem = nullptr;
	cl_mem b_mem = nullptr;
	cl_mem c_mem = nullptr;
	cl_event events[3];
	float *c_arg = NULL;
	std::string ker_name = "matrix_mult";
	std::string func;

	context = clCreateContext(nullptr, 1, &dev.device_id, nullptr, nullptr, &err);
	if (err)
	{
		fprintf(stderr, "Error while trying create context\n");
		err = ERROR_CL_CREATE;
		goto clean;
	}

	command_queue = clCreateCommandQueue(context, dev.device_id, CL_QUEUE_PROFILING_ENABLE, &err);
	if (err)
	{
		fprintf(stderr, "Error while trying create command queue\n");
		err = ERROR_CL_CREATE;
		goto clean;
	}

	char *source_str;
	size_t source_size;
	if (data.realization == 1)
	{
		func = "matrix_mult1.cl";
	}
	else if (data.realization == 2)
	{
		func = "matrix_mult2.cl";
	}
	else if (data.realization == 3)
	{
		func = "matrix_mult3.cl";
	}

	err = read_source(func, &source_str, source_size);
	if (err)
	{
		goto clean;
	}

	program = clCreateProgramWithSource(context, 1, (const char **)&source_str, (const size_t *)&source_size, &err);
	if (err)
	{
		fprintf(stderr, "Error while trying create program\n");
		err = ERROR_CL_CREATE;
		goto clean;
	}

	err = clBuildProgram(program, 1, &dev.device_id, "-cl-mad-enable -cl-fast-relaxed-math", nullptr, nullptr);
	if (err)
	{
		size_t build_log_size;
		clGetProgramBuildInfo(program, dev.device_id, CL_PROGRAM_BUILD_LOG, 0, NULL, &build_log_size);
		char*build_log = (char*)malloc(build_log_size);
		if (build_log)
		{
			clGetProgramBuildInfo(program, dev.device_id, CL_PROGRAM_BUILD_LOG, build_log_size, build_log, NULL);
			fprintf(stderr, "Error while trying build program on device %s: %i\n%s\n", dev.device_name.c_str(), err, build_log);
			free(build_log);
		}
		else
		{
			fprintf(stderr, "Error while trying build program on device %s: %i\n", dev.device_name.c_str(), err);

		}
		err = ERROR_CL_BUILD_PROGRAM;
		goto clean;
	}

	kernel = clCreateKernel(program, ker_name.c_str(), &err);
	if (err)
	{
		fprintf(stderr, "Error while trying create kernel\n");
		err = ERROR_CL_CREATE;
		goto clean;
	}

	c_arg = (float *)malloc(sizeof(float) * n * m);
	if (!c_arg)
	{
		fprintf(stderr, "Out of memory\n");
		err = ERROR_OUT_OF_MEMORY;
		goto clean;
	}

	a_mem = clCreateBuffer(context, CL_MEM_READ_ONLY, m * k * sizeof(cl_float), nullptr, &err);
	if (err)
	{
		fprintf(stderr, "Error while trying create buffer\n");
		err = ERROR_CL_CREATE;
		goto clean;
	}

	b_mem = clCreateBuffer(context, CL_MEM_READ_ONLY, n * k * sizeof(cl_float), nullptr, &err);
	if (err)
	{
		fprintf(stderr, "Error while trying create buffer\n");
		err = ERROR_CL_CREATE;
		goto clean;
	}

	c_mem = clCreateBuffer(context, CL_MEM_WRITE_ONLY, m * n * sizeof(cl_float), nullptr, &err);
	if (err)
	{
		fprintf(stderr, "Error while trying create buffer\n");
		err = ERROR_CL_CREATE;
		goto clean;
	}

	cl_event a_ev, b_ev, c_ev, c_rd_ev;
	err = clEnqueueWriteBuffer(command_queue, a_mem, CL_TRUE, 0, m * k * sizeof(cl_float), mat1, 0, nullptr, &a_ev);
	err |= clEnqueueWriteBuffer(command_queue, b_mem, CL_TRUE, 0, n * k * sizeof(cl_float), mat2, 0, nullptr, &b_ev);
	err |= clEnqueueWriteBuffer(command_queue, c_mem, CL_TRUE, 0, n * m * sizeof(cl_float), c_arg, 0, nullptr, &c_ev);
	if (err)
	{
		fprintf(stderr, "Error while trying write to buffer\n");
		err = ERROR_CL_WRITE_BUFFER;
		goto clean;
	}

	err = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&a_mem);
	err |= clSetKernelArg(kernel, 1, sizeof(cl_mem), (void *)&b_mem);

	if (err)
	{
		fprintf(stderr, "Error while trying set args\n");
		err = ERROR_CL_SET_ARG;
		goto clean;
	}

	err = clSetKernelArg(kernel, 2, sizeof(cl_mem), (void *)&c_mem);
	err |= clSetKernelArg(kernel, 3, sizeof(cl_int), (void *)&k);
	err |= clSetKernelArg(kernel, 4, sizeof(cl_int), (void *)&n);
	err |= clSetKernelArg(kernel, 5, sizeof(cl_int), (void *)&m);

	if (err)
	{
		fprintf(stderr, "Error while trying set args\n");
		err = ERROR_CL_SET_ARG;
		goto clean;
	}

	size_t global_work_size[2];
	size_t local_work_size[2];

	global_work_size[1] = m / (data.realization == 3 ? X : 1);
	global_work_size[0] = n / (data.realization == 3 ? X : 1);
	local_work_size[0] = tile;
	local_work_size[1] = tile;

	events[0] = a_ev;
	events[1] = b_ev;
	events[2] = c_ev;
	clWaitForEvents(3, events);

	cl_event ker_event;
	err = clEnqueueNDRangeKernel(command_queue, kernel, 2, nullptr, global_work_size, data.realization == 1 ? nullptr : local_work_size, 0, nullptr, &ker_event);

	clWaitForEvents(1, &ker_event);

	if (err)
	{
		fprintf(stderr, "Error while running kernel\n");
		err = ERROR_CL_RUNNING_KERNEL;
		goto clean;
	}

	err = clEnqueueReadBuffer(command_queue, c_mem, CL_TRUE, 0, n * m * sizeof(cl_float), c_arg, 0, nullptr, &c_rd_ev);

	clWaitForEvents(1, &c_rd_ev);

	if (err)
	{
		fprintf(stderr, "Error while trying read buffer\n");
		err = ERROR_CL_READ_BUFFER;
		goto clean;
	}

	*res = c_arg;
	c_arg = nullptr;
	double run_t, a_t, b_t, c_t, c_r_t;

	err = get_time(ker_event, run_t);
	err |= get_time(a_ev, a_t);
	err |= get_time(b_ev, b_t);
	err |= get_time(c_ev, c_t);
	err |= get_time(c_rd_ev, c_r_t);

	if (err)
	{
		goto clean;
	}

	printf("Device: %s\tPlatform: %s\n", dev.device_name.c_str(), dev.platform_name.c_str());
	printf("Time: %g\t%g\n", run_t, run_t + a_t + b_t + c_t + c_r_t);

	if (data.realization >= 2)
	{
		printf("LOCAL_WORK_SIZE [%i, %i]\n", tile, tile);
	}

	if (data.realization == 3)
	{
		printf("WI_WORK %i\n", X * X);
	}

	err = SUCCESS;

clean:
	if (command_queue)
	{
		clReleaseCommandQueue(command_queue);
	}

	if (context)
	{
		clReleaseContext(context);
	}

	if (program)
	{
		clReleaseProgram(program);
	}

	if (kernel)
	{
		clReleaseKernel(kernel);
	}

	if (a_mem)
	{
		clReleaseMemObject(a_mem);
	}

	if (b_mem)
	{
		clReleaseMemObject(b_mem);
	}

	if (c_mem)
	{
		clReleaseMemObject(c_mem);
	}

	if (c_arg)
	{
		free(c_arg);
	}
	//free_mat(c_arg);

	return err;
}

int matrix_multiply_basic(int n, int k, int m, const float *mat1, const float *mat2, float **res)
{
	auto new_res = (float *)malloc(sizeof(float) * n * m);
	if (!new_res)
	{
		fprintf(stderr, "Out of memory\n");
		return ERROR_OUT_OF_MEMORY;
	}

	auto start = std::chrono::steady_clock::now();

	size_t res_i;
	size_t res_j;
	size_t i;

#pragma omp parallel for shared(mat1, mat2, new_res) private(res_i, res_j, i) collapse(2)
	for (res_i = 0; res_i < m; res_i++)
	{
		for (res_j = 0; res_j < n; res_j++)
		{
			float r = 0;
			for (i = 0; i < k; i++)
			{
				r += mat1[ind(res_i, i, k, m, false)] * mat2[ind(i, res_j, n, m, false)];
			}
			new_res[ind(res_i, res_j, n, m, false)] = r;
		}
	}

	auto end = std::chrono::steady_clock::now();
	auto duration = std::chrono::duration_cast< std::chrono::milliseconds >(end - start).count();

	printf("Time: %g\n", (double)duration);

	*res = new_res;
	return SUCCESS;
}

int matrix_multiply(input_data &data)
{
	float *mat1 = nullptr;
	float *mat2 = nullptr;
	int n, k, m;
	int real_n, real_k, real_m;
	int tile;
	device dev;
	int err = read_data(data, &mat1, &mat2, n, k, m, real_n, real_k, real_m, dev, tile);
	if (err)
	{
		free_mat(mat1);
		free_mat(mat2);
		return err;
	}

	float *res = nullptr;
	if (data.realization == 0)
	{
		err = matrix_multiply_basic(n, k, m, mat1, mat2, &res);
	}
	else if (data.realization > 0 && data.realization < 4)
	{
		err = matrix_multiply_gpu(mat1, mat2, n, k, m, &res, data, dev, tile);
	}

	free_mat(mat1);
	free_mat(mat2);

	if (err)
	{
		free_mat(res);
		return err;
	}

	err = print_matrix(&res, m, n, real_m, real_n, data);
	if (err)
	{
		free_mat(res);
		return err;
	}

	free_mat(res);

	return SUCCESS;
}

bool help(int argc, char **argv)
{
	if (argc != 2)
	{
		return false;
	}

	if (!strcmp(argv[1], "--help"))
	{
		print_help(false);
		return true;
	}

	return false;
}

int main(int argc, char *argv[])
{
	if (help(argc, argv))
	{
		return SUCCESS;
	}

	input_data data;
	int err = check_args(argc, argv, data);
	if (err)
	{
		return err;
	}

	err = matrix_multiply(data);
	if (err)
	{
		return err;
	}

	return SUCCESS;
}
