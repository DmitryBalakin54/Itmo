#include "return_codes.h"
#include <CL/cl.h>

#include <chrono>
#include <cstdlib>
#include <cstring>
#include <iostream>
#include <string>
#include <vector>

#define TILE 256

size_t MAX_SOURCE_SIZE = 1024 * 1024;

void free_arr(float *arr)
{
	if (arr)
	{
		free(arr);
	}
}

void print_help(bool is_err)
{
	fprintf(is_err ? stderr : stdout,
			"lab1.exe < --input file_name > \n"
			"         < --output file_name > \n"
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
	device_type_enum type = deflt;
};

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

	return SUCCESS;
}

int read_arr(FILE *file, float **arr, int n, int real_n)
{
	auto *new_arr = (float *)malloc(sizeof(float) * n);
	if (!new_arr)
	{
		fprintf(stderr, "Out of memory\n");
		return ERROR_OUT_OF_MEMORY;
	}

	for (size_t i = real_n; i < n; i++)
	{
		new_arr[i] = 0.0f;
	}

	int err = 0;
	for (size_t i = 0; i < real_n; i++)
	{
		err = fscanf(file, "%f", &new_arr[i]);
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

	(*arr) = new_arr;
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

int read_data(const input_data &data, float **arr, int &n, int &real_n, device &res_dev, int &tile)
{
	FILE *file;
	file = fopen(data.in.c_str(), "r");
	int err;
	if (!file)
	{
		fprintf(stderr, "Couldn't open input file\n");
		return ERROR_WITH_FILE;
	}

	err = fscanf(file, "%d", &n);
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

	real_n = n;

	err = get_device(data, res_dev);
	if (err)
	{
		fclose(file);
		return err;
	}

	tile = TILE;

	err = read_arr(file, arr, n, real_n);
	if (err)
	{
		fclose(file);
		return err;
	}

	fclose(file);

	return SUCCESS;
}

int print_arr(float *arr, int n, int real_n, input_data &data)
{
	FILE *file = fopen(data.out.c_str(), "w");
	if (!file)
	{
		fprintf(stderr, "Couldn't open output file\n");
		return ERROR_WITH_FILE;
	}

	for (int i = 0; i < real_n; i++)
	{
		double el = arr[i];
		if (fprintf(file, (i == real_n - 1 ? "%f" : "%f "), el) < 0)
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

int eval_deg(int len)
{
	int res = 0;
	unsigned long long int num = 1;

	while (num < len)
	{
		res++;
		num *= TILE * 2;
	}

	return res;
}

int pref_gpu(float *arr, int n, float **res, input_data &data, device dev, int tile)
{
	cl_int err = 0;
	cl_context context = nullptr;
	cl_command_queue command_queue = nullptr;
	cl_program program = nullptr;
	cl_kernel kernel = nullptr;
	cl_kernel kernel2 = nullptr;
	cl_mem a_mem = nullptr;
	std::string ker_name = "first";
	std::string ker_name2 = "second";
	std::string func = "pref_sum.cl";

	long offset = 0;
	long old_offset = 0;
	int bound = 0;

	cl_event tmp_ev = nullptr;
	cl_event r_rd_ev = nullptr;
	double sum_t = 0.0;

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

	err = clBuildProgram(program, 1, &dev.device_id, "-cl-std=CL1.2 -cl-mad-enable -cl-fast-relaxed-math -cl-unsafe-math-optimizations -w", nullptr, nullptr);
	if (err)
	{
		fprintf(stderr, "Error while trying build program on device %s: %i\n", dev.device_name.c_str(), err);
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

	kernel2 = clCreateKernel(program, ker_name2.c_str(), &err);
	if (err)
	{
		fprintf(stderr, "Error while trying create kernel\n");
		err = ERROR_CL_CREATE;
		goto clean;
	}

	a_mem = clCreateBuffer(context, CL_MEM_READ_WRITE, n * sizeof(cl_float), nullptr, &err);
	if (err)
	{
		fprintf(stderr, "Error while trying create buffer\n");
		err = ERROR_CL_CREATE;
		goto clean;
	}

	cl_event a_ev, a_rd_ev;
	err = clEnqueueWriteBuffer(command_queue, a_mem, CL_TRUE, 0, n * sizeof(cl_float), arr, 0, nullptr, &a_ev);
	err |= clFinish(command_queue);
	if (err)
	{
		fprintf(stderr, "Error while trying write to buffer\n");
		err = ERROR_CL_WRITE_BUFFER;
		goto clean;
	}


	size_t global_work_size[1];
	size_t local_work_size[1];
	bound = eval_deg(n);

	err = clSetKernelArg(kernel, 0, sizeof(cl_mem), (void *)&a_mem);
	err |= clSetKernelArg(kernel, 1, sizeof(cl_int), (void *)&n);

	for (int i = 0; i < bound; i++)
	{
		offset = old_offset == 0 ? 1 : old_offset * tile * 2;
		old_offset = offset;

		err |= clSetKernelArg(kernel, 2, sizeof(cl_int), (void *)&offset);
		if (err)
		{
			fprintf(stderr, "Error while trying set args\n");
			err = ERROR_CL_SET_ARG;
			goto clean;
		}

		int sz1 = n / 2 / offset;
		int tmp1 = tile;
		sz1 = (sz1 + tile - 1) / tile * tile;

		global_work_size[0] = sz1 + tile;
		local_work_size[0] = tile;

		err = clEnqueueNDRangeKernel(command_queue, kernel, 1, nullptr, global_work_size, local_work_size, 0, nullptr, &tmp_ev);
		err |= clFinish(command_queue);
		if (err)
		{
			fprintf(stderr, "Error while running kernel: %i\n", err);
			err = ERROR_CL_RUNNING_KERNEL;
			goto clean;
		}

		double tmp_db = 0;
		err = get_time(tmp_ev, tmp_db);
		if (err)
		{
			goto clean;
		}
		sum_t += tmp_db;
		tmp_ev = nullptr;
	}

	err = clSetKernelArg(kernel2, 0, sizeof(cl_mem), (void *)&a_mem);
	err |= clSetKernelArg(kernel2, 1, sizeof(cl_int), (void *)&n);

	for (int i = 0; i < bound; i++)
	{
		err |= clSetKernelArg(kernel2, 2, sizeof(cl_int), (void *)&offset);
		if (err)
		{
			fprintf(stderr, "Error while trying set args2\n");
			err = ERROR_CL_SET_ARG;
			goto clean;
		}

		int sz2 = n / 2 / offset;
		sz2 = (sz2 + tile - 1) / tile * tile;
		global_work_size[0] = sz2 + tile;
		local_work_size[0] = tile;

		err = clEnqueueNDRangeKernel(command_queue, kernel2, 1, nullptr, global_work_size, local_work_size, 0, nullptr, &tmp_ev);
		err |= clFinish(command_queue);
		if (err)
		{
			fprintf(stderr, "Error while running kernel2\n");
			err = ERROR_CL_RUNNING_KERNEL;
			goto clean;
		}

		double tmp_db = 0;
		err = get_time(tmp_ev, tmp_db);
		if (err)
		{
			goto clean;
		}
		sum_t += tmp_db;
		tmp_ev = nullptr;

		offset /= tile * 2;
	}

	*res = (float *)malloc(sizeof(float) * n);
	if (!(*res))
	{
		fprintf(stderr, "Out of memory\n");
		err = ERROR_OUT_OF_MEMORY;
		goto clean;
	}

	err = clEnqueueReadBuffer(command_queue, a_mem, CL_TRUE, 0, n * sizeof(cl_float), *res, 0, nullptr, &r_rd_ev);
	err |= clFinish(command_queue);

	if (err)
	{
		fprintf(stderr, "Error while trying read buffer\n");
		err = ERROR_CL_READ_BUFFER;
		goto clean;
	}

	double a_t, c_r_t, r_r_t;

	err = get_time(a_ev, a_t);
	err |= get_time(r_rd_ev, r_r_t);

	if (err)
	{
		goto clean;
	}

	printf("Device: %s\tPlatform: %s\n", dev.device_name.c_str(), dev.platform_name.c_str());
	printf("Time: %g\t%g\n", sum_t, sum_t + a_t + r_r_t);

	printf("LOCAL_WORK_SIZE [%i, 0]\n", tile);

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

	if (kernel2)
	{
		clReleaseKernel(kernel2);
	}

	if (a_mem)
	{
		clReleaseMemObject(a_mem);
	}

	return err;
}

int make_pref_sum(input_data &data)
{
	float *arr = nullptr;
	int n;
	int real_n;
	int tile;
	device dev;
	int err = read_data(data, &arr, n, real_n, dev, tile);
	if (err)
	{
		free_arr(arr);
		return err;
	}

	float *res = nullptr;

	err = pref_gpu(arr, n, &res, data, dev, tile);

	if (err)
	{
		free_arr(res);
		return err;
	}

	err = print_arr(res, n, real_n, data);
	if (err)
	{
		free_arr(res);
		return err;
	}

	free_arr(res);

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

	err = make_pref_sum(data);
	if (err)
	{
		return err;
	}

	return SUCCESS;
}
