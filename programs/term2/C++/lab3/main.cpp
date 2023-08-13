#include "LN.h"
#include "LNException.h"
#include "return_codes.h"

#include <fstream>
#include <functional>
#include <set>
#include <stack>

std::string str_to_uppercase(std::string &str)
{
	for (char &i : str)
	{
		if ('a' <= i && i <= 'z')
		{
			i -= 'a';
			i += 'A';
		}
	}
	return str;
}

int main(int argc, char *argv[])
{
	if (argc != 3)
	{
		std::cerr << "Invalid number of arguments" << std::endl;
		return ERROR_PARAMETER_INVALID;
	}

	std::ifstream in(argv[1], std::ios_base::in);

	if (!in.is_open())
	{
		std::cerr << "Couldn't open input file: " << argv[1] << std::endl;
		return ERROR_CANNOT_OPEN_FILE;
	}

	std::set< std::string > binary_operations = { "+", "-", "*", "/", "%" };

	std::set< std::string > compare_operations = { "==", "!=", "<", "<=", ">", ">=" };

	std::set< std::string, std::less<>, std::allocator< std::string > > unary_operations = { "_", "~" };

	std::stack< LN > stack;
	std::string str;

	while (in >> str)
	{
		str = str_to_uppercase(str);
		if (binary_operations.count(str) || compare_operations.count(str))
		{
			LN left = stack.top();
			stack.pop();
			LN right = stack.top();
			stack.pop();
			LN res;
			try
			{
				if (str == "+")
				{
					res = left + right;
				}
				else if (str == "-")
				{
					res = left - right;
				}
				else if (str == "*")
				{
					res = left * right;
				}
				else if (str == "/")
				{
					res = left / right;
				}
				else if (str == "%")
				{
					res = left % right;
				}
				else if (str == "==")
				{
					res = LN(left == right);
				}
				else if (str == "!=")
				{
					res = LN(left != right);
				}
				else if (str == "<")
				{
					res = LN(left < right);
				}
				else if (str == "<=")
				{
					res = LN(left <= right);
				}
				else if (str == ">")
				{
					res = LN(left > right);
				}
				else if (str == ">=")
				{
					res = LN(left >= right);
				}
				stack.push(res);
			} catch (LNException &e)
			{
				in.close();
				std::cerr << e.get_message() << std::endl;
				return e.get_code();	// ERROR_OUT_OF_MEMORY
			}
		}
		else if (unary_operations.count(str))
		{
			LN val = stack.top();
			stack.pop();
			LN res;

			try
			{
				if (str == "_")
				{
					res = -val;
				}
				else if (str == "~")
				{
					res = ~val;
				}
				stack.push(res);
			} catch (LNException &e)
			{
				in.close();
				std::cerr << e.get_message() << std::endl;
				return e.get_code();	// ERROR_OUT_OF_MEMORY
			}
		}
		else
		{
			try
			{
				LN el = LN(str);
				stack.push(el);
			} catch (LNException &e)
			{
				in.close();
				std::cerr << e.get_message() << std::endl;
				return e.get_code();	// ERROR_OUT_OF_MEMORY
			}
		}
	}
	in.close();

	std::ofstream out(argv[2]);
	if (!out.is_open())
	{
		std::cerr << "Couldn't open output file: " << argv[2] << std::endl;
		return ERROR_CANNOT_OPEN_FILE;
	}

	while (!stack.empty())
	{
		out << stack.top() << std::endl;
		stack.pop();
	}
	out.close();
	return SUCCESS;
}
