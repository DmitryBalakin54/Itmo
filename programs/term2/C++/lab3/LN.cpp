#include "LN.h"

#include "LNException.h"
#include "return_codes.h"

LN LN::add_positives(const LN &l, const LN &r)
{
	size_t length = l.len > r.len ? l.len : r.len;
	uint8_t *res_array = (uint8_t *)malloc(length * sizeof(uint8_t));
	if (!res_array)
	{
		throw LNException("No enough memory", ERROR_OUT_OF_MEMORY);
	}

	uint8_t carry = 0;
	for (int i = 0; i < length + 1; i++)
	{
		int val = 0;
		if (i < l.len && i < r.len)
		{
			val = l.num[i] + r.num[i];
		}
		else if (i < l.len)
		{
			val = l.num[i];
		}
		else if (i < r.len)
		{
			val = r.num[i];
		}
		val += carry;
		carry = val / 256;
		uint8_t res_val = val % 256;

		if (i == length && val)
		{
			length++;
			res_array = (uint8_t *)realloc(res_array, length);
			if (!res_array)
			{
				throw LNException("No enough memory", ERROR_OUT_OF_MEMORY);
			}
			res_array[i] = res_val;
			break;
		}
		else if (i == length)
		{
			break;
		}
		res_array[i] = res_val;
	}
	return { length, true, res_array, false };
}

LN LN::sub_positives(const LN &l, const LN &r)
{
	size_t length = l.len;
	uint8_t *res_array = (uint8_t *)malloc(length);
	if (!res_array)
	{
		throw LNException("No enough memory", ERROR_OUT_OF_MEMORY);
	}
	for (size_t i = 0; i < length; i++)
	{
		int val = 0;

		if (i >= r.len)
		{
			val = l.num[i];
		}
		else
		{
			val = l.num[i] - r.num[i];
		}

		if (val < 0)
		{
			val += 256;
			for (size_t j = i + 1; j < length; j++)
			{
				if (l.num[j] == 0)
				{
					l.num[j] = 255;
				}
				else
				{
					l.num[j]--;
					break;
				}
			}
		}
		res_array[i] = (uint8_t)val;
	}
	while (res_array[length - 1] == 0 && length > 1)
	{
		length--;
	}
	res_array = (uint8_t *)realloc(res_array, length);
	if (!res_array)
	{
		throw LNException("No enough memory", ERROR_OUT_OF_MEMORY);
	}
	return { length, true, res_array, false };
}

LN LN::multiply_positives(const LN &l, const LN &r)
{
	size_t length = l.len + r.len;
	uint8_t *res_array = (uint8_t *)malloc(length);
	if (!res_array)
	{
		throw LNException("No enough memory", ERROR_OUT_OF_MEMORY);
	}
	for (size_t i = 0; i < length; i++)
	{
		res_array[i] = 0;
	}

	uint8_t carry = 0;
	for (size_t i = 0; i < l.len; i++)
	{
		for (size_t j = 0; j < r.len; j++)
		{
			int val = l.num[i] * r.num[j] + res_array[i + j];
			val += carry;
			carry = (uint8_t)(val / 256);
			res_array[i + j] = (uint8_t)(val % 256);
		}
	}

	if (carry)
	{
		res_array[l.len - 1 + r.len] = carry;
	}

	while (res_array[length - 1] == 0 && length > 1)
	{
		length--;
	}
	res_array = (uint8_t *)realloc(res_array, length);
	if (!res_array)
	{
		throw LNException("No enough memory", ERROR_OUT_OF_MEMORY);
	}
	return { length, true, res_array, false };
}

LN LN::divide_positives(const LN &l, const LN &r)
{
	if (r.len == 1 && r.num[0] == 1)
	{
		LN res = LN(l);
		return res;
	}

	LN l_abs = LN(l);
	l_abs.sign = true;
	LN res = LN();
	uint8_t *tmp_arr = (uint8_t *)malloc(l.len);
	if (!tmp_arr)
	{
		throw LNException("No enough memory", ERROR_OUT_OF_MEMORY);
	}
	for (size_t i = 0; i < l.len; i++)
	{
		tmp_arr[i] = 0;
	}
	tmp_arr[l.len - 1] = 128;
	LN tmp = { l.len, true, tmp_arr, false };
	size_t ind = tmp.len - 1;
	while (l_abs != LN())
	{
		LN tmp_r = tmp * r;
		tmp_r.sign = true;
		if (tmp_r <= l_abs)
		{
			l_abs -= tmp_r;
			res += tmp;
		}
		if (tmp.num[ind] == 1)
		{
			tmp.num[ind] = 0;
			if (!ind)
			{
				break;
			}
			ind--;
			tmp.num[ind] = 128;
		}
		else
		{
			tmp.num[ind] >>= 1;
		}
	}
	return res;
}

LN LN::sqrt(const LN &val)
{
	LN val_copy = LN(val);
	LN res = LN();
	uint8_t *tmp_arr = (uint8_t *)malloc(val.len);
	if (!tmp_arr)
	{
		throw LNException("No enough memory", ERROR_OUT_OF_MEMORY);
	}
	for (size_t i = 0; i < val.len; i++)
	{
		tmp_arr[i] = 0;
	}
	tmp_arr[val.len - 1] = 128;
	LN tmp = { val.len, true, tmp_arr, false };
	size_t ind = tmp.len - 1;
	while (val_copy != LN())
	{
		LN tmp_plus_res = tmp + res;
		LN tmp_res = tmp_plus_res * tmp_plus_res;
		if (tmp_res <= val_copy)
		{
			res += tmp;
		}

		if (tmp.num[ind] == 1)
		{
			tmp.num[ind] = 0;
			if (!ind)
			{
				break;
			}
			ind--;
			tmp.num[ind] = 128;
			tmp.len--;
		}
		else
		{
			tmp.num[ind] >>= 1;
		}
	}
	return res;
}

int LN::compare_positives(const LN &l, const LN &r)
{
	if (l.len < r.len)
	{
		return 1;
	}
	else if (l.len > r.len)
	{
		return -1;
	}

	for (size_t i = 0; i < r.len; i++)
	{
		if (l.num[i] < r.num[i])
		{
			return 1;
		}
		else if (l.num[i] > r.num[i])
		{
			return -1;
		}
	}
	return 0;
}

int LN::compare(const LN &l, const LN &r)
{
	if (l.sign && !r.sign)
	{
		return -1;
	}
	else if (!l.sign && r.sign)
	{
		return 1;
	}
	else
	{
		if (l.sign && r.sign)
		{
			return compare_positives(l, r);
		}
		else
		{
			return compare_positives(r, l);
		}
	}
}

LN::LN(long long int value)
{
	this->is_NaN = false;
	this->sign = value >= 0;
	value = !this->sign ? value : -value;
	long long int max = -1;
	int step = 0;
	while (step < 63 && max + 1 > value)
	{
		step++;
		max = max << 1;
	}
	this->len = step / 8 + (step % 8 == 0 ? 0 : 1);
	this->len = this->len > 0 ? this->len : 1;
	this->num = (uint8_t *)malloc(len);
	if (!this->num)
	{
		throw LNException("No enough memory", ERROR_OUT_OF_MEMORY);
	}
	step = 0;
	while (step < this->len)
	{
		num[step] = -(value % 256);
		value /= 256;
		step++;
	}
}

LN::LN(size_t len, bool sign, uint8_t *num, bool is_NaN)
{
	this->len = len;
	this->sign = sign;
	this->num = num;
	this->is_NaN = is_NaN;
}

LN::LN(const LN &val)
{
	this->len = val.len;
	this->sign = val.sign;
	this->is_NaN = val.is_NaN;
	this->num = (uint8_t *)malloc(len);
	if (!this->num)
	{
		throw LNException("No enough memory", ERROR_OUT_OF_MEMORY);
	}
	memcpy(this->num, val.num, len);
}

size_t str_len(const char *str)
{
	size_t res = 0;
	while (str[res] != 0)
	{
		res++;
	}
	return res;
}

LN::LN(const char *val)
{
	this->len = str_len(val);
	this->num = (uint8_t *)malloc(this->len);
	if (!this->num)
	{
		throw LNException("No enough memory", ERROR_OUT_OF_MEMORY);
	}
	for (size_t i = 0; i < this->len; i++)
	{
		this->num[i] = 0;
	}
	this->sign = true;
	for (size_t i = this->len - 1;; i--)
	{
		if (val[i] == '-')
		{
			sign = false;
			break;
		}

		uint8_t value = (val[i] >= '0' && val[i] <= '9') ? val[i] - '0' : val[i] - 'A' + 10;

		if (!((this->len - i) % 2))
		{
			this->num[((this->len - 1) - i) / 2] += 16 * value;
		}
		else
		{
			this->num[((this->len - 1) - i) / 2] = value;
		}

		if (i == 0)
		{
			break;
		}
	}

	while (this->num[this->len - 1] == 0 && this->len > 1)
	{
		this->len--;
	}
	this->num = (uint8_t *)realloc(num, this->len);
	if (!this->num)
	{
		throw LNException("No enough memory", ERROR_OUT_OF_MEMORY);
	}
	this->is_NaN = false;
	if (this->len == 1 && !this->num[0])
	{
		this->sign = true;
	}
}

LN::LN(std::string_view val)
{
	this->len = val.length();
	this->num = (uint8_t *)malloc(this->len);
	if (!this->num)
	{
		throw LNException("No enough memory", ERROR_OUT_OF_MEMORY);
	}
	for (size_t i = 0; i < this->len; i++)
	{
		this->num[i] = 0;
	}
	this->sign = true;
	for (size_t i = this->len - 1;; i--)
	{
		if (val[i] == '-')
		{
			sign = false;
			break;
		}

		uint8_t value = (val[i] >= '0' && val[i] <= '9') ? val[i] - '0' : val[i] - 'A' + 10;

		if (!((this->len - i) % 2))
		{
			this->num[((this->len - 1) - i) / 2] += 16 * value;
		}
		else
		{
			this->num[((this->len - 1) - i) / 2] = value;
		}

		if (i == 0)
		{
			break;
		}
	}

	while (this->num[this->len - 1] == 0 && this->len > 1)
	{
		this->len--;
	}
	this->num = (uint8_t *)realloc(num, this->len);
	if (!this->num)
	{
		throw LNException("No enough memory", ERROR_OUT_OF_MEMORY);
	}
	this->is_NaN = false;
	if (this->len == 1 && !this->num[0])
	{
		this->sign = true;
	}
}

LN &LN::operator=(const LN &val)
{
	if (this != &val)
	{
		this->sign = val.sign;
		this->len = val.len;
		this->num = (uint8_t *)realloc(this->num, this->len);
		if (!this->num)
		{
			throw LNException("No enough memory", ERROR_OUT_OF_MEMORY);
		}
		memcpy(this->num, val.num, this->len);
		this->is_NaN = val.is_NaN;
	}
	return *this;
}

LN &LN::operator=(LN &&val) noexcept
{
	if (this != &val)
	{
		this->sign = val.sign;
		this->len = val.len;
		free(this->num);
		this->num = val.num;
		val.num = nullptr;
		this->is_NaN = val.is_NaN;
	}
	return *this;
}

LN operator-(LN &val)
{
	LN res = LN(val);
	res.sign = !res.sign || (res.len == 1 && res.num[0] == 0);
	return res;
}

LN operator+(const LN &l, const LN &r)
{
	if (l.is_NaN || r.is_NaN)
	{
		return { 0, true, nullptr, true };
	}

	LN res;
	if (l.sign && r.sign)
	{
		res = LN::add_positives(l, r);
	}
	else if (!l.sign && !r.sign)
	{
		res = LN::add_positives(l, r);
		res.sign = false;
	}
	else if (!l.sign && r.sign)
	{
		if (LN::compare_positives(l, r) == -1)
		{
			res = LN::sub_positives(l, r);
			res.sign = false;
		}
		else
		{
			res = LN::sub_positives(r, l);
		}
	}
	else
	{
		if (LN::compare_positives(l, r) == 1)
		{
			res = LN::sub_positives(r, l);
			res.sign = false;
		}
		else
		{
			res = LN::sub_positives(l, r);
		}
	}
	return res;
}

LN operator-(const LN &l, const LN &r)
{
	if (l.is_NaN || r.is_NaN)
	{
		return { 0, true, nullptr, true };
	}

	LN res;
	if (l.sign && r.sign)
	{
		if (LN::compare_positives(l, r) == 1)
		{
			res = LN::sub_positives(r, l);
			res.sign = false;
		}
		else
		{
			res = LN::sub_positives(l, r);
		}
	}
	else if (!l.sign && !r.sign)
	{
		if (LN::compare_positives(l, r) == -1)
		{
			res = LN::sub_positives(l, r);
			res.sign = false;
		}
		else
		{
			res = LN::sub_positives(r, l);
		}
	}
	else if (!l.sign && r.sign)
	{
		res = LN::add_positives(l, r);
		res.sign = false;
	}
	else
	{
		res = LN::add_positives(l, r);
	}
	return res;
}

LN operator*(const LN &l, const LN &r)
{
	if (l.is_NaN || r.is_NaN)
	{
		return { 0, true, nullptr, true };
	}

	LN res = LN::multiply_positives(l, r);
	res.sign = (res.len == 1 && res.num[0] == 0) || !(l.sign ^ r.sign);
	return res;
}

LN operator/(const LN &l, const LN &r)
{
	if (l.is_NaN || r.is_NaN || (r.len == 1 && !r.num[0]))
	{
		return { 0, true, nullptr, true };
	}

	if (LN::compare_positives(l, r) == 1)
	{
		return LN();
	}

	LN res = LN::divide_positives(l, r);
	res.sign = !(l.sign ^ r.sign);
	return res;
}

LN operator~(const LN &val)
{
	if (!val.sign || val.is_NaN)
	{
		return { 0, true, nullptr, true };
	}

	return LN::sqrt(val);
}

LN operator%(const LN &l, const LN &r)
{
	LN tmp = l / r;
	tmp *= r;
	return l - tmp;
}

LN &LN::operator+=(const LN &val)
{
	*this = *this + val;
	return *this;
}

LN &LN::operator-=(const LN &val)
{
	*this = *this - val;
	return *this;
}

LN &LN::operator*=(const LN &val)
{
	*this = *this * val;
	return *this;
}

LN &LN::operator/=(const LN &val)
{
	*this = *this / val;
	return *this;
}

LN &LN::operator%=(const LN &val)
{
	*this = *this % val;
	return *this;
}

bool operator==(const LN &l, const LN &r)
{
	return LN::compare(l, r) == 0;
}

bool operator!=(const LN &l, const LN &r)
{
	return !(l == r);
}

bool operator<(const LN &l, const LN &r)
{
	return LN::compare(l, r) == 1;
}

bool operator>(const LN &l, const LN &r)
{
	return LN::compare(l, r) == -1;
}

bool operator<=(const LN &l, const LN &r)
{
	return LN::compare(l, r) >= 0;
}

bool operator>=(const LN &l, const LN &r)
{
	return LN::compare(l, r) <= 0;
}

LN::operator long long int() const
{
	long long int res = 0;
	long long int coef = 1;
	const long long int min_ll = -9223372036854775808;
	for (size_t i = 0; i < this->len; i++)
	{
		long long int tmp = this->num[i];
		if (min_ll / coef > -tmp)
		{
			throw LNException("LN cannot be cast to type long long int", -1);
		}
		res -= tmp * coef;
		if (res >= 0)
		{
			throw LNException("LN cannot be cast to type long long int", -1);
		}
		coef *= 256;
	}

	if (sign)
	{
		if (res == min_ll)
		{
			throw LNException("LN cannot be cast to type long long int", ERROR_UNKNOWN);
		}
		else
		{
			return -res;
		}
	}
	return res;
}

LN::operator bool()
{
	return !(this->len == 1 && !this->num[0]);
}

LN operator""_ln(const char *val)
{
	return LN(val);
}

std::ostream &operator<<(std::ostream &stream, LN &val)
{
	if (val.is_NaN)
	{
		stream << "NaN";
		return stream;
	}

	if (!val.sign)
	{
		stream << "-";
	}

	for (int i = (int)val.len - 1; i >= 0; i--)
	{
		char first = (char)(val.num[i] / 16);
		char second = (char)(val.num[i] % 16);

		first = first < 10 ? (char)('0' + first) : (char)('A' + first - 10);
		second = second < 10 ? (char)('0' + second) : (char)('A' + second - 10);

		if (i == val.len - 1 && first == '0')
		{
			stream << second;
		}
		else
		{
			stream << first;
			stream << second;
		}
	}

	return stream;
}

LN::~LN()
{
	free(this->num);
}
