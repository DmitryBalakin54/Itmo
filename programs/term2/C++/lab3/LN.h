#include <string_view>

#include <cstring>
#include <iostream>

class LN
{
	size_t len;
	bool sign;
	uint8_t *num;
	bool is_NaN;

	static LN add_positives(const LN &l, const LN &r);
	static LN sub_positives(const LN &l, const LN &r);
	static LN multiply_positives(const LN &l, const LN &r);
	static LN divide_positives(const LN &l, const LN &r);
	static LN sqrt(const LN &val);

	static int compare_positives(const LN &l, const LN &r);
	static int compare(const LN &l, const LN &r);

  public:
	explicit LN(long long int value = 0);
	LN(size_t len, bool sign, uint8_t *num, bool is_NaN);
	LN(const LN &val);
	explicit LN(const char *val);
	explicit LN(std::string_view val);

	LN &operator=(const LN &val);
	LN &operator=(LN &&val) noexcept;
	friend LN operator-(LN &val);
	friend LN operator+(const LN &l, const LN &r);
	friend LN operator-(const LN &l, const LN &r);
	friend LN operator*(const LN &l, const LN &r);
	friend LN operator/(const LN &l, const LN &r);
	friend LN operator~(const LN &val);
	friend LN operator%(const LN &l, const LN &r);

	LN &operator+=(const LN &val);
	LN &operator-=(const LN &val);
	LN &operator*=(const LN &val);
	LN &operator/=(const LN &val);
	LN &operator%=(const LN &val);

	friend bool operator==(const LN &l, const LN &r);
	friend bool operator!=(const LN &l, const LN &r);
	friend bool operator<(const LN &l, const LN &r);
	friend bool operator>(const LN &l, const LN &r);
	friend bool operator<=(const LN &l, const LN &r);
	friend bool operator>=(const LN &l, const LN &r);

	explicit operator long long int() const;
	explicit operator bool();

	friend std::ostream &operator<<(std::ostream &stream, LN &val);

	~LN();
};

LN operator"" _ln(const char *val);
