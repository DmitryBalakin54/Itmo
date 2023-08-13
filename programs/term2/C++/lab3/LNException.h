#pragma once

class LNException
{
	int code;
	char *message;

  public:
	LNException(char *message, int code)
	{
		this->code = code;
		this->message = message;
	}

	[[nodiscard]] int get_code() const { return this->code; }

	char *get_message() { return this->message; }
};