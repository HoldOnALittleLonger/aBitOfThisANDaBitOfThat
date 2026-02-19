#include <iostream>
#include <string_view>
#include <vector>
#include <iterator>
#include <charconv>
#include <system_error>
#include <cstdint>

std::string_view english_numbers_120[20] = {
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine",
  "ten",
  "eleven",
  "twelve",
  "thirteen",
  "fourteen",
  "fiveteen",
  "sixteen",
  "seventeen",
  "eighteen",
  "nineteen",
  "twenty",
};

auto num_to_str_120(uint8_t n) -> const std::string_view& {
  static std::string_view bad_ret{"bad number 1--20"};
  if (n > 20)
    return bad_ret;
  return english_numbers_120[n - 1];
}

std::string_view english_numbers_3090[7] = {
  "thirty",
  "fourty",
  "fivety",
  "sixty",
  "seventy",
  "eighty",
  "ninety",
};

auto num_to_str_3090(uint8_t n) -> const std::string_view& {
  static std::string_view bad_ret{"bad number 30--90"};

  if (n < 30 || n > 90)
    return bad_ret;
  uint8_t i(n / 10);
  return english_numbers_3090[i - 3];
}

std::string_view english_digits[] = {
  "hundred",
  "thousand",
};

static std::string_view bad_msg{"UNKNOWN ERROR"};

int main(void)
{
  char input_buffer[32] = {0};
  unsigned long int number(0);
  
  std::cout << "reading - number string : " << std::endl;
  std::cin.getline(input_buffer, 32);
  if (std::cin.bad()) {
    std::cerr << "bad input : " << input_buffer << std::endl;
    return -1;
  }

  // convert number string to number.based on 10.
  std::from_chars_result result = std::from_chars(input_buffer, input_buffer + 30, number);
  if (result.ec != std::errc{}) {
    std::cerr << "failed to convert number string - " << input_buffer << std::endl;
    return -1;
  }

  std::vector<std::reference_wrapper<const std::string_view> > print_msg;

  uint8_t handle_hundred_index(2);
  uint8_t number_array[3] = {0};

  auto put_to_hundred_numbers = [&handle_hundred_index, &number_array](uint8_t n) -> void {
    number_array[handle_hundred_index--] = n;
  };

  auto handle_hundred = [&](void) -> void {
    unsigned short n(0);
    unsigned short k(0);
    unsigned short sum(0);

    switch (handle_hundred_index) {
    case 0:
      n = number_array[1] * 10;
      k = number_array[2];

      sum = n + k;
      if (sum <= 20) {
        print_msg.push_back(std::cref(num_to_str_120(sum)));
      } else {
        print_msg.push_back(std::cref(num_to_str_120(k)));
        if (n > 20)
          print_msg.push_back(std::cref(num_to_str_3090(n)));
        else
          print_msg.push_back(std::cref(num_to_str_120(n)));
      }
      break;

    case 1:
      n = number_array[2];
      if (n)
        print_msg.push_back(std::cref(num_to_str_120(n)));
      break;

    case 255:


      n = number_array[1] * 10;
      k = number_array[2];

      sum = n + k;
      if (sum <= 20) {
        print_msg.push_back(std::cref(num_to_str_120(sum)));
      } else {
        print_msg.push_back(std::cref(num_to_str_120(k)));
        if (n > 20)
          print_msg.push_back(std::cref(num_to_str_3090(n)));
        else
          print_msg.push_back(std::cref(num_to_str_120(n)));
      }

      n = number_array[0];

      print_msg.push_back(std::cref(english_digits[0]));
      print_msg.push_back(std::cref(num_to_str_120(n)));
      break;
      
    default:
      print_msg.push_back(std::cref(bad_msg));
    }
  };


  uint8_t i;
  uint8_t thousand_digit(0);
  for (i = 0; number / 10 != 0; number /= 10) {
    uint8_t n = number - (number / 10) * 10;
    put_to_hundred_numbers(n);
    ++i;
    if (!(i % 3)) {
      handle_hundred();
      handle_hundred_index = 2;
      thousand_digit = 1;
      continue;
    }

    if (thousand_digit) {
      print_msg.push_back(std::cref(english_digits[1]));
      thousand_digit = 0;
    }
  }

  if (thousand_digit) {
    print_msg.push_back(std::cref(english_digits[1]));
    thousand_digit = 0;
  }

  put_to_hundred_numbers(number);
  ++i;
  handle_hundred();
  
  std::copy(print_msg.crbegin(), print_msg.crend(), std::ostream_iterator<std::string_view>(std::cout, " "));
  std::cout << std::endl;

  return 0;
}
