#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <iomanip>
#include <iostream>
#include <numeric>
#include <string>
#include <vector>

#ifdef __APPLE__
#ifndef ACCELERATE_NEW_LAPACK
#define ACCELERATE_NEW_LAPACK
#endif
#include <Accelerate/Accelerate.h>
#define BENCH_HAVE_ACCELERATE 1
#endif

namespace {

using Clock = std::chrono::steady_clock;

constexpr int kMatrixN = 1200;
constexpr std::size_t kNumericN = 6000000;
constexpr std::size_t kSortN = 1000000;
constexpr std::size_t kGroupN = 5000000;
constexpr int kGroupCount = 1000;
constexpr std::size_t kTextN = 250000;

volatile double global_sink = 0.0;

struct Task {
  std::string id;
  std::string name;
  std::function<double()> run;
};

int iterations_from_env() {
  const char *value = std::getenv("BENCH_ITERATIONS");
  if (!value || std::strlen(value) == 0) {
    return 7;
  }
  const int parsed = std::atoi(value);
  return parsed > 0 ? parsed : 7;
}

std::vector<double> make_matrix_a() {
  std::vector<double> a(static_cast<std::size_t>(kMatrixN) * kMatrixN);
  for (int col = 0; col < kMatrixN; ++col) {
    for (int row = 0; row < kMatrixN; ++row) {
      a[static_cast<std::size_t>(row) + static_cast<std::size_t>(col) * kMatrixN] =
          std::sin((row + 1) * 0.013 + (col + 1) * 0.017);
    }
  }
  return a;
}

std::vector<double> make_matrix_b() {
  std::vector<double> b(static_cast<std::size_t>(kMatrixN) * kMatrixN);
  for (int col = 0; col < kMatrixN; ++col) {
    for (int row = 0; row < kMatrixN; ++row) {
      b[static_cast<std::size_t>(row) + static_cast<std::size_t>(col) * kMatrixN] =
          std::cos((row + 1) * 0.011 - (col + 1) * 0.019);
    }
  }
  return b;
}

std::vector<double> make_numeric_data() {
  std::vector<double> x(kNumericN);
  for (std::size_t i = 0; i < x.size(); ++i) {
    x[i] = std::sin((static_cast<double>(i) + 1.0) * 0.0129898) * 100.0;
  }
  return x;
}

std::vector<double> make_sort_data() {
  std::vector<double> x(kSortN);
  for (std::size_t i = 0; i < x.size(); ++i) {
    const double raw = std::sin((static_cast<double>(i) + 1.0) * 12.9898) * 43758.5453;
    x[i] = raw - std::floor(raw);
  }
  return x;
}

std::vector<int> make_group_ids() {
  std::vector<int> groups(kGroupN);
  for (std::size_t i = 0; i < groups.size(); ++i) {
    groups[i] = static_cast<int>((i + 1) % kGroupCount);
  }
  return groups;
}

std::vector<double> make_group_values(const std::vector<int> &groups) {
  std::vector<double> values(groups.size());
  for (std::size_t i = 0; i < values.size(); ++i) {
    values[i] = std::sin((static_cast<double>(i) + 1.0) * 0.01) +
                (static_cast<double>(groups[i] + 1) * 0.001);
  }
  return values;
}

std::vector<std::string> make_text_data() {
  std::vector<std::string> rows;
  rows.reserve(kTextN);
  char buffer[64];
  for (std::size_t i = 1; i <= kTextN; ++i) {
    const double temp = static_cast<double>((i * 37) % 100000) / 100.0;
    std::snprintf(buffer, sizeof(buffer), "sensor=%06zu|temp=%07.3f|flag=%zu", i, temp, i % 7);
    rows.emplace_back(buffer);
  }
  return rows;
}

double matrix_multiply_checksum(const std::vector<double> &a, const std::vector<double> &b) {
  std::vector<double> c(static_cast<std::size_t>(kMatrixN) * kMatrixN);
#ifdef BENCH_HAVE_ACCELERATE
  cblas_dgemm(CblasColMajor, CblasNoTrans, CblasNoTrans, kMatrixN, kMatrixN, kMatrixN, 1.0,
              a.data(), kMatrixN, b.data(), kMatrixN, 0.0, c.data(), kMatrixN);
#else
  for (int col = 0; col < kMatrixN; ++col) {
    for (int inner = 0; inner < kMatrixN; ++inner) {
      const double b_value = b[static_cast<std::size_t>(inner) +
                               static_cast<std::size_t>(col) * kMatrixN];
      for (int row = 0; row < kMatrixN; ++row) {
        c[static_cast<std::size_t>(row) + static_cast<std::size_t>(col) * kMatrixN] +=
            a[static_cast<std::size_t>(row) + static_cast<std::size_t>(inner) * kMatrixN] *
            b_value;
      }
    }
  }
#endif
  return std::accumulate(c.begin(), c.end(), 0.0);
}

double numeric_transform_checksum(const std::vector<double> &x) {
  double total = 0.0;
  for (const double value : x) {
    total += std::sqrt(std::abs(value)) * std::sin(value) + std::cos(value * 0.5);
  }
  return total;
}

double sort_checksum(const std::vector<double> &source) {
  std::vector<double> values(source);
  std::sort(values.begin(), values.end());
  return values.front() + values[values.size() / 2] + values.back();
}

double group_sum_checksum(const std::vector<int> &groups, const std::vector<double> &values) {
  std::vector<double> sums(kGroupCount, 0.0);
  for (std::size_t i = 0; i < values.size(); ++i) {
    sums[static_cast<std::size_t>(groups[i])] += values[i];
  }
  return std::accumulate(sums.begin(), sums.end(), 0.0);
}

double text_parse_checksum(const std::vector<std::string> &rows) {
  double total = 0.0;
  for (const std::string &row : rows) {
    const std::size_t start = row.find("temp=");
    if (start == std::string::npos) {
      continue;
    }
    total += std::strtod(row.c_str() + start + 5, nullptr);
  }
  return total;
}

void run_task(const Task &task, int iterations) {
  global_sink += task.run();
  for (int iteration = 1; iteration <= iterations; ++iteration) {
    const auto start = Clock::now();
    const double checksum = task.run();
    const auto end = Clock::now();
    global_sink += checksum;

    const double elapsed_ms =
        std::chrono::duration<double, std::milli>(end - start).count();
    std::cout << "C++," << task.id << ",\"" << task.name << "\"," << iteration << ','
              << std::fixed << std::setprecision(12) << elapsed_ms << ',' << checksum
              << '\n';
  }
}

}  // namespace

int main() {
  const int iterations = iterations_from_env();

  const std::vector<double> matrix_a = make_matrix_a();
  const std::vector<double> matrix_b = make_matrix_b();
  const std::vector<double> numeric_data = make_numeric_data();
  const std::vector<double> sort_data = make_sort_data();
  const std::vector<int> group_ids = make_group_ids();
  const std::vector<double> group_values = make_group_values(group_ids);
  const std::vector<std::string> text_data = make_text_data();

  const std::vector<Task> tasks = {
      {"matrix_multiply", "Matrix multiplication",
       [&]() { return matrix_multiply_checksum(matrix_a, matrix_b); }},
      {"numeric_transform", "Element-wise numeric transform",
       [&]() { return numeric_transform_checksum(numeric_data); }},
      {"sorting", "Numeric sorting", [&]() { return sort_checksum(sort_data); }},
      {"group_sum", "Group-by summation",
       [&]() { return group_sum_checksum(group_ids, group_values); }},
      {"text_parse", "Text field parsing", [&]() { return text_parse_checksum(text_data); }},
  };

  std::cout << "language,task_id,task_name,iteration,elapsed_ms,checksum\n";
  for (const Task &task : tasks) {
    run_task(task, iterations);
  }

  return global_sink == 0.12345 ? 1 : 0;
}
