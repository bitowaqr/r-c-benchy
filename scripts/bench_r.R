iterations <- as.integer(Sys.getenv("BENCH_ITERATIONS", "7"))
if (is.na(iterations) || iterations <= 0) {
  iterations <- 7L
}

has_matrix <- requireNamespace("Matrix", quietly = TRUE)
has_data_table <- requireNamespace("data.table", quietly = TRUE)

matrix_n <- 1200L
numeric_n <- 6000000L
sort_n <- 1000000L
group_n <- 5000000L
group_count <- 1000L
text_n <- 250000L

matrix_index <- seq_len(matrix_n)
matrix_a <- outer(
  matrix_index,
  matrix_index,
  function(row, col) sin(row * 0.013 + col * 0.017)
)
matrix_b <- outer(
  matrix_index,
  matrix_index,
  function(row, col) cos(row * 0.011 - col * 0.019)
)

if (has_matrix) {
  matrix_a_matrix <- Matrix::Matrix(matrix_a, sparse = FALSE)
  matrix_b_matrix <- Matrix::Matrix(matrix_b, sparse = FALSE)
} else {
  message("Matrix package is not installed; skipping R Matrix variant.")
}

numeric_index <- seq_len(numeric_n)
numeric_data <- sin(numeric_index * 0.0129898) * 100

sort_index <- seq_len(sort_n)
sort_data <- (sin(sort_index * 12.9898) * 43758.5453) %% 1

group_index <- seq_len(group_n)
group_ids <- (group_index %% group_count) + 1L
group_values <- sin(group_index * 0.01) + (group_ids * 0.001)

if (has_data_table) {
  dt_sort <- data.table::data.table(x = sort_data)
  dt_group <- data.table::data.table(group = group_ids, value = group_values)
} else {
  message("data.table package is not installed; skipping R data.table variants.")
}

text_index <- seq_len(text_n)
text_values <- ((text_index * 37L) %% 100000L) / 100
text_data <- sprintf("sensor=%06d|temp=%07.3f|flag=%d", text_index, text_values, text_index %% 7L)

csv_number <- function(x) {
  formatC(x, digits = 12, format = "f")
}

run_task <- function(implementation, task_id, task_name, task_fn) {
  invisible(task_fn())
  invisible(gc())

  for (iteration in seq_len(iterations)) {
    start <- proc.time()[["elapsed"]]
    checksum <- task_fn()
    elapsed_ms <- (proc.time()[["elapsed"]] - start) * 1000

    cat(
      implementation, ",", task_id, ",\"", task_name, "\",", iteration, ",",
      csv_number(elapsed_ms), ",", csv_number(checksum), "\n",
      sep = ""
    )
  }
}

task_matrix_multiply <- function() {
  result <- matrix_a %*% matrix_b
  sum(result)
}

task_matrix_multiply_matrix <- function() {
  result <- matrix_a_matrix %*% matrix_b_matrix
  sum(result)
}

task_numeric_transform <- function() {
  sum(sqrt(abs(numeric_data)) * sin(numeric_data) + cos(numeric_data * 0.5))
}

task_sorting <- function() {
  sorted <- sort(sort_data)
  sorted[[1L]] + sorted[[length(sorted) %/% 2L]] + sorted[[length(sorted)]]
}

task_sorting_data_table <- function() {
  sorted <- data.table::copy(dt_sort)
  data.table::setorder(sorted, x)
  sorted[["x"]][[1L]] + sorted[["x"]][[nrow(sorted) %/% 2L]] + sorted[["x"]][[nrow(sorted)]]
}

task_group_sum <- function() {
  sum(rowsum(group_values, group_ids, reorder = FALSE))
}

task_group_sum_data_table <- function() {
  grouped <- dt_group[, list(total = sum(value)), by = group]
  sum(grouped[["total"]])
}

task_text_parse <- function() {
  values <- as.numeric(sub(".*\\|temp=([0-9.]+)\\|flag=.*", "\\1", text_data, perl = TRUE))
  sum(values)
}

cat("implementation,task_id,task_name,iteration,elapsed_ms,checksum\n")
run_task("R base", "matrix_multiply", "Matrix multiplication", task_matrix_multiply)
if (has_matrix) {
  run_task("R Matrix", "matrix_multiply", "Matrix multiplication", task_matrix_multiply_matrix)
}
run_task("R base", "numeric_transform", "Element-wise numeric transform", task_numeric_transform)
run_task("R base", "sorting", "Numeric sorting", task_sorting)
if (has_data_table) {
  run_task("R data.table", "sorting", "Numeric sorting", task_sorting_data_table)
}
run_task("R base", "group_sum", "Group-by summation", task_group_sum)
if (has_data_table) {
  run_task("R data.table", "group_sum", "Group-by summation", task_group_sum_data_table)
}
run_task("R base", "text_parse", "Text field parsing", task_text_parse)
