iterations <- as.integer(Sys.getenv("BENCH_ITERATIONS", "7"))
if (is.na(iterations) || iterations <= 0) {
  iterations <- 7L
}

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

numeric_index <- seq_len(numeric_n)
numeric_data <- sin(numeric_index * 0.0129898) * 100

sort_index <- seq_len(sort_n)
sort_data <- (sin(sort_index * 12.9898) * 43758.5453) %% 1

group_index <- seq_len(group_n)
group_ids <- (group_index %% group_count) + 1L
group_values <- sin(group_index * 0.01) + (group_ids * 0.001)

text_index <- seq_len(text_n)
text_values <- ((text_index * 37L) %% 100000L) / 100
text_data <- sprintf("sensor=%06d|temp=%07.3f|flag=%d", text_index, text_values, text_index %% 7L)

csv_number <- function(x) {
  formatC(x, digits = 12, format = "f")
}

run_task <- function(task_id, task_name, task_fn) {
  invisible(task_fn())
  invisible(gc())

  for (iteration in seq_len(iterations)) {
    start <- proc.time()[["elapsed"]]
    checksum <- task_fn()
    elapsed_ms <- (proc.time()[["elapsed"]] - start) * 1000

    cat(
      "R,", task_id, ",\"", task_name, "\",", iteration, ",",
      csv_number(elapsed_ms), ",", csv_number(checksum), "\n",
      sep = ""
    )
  }
}

task_matrix_multiply <- function() {
  result <- matrix_a %*% matrix_b
  sum(result)
}

task_numeric_transform <- function() {
  sum(sqrt(abs(numeric_data)) * sin(numeric_data) + cos(numeric_data * 0.5))
}

task_sorting <- function() {
  sorted <- sort(sort_data)
  sorted[[1L]] + sorted[[length(sorted) %/% 2L]] + sorted[[length(sorted)]]
}

task_group_sum <- function() {
  sum(rowsum(group_values, group_ids, reorder = FALSE))
}

task_text_parse <- function() {
  values <- as.numeric(sub(".*\\|temp=([0-9.]+)\\|flag=.*", "\\1", text_data, perl = TRUE))
  sum(values)
}

cat("language,task_id,task_name,iteration,elapsed_ms,checksum\n")
run_task("matrix_multiply", "Matrix multiplication", task_matrix_multiply)
run_task("numeric_transform", "Element-wise numeric transform", task_numeric_transform)
run_task("sorting", "Numeric sorting", task_sorting)
run_task("group_sum", "Group-by summation", task_group_sum)
run_task("text_parse", "Text field parsing", task_text_parse)
