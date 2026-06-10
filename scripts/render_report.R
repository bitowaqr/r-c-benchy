args <- commandArgs(trailingOnly = TRUE)
input_path <- if (length(args) >= 1) args[[1]] else "results/timings.csv"
output_path <- if (length(args) >= 2) args[[2]] else "report.html"

timings <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
if ("language" %in% names(timings) && !"implementation" %in% names(timings)) {
  names(timings)[names(timings) == "language"] <- "implementation"
}
timings$elapsed_ms <- as.numeric(timings$elapsed_ms)
timings$implementation <- as.character(timings$implementation)

task_order <- unique(timings$task_id)
implementation_order <- c("R base", "R Matrix", "R data.table", "C++")

task_meta <- list(
  matrix_multiply = list(
    scale = "1,200 x 1,200 double matrices",
    note = "Base R matrix product, Matrix dense dgeMatrix product, and C++ cblas_dgemm. Matrix conversion happens before timing."
  ),
  numeric_transform = list(
    scale = "6,000,000 doubles",
    note = "sqrt(abs(x)) * sin(x) + cos(0.5 * x), reduced to one checksum."
  ),
  sorting = list(
    scale = "1,000,000 doubles",
    note = "Base R sort(), data.table setorder() on a copied table, and C++ std::sort."
  ),
  group_sum = list(
    scale = "5,000,000 rows, 1,000 groups",
    note = "Base R rowsum(), data.table grouped sum, and a direct C++ accumulation loop."
  ),
  text_parse = list(
    scale = "250,000 delimited strings",
    note = "Base R regex extraction versus C++ delimiter scan and strtod."
  )
)

html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

fmt_ms <- function(x) {
  if (is.na(x)) {
    "n/a"
  } else if (x >= 1000) {
    sprintf("%.2f s", x / 1000)
  } else if (x >= 100) {
    sprintf("%.0f ms", x)
  } else if (x >= 10) {
    sprintf("%.1f ms", x)
  } else {
    sprintf("%.2f ms", x)
  }
}

fmt_impl_class <- function(implementation) {
  class <- tolower(implementation)
  class <- gsub("\\+", "plus", class)
  class <- gsub("[^a-z0-9]+", "-", class)
  class <- gsub("(^-|-$)", "", class)
  class
}

rank_implementations <- function(implementations) {
  ranks <- match(implementations, implementation_order)
  ranks[is.na(ranks)] <- length(implementation_order) + seq_len(sum(is.na(ranks)))
  implementations[order(ranks, implementations)]
}

named_value_or_na <- function(values, name) {
  if (name %in% names(values)) values[[name]] else NA_real_
}

median_for <- function(task_id, implementation) {
  values <- timings$elapsed_ms[
    timings$task_id == task_id & timings$implementation == implementation
  ]
  if (!length(values)) NA_real_ else median(values)
}

mean_for <- function(task_id, implementation) {
  values <- timings$elapsed_ms[
    timings$task_id == task_id & timings$implementation == implementation
  ]
  if (!length(values)) NA_real_ else mean(values)
}

sd_for <- function(task_id, implementation) {
  values <- timings$elapsed_ms[
    timings$task_id == task_id & timings$implementation == implementation
  ]
  if (length(values) <= 1) NA_real_ else sd(values)
}

base_cpp_label <- function(base_r, cpp) {
  if (is.na(base_r) || is.na(cpp)) {
    return("Base R / C++ unavailable")
  }
  ratio <- base_r / cpp
  if (ratio >= 1) {
    sprintf("C++ %.2fx faster than base R", ratio)
  } else {
    sprintf("Base R %.2fx faster than C++", 1 / ratio)
  }
}

summary_rows <- lapply(task_order, function(task_id) {
  task_timings <- timings[timings$task_id == task_id, ]
  task_name <- unique(task_timings$task_name)[[1]]
  implementations <- rank_implementations(unique(task_timings$implementation))
  medians <- vapply(implementations, function(impl) median_for(task_id, impl), numeric(1))
  fastest <- implementations[[which.min(medians)]]
  base_r <- named_value_or_na(medians, "R base")
  cpp <- named_value_or_na(medians, "C++")

  list(
    task_id = task_id,
    task_name = task_name,
    implementations = implementations,
    medians = medians,
    fastest = fastest,
    fastest_median = medians[[fastest]],
    base_r_median = base_r,
    cpp_median = cpp,
    base_cpp_ratio = base_r / cpp,
    base_cpp_label = base_cpp_label(base_r, cpp)
  )
})

base_comparisons <- Filter(
  function(row) !is.na(row$base_r_median) && !is.na(row$cpp_median),
  summary_rows
)
base_ratios <- vapply(base_comparisons, function(row) row$base_cpp_ratio, numeric(1))
has_base_cpp <- length(base_ratios) > 0
geomean_speedup <- if (has_base_cpp) exp(mean(log(base_ratios))) else NA_real_
cpp_wins <- if (has_base_cpp) sum(base_ratios >= 1) else 0
fastest_counts <- sort(table(vapply(summary_rows, function(row) row$fastest, character(1))), decreasing = TRUE)
fastest_summary <- paste(
  sprintf("%s on %d", names(fastest_counts), as.integer(fastest_counts)),
  collapse = ", "
)
largest_cpp_ratio <- if (has_base_cpp) max(base_ratios) else NA_real_

generated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
cpu <- tryCatch(system("sysctl -n machdep.cpu.brand_string", intern = TRUE), error = function(e) "Unavailable")
if (!length(cpu)) cpu <- "Unavailable"
compiler <- tryCatch(system("g++ --version | head -n 1", intern = TRUE), error = function(e) "Unavailable")
if (!length(compiler)) compiler <- "Unavailable"
blas <- tryCatch(unname(extSoftVersion()[["BLAS"]]), error = function(e) "Unavailable")
if (is.na(blas) || !length(blas)) blas <- "Unavailable"
r_version <- R.version.string
matrix_version <- if (requireNamespace("Matrix", quietly = TRUE)) {
  as.character(packageVersion("Matrix"))
} else {
  "not installed"
}
data_table_version <- if (requireNamespace("data.table", quietly = TRUE)) {
  as.character(packageVersion("data.table"))
} else {
  "not installed"
}
data_table_threads <- if (requireNamespace("data.table", quietly = TRUE)) {
  as.character(data.table::getDTthreads())
} else {
  "n/a"
}

hero_line <- sprintf(
  "Against base R, C++ won %d of %d tasks; geometric mean speedup %.2fx. Extra R package variants are shown where they apply.",
  cpp_wins,
  length(base_comparisons),
  geomean_speedup
)
if (!has_base_cpp) {
  hero_line <- "Extra R package variants are shown where they apply. Add C++ rows for the base R versus C++ headline."
}

card_html <- vapply(summary_rows, function(row) {
  max_time <- max(row$medians, na.rm = TRUE)
  bar_html <- vapply(row$implementations, function(implementation) {
    median_value <- row$medians[[implementation]]
    width <- max(3, median_value / max_time * 100)
    impl_class <- fmt_impl_class(implementation)
    paste0(
      '<div class="bar-row impl-row ', impl_class, '">',
      '<span class="lang">', html_escape(implementation), '</span>',
      '<span class="track"><i style="width:', sprintf("%.2f", width), '%"></i></span>',
      '<strong>', fmt_ms(median_value), '</strong>',
      '</div>'
    )
  }, character(1))

  meta <- task_meta[[row$task_id]]
  winner_class <- fmt_impl_class(row$fastest)
  paste0(
    '<article class="task-card">',
    '<div class="task-top">',
    '<div><p class="kicker">', html_escape(meta$scale), '</p>',
    '<h2>', html_escape(row$task_name), '</h2></div>',
    '<span class="winner ', winner_class, '">Fastest: ', html_escape(row$fastest), '</span>',
    '</div>',
    '<div class="bars" aria-label="Median timing bars for ', html_escape(row$task_name), '">',
    paste(bar_html, collapse = ""),
    '</div>',
    '<p class="comparison">', html_escape(row$base_cpp_label), '</p>',
    '<p class="note">', html_escape(meta$note), '</p>',
    '</article>'
  )
}, character(1))

detail_rows <- unlist(lapply(summary_rows, function(row) {
  vapply(seq_along(row$implementations), function(index) {
    implementation <- row$implementations[[index]]
    paste0(
      '<tr>',
      '<td>', html_escape(row$task_name), '</td>',
      '<td><span class="impl-chip ', fmt_impl_class(implementation), '">', html_escape(implementation), '</span></td>',
      '<td>', fmt_ms(median_for(row$task_id, implementation)), '</td>',
      '<td>', fmt_ms(mean_for(row$task_id, implementation)), '</td>',
      '<td>', fmt_ms(sd_for(row$task_id, implementation)), '</td>',
      '<td>', if (index == 1) html_escape(row$base_cpp_label) else "", '</td>',
      '</tr>'
    )
  }, character(1))
}))

raw_csv <- paste(readLines(input_path, warn = FALSE), collapse = "\n")

html <- paste0(
'<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>R vs C++ Benchmark Brief</title>
<style>
:root {
  --paper: #fbfbf8;
  --ink: #171717;
  --muted: #5b5f63;
  --line: #d9d6cc;
  --r-blue: #136f9f;
  --r-matrix: #0a8fa8;
  --r-datatable: #4b86c5;
  --cpp-red: #d33f2f;
  --yellow: #f1b51c;
  --panel: #ffffff;
}
* { box-sizing: border-box; }
html { background: var(--ink); }
body {
  margin: 0;
  color: var(--ink);
  background:
    linear-gradient(90deg, rgba(19,111,159,.08) 1px, transparent 1px),
    linear-gradient(0deg, rgba(211,63,47,.07) 1px, transparent 1px),
    var(--paper);
  background-size: 38px 38px;
  font-family: "Avenir Next", "Gill Sans", "Trebuchet MS", sans-serif;
  line-height: 1.5;
}
.page {
  width: min(1180px, calc(100% - 32px));
  margin: 0 auto;
  padding: 34px 0 48px;
}
.hero {
  min-height: 72vh;
  display: grid;
  grid-template-columns: minmax(0, 1.12fr) minmax(300px, .88fr);
  gap: 24px;
  align-items: stretch;
  border-top: 8px solid var(--ink);
  padding-top: 18px;
}
.hero-copy {
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  min-height: 560px;
}
.eyebrow {
  display: inline-flex;
  align-items: center;
  gap: 10px;
  width: fit-content;
  padding: 7px 10px;
  border: 2px solid var(--ink);
  background: var(--yellow);
  color: var(--ink);
  font-size: 13px;
  font-weight: 800;
  letter-spacing: 0;
  text-transform: uppercase;
}
h1 {
  margin: 24px 0 0;
  max-width: 860px;
  font-family: Georgia, "Times New Roman", serif;
  font-size: clamp(56px, 10vw, 138px);
  line-height: .84;
  letter-spacing: 0;
}
.lede {
  max-width: 730px;
  margin: 28px 0 0;
  font-size: clamp(19px, 2vw, 27px);
  font-weight: 650;
}
.stamp {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 12px;
  margin-top: 32px;
}
.stamp div {
  min-height: 116px;
  padding: 14px;
  border: 2px solid var(--ink);
  background: var(--panel);
  box-shadow: 6px 6px 0 var(--ink);
}
.stamp span, .metric span, .kicker {
  display: block;
  color: var(--muted);
  font-size: 12px;
  font-weight: 850;
  letter-spacing: 0;
  text-transform: uppercase;
}
th {
  color: var(--muted);
  font-size: 12px;
  font-weight: 850;
  letter-spacing: 0;
  text-transform: uppercase;
}
.stamp strong {
  display: block;
  margin-top: 10px;
  font-size: clamp(24px, 3vw, 40px);
  line-height: 1;
}
.scoreboard {
  background: var(--ink);
  color: var(--paper);
  padding: 20px;
  min-height: 560px;
  display: grid;
  align-content: space-between;
  position: relative;
  overflow: hidden;
}
.scoreboard:before {
  content: "";
  position: absolute;
  inset: 0;
  background:
    linear-gradient(135deg, transparent 0 48%, rgba(241,181,28,.95) 48% 51%, transparent 51%),
    radial-gradient(circle at 84% 12%, rgba(19,111,159,.55), transparent 28%);
  opacity: .55;
}
.scoreboard > * { position: relative; }
.metric {
  border: 1px solid rgba(251,251,248,.35);
  padding: 16px;
  background: rgba(251,251,248,.06);
  backdrop-filter: blur(4px);
}
.metric + .metric { margin-top: 12px; }
.metric span { color: #d8d4c8; }
.metric strong {
  display: block;
  margin-top: 8px;
  font-family: Georgia, "Times New Roman", serif;
  font-size: clamp(42px, 8vw, 92px);
  line-height: .9;
  overflow-wrap: anywhere;
}
.text-metric strong {
  font-size: clamp(24px, 3.2vw, 46px);
  line-height: .98;
}
.score-note {
  margin: 24px 0 0;
  color: #e8e3d6;
  font-size: 15px;
}
.section-title {
  display: flex;
  justify-content: space-between;
  align-items: end;
  gap: 18px;
  margin: 38px 0 16px;
  border-top: 3px solid var(--ink);
  padding-top: 14px;
}
.section-title h2 {
  margin: 0;
  font-family: Georgia, "Times New Roman", serif;
  font-size: clamp(30px, 4vw, 58px);
  line-height: 1;
}
.section-title p {
  max-width: 560px;
  margin: 0;
  color: var(--muted);
  font-weight: 650;
}
.task-grid {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 16px;
}
.task-card {
  border: 2px solid var(--ink);
  background: var(--panel);
  padding: 18px;
  box-shadow: 5px 5px 0 rgba(23,23,23,.9);
}
.task-card:first-child {
  grid-column: 1 / -1;
  background:
    linear-gradient(90deg, rgba(241,181,28,.22), transparent 56%),
    var(--panel);
}
.task-top {
  display: flex;
  justify-content: space-between;
  gap: 16px;
  align-items: start;
}
.task-card h2 {
  margin: 3px 0 0;
  font-size: clamp(21px, 2vw, 31px);
  line-height: 1.05;
}
.winner, .impl-chip {
  border: 2px solid var(--ink);
  padding: 7px 9px;
  font-weight: 900;
  font-size: 13px;
  background: var(--yellow);
}
.winner {
  flex: 0 0 auto;
  white-space: nowrap;
}
.impl-chip {
  display: inline-block;
  min-width: 92px;
  text-align: center;
}
.winner.r-base, .impl-chip.r-base { background: var(--r-blue); color: white; }
.winner.r-matrix, .impl-chip.r-matrix { background: var(--r-matrix); color: white; }
.winner.r-data-table, .impl-chip.r-data-table { background: var(--r-datatable); color: white; }
.winner.cplusplus, .impl-chip.cplusplus { background: var(--cpp-red); color: white; }
.bars {
  margin-top: 22px;
  display: grid;
  gap: 10px;
}
.bar-row {
  display: grid;
  grid-template-columns: 104px minmax(120px, 1fr) 82px;
  gap: 10px;
  align-items: center;
}
.lang {
  font-weight: 950;
  font-size: 13px;
}
.track {
  height: 18px;
  border: 2px solid var(--ink);
  background: repeating-linear-gradient(90deg, #f0eee6 0 8px, #ffffff 8px 16px);
  overflow: hidden;
}
.track i {
  display: block;
  height: 100%;
  border-right: 2px solid var(--ink);
}
.impl-row.r-base .track i { background: var(--r-blue); }
.impl-row.r-matrix .track i { background: var(--r-matrix); }
.impl-row.r-data-table .track i { background: var(--r-datatable); }
.impl-row.cplusplus .track i { background: var(--cpp-red); }
.bar-row strong {
  text-align: right;
  font-size: 14px;
}
.comparison {
  margin: 16px 0 0;
  font-weight: 900;
  color: var(--ink);
}
.note {
  margin: 8px 0 0;
  color: var(--muted);
  font-size: 14px;
  font-weight: 600;
}
.table-wrap {
  overflow-x: auto;
  border: 2px solid var(--ink);
  background: var(--panel);
}
table {
  width: 100%;
  border-collapse: collapse;
  min-width: 900px;
}
th, td {
  padding: 13px 14px;
  border-bottom: 1px solid var(--line);
  text-align: left;
}
td {
  font-weight: 650;
}
tr:nth-child(odd) {
  background: #f3f6f7;
}
.method {
  display: grid;
  grid-template-columns: minmax(0, .92fr) minmax(0, 1.08fr);
  gap: 16px;
}
.method-box {
  border: 2px solid var(--ink);
  background: var(--panel);
  padding: 18px;
}
.method-box h3 {
  margin: 0 0 10px;
  font-size: 20px;
}
.method-box p {
  margin: 0 0 10px;
  color: var(--muted);
  font-weight: 600;
}
.meta-list {
  display: grid;
  gap: 8px;
  margin-top: 12px;
  font-size: 14px;
}
.meta-list div {
  display: grid;
  grid-template-columns: 120px minmax(0, 1fr);
  gap: 10px;
  padding-bottom: 8px;
  border-bottom: 1px solid var(--line);
}
.meta-list span {
  color: var(--muted);
  font-weight: 850;
  text-transform: uppercase;
  font-size: 11px;
}
.meta-list strong {
  min-width: 0;
  overflow-wrap: anywhere;
}
details {
  margin-top: 16px;
  border: 2px solid var(--ink);
  background: var(--ink);
  color: var(--paper);
}
summary {
  cursor: pointer;
  padding: 14px 16px;
  font-weight: 900;
}
pre {
  margin: 0;
  padding: 16px;
  overflow-x: auto;
  border-top: 1px solid rgba(251,251,248,.28);
  color: #f8f3df;
  font-size: 12px;
}
@media (max-width: 860px) {
  .page { width: min(100% - 20px, 1180px); padding-top: 18px; }
  .hero, .method { grid-template-columns: 1fr; min-height: auto; }
  .hero-copy, .scoreboard { min-height: auto; }
  .stamp, .task-grid { grid-template-columns: 1fr; }
  .task-card:first-child { grid-column: auto; }
  .section-title { display: block; }
  .section-title p { margin-top: 10px; }
  .task-top { display: block; }
  .winner { display: inline-block; margin-top: 12px; white-space: normal; }
  .bar-row { grid-template-columns: 92px minmax(72px, 1fr) 66px; gap: 7px; }
  .lang { font-size: 11px; }
  .bar-row strong { font-size: 12px; }
  h1 { font-size: clamp(52px, 18vw, 92px); }
}
</style>
</head>
<body>
<main class="page">
  <section class="hero">
    <div class="hero-copy">
      <div>
        <span class="eyebrow">Benchmark Brief</span>
        <h1>R vs C++</h1>
        <p class="lede">', html_escape(hero_line), ' Median elapsed time is used for the headline comparison across seven measured runs per implementation.</p>
      </div>
      <div class="stamp">
        <div><span>Tasks</span><strong>', length(summary_rows), '</strong></div>
        <div><span>Implementations</span><strong>', length(unique(timings$implementation)), '</strong></div>
        <div><span>Generated</span><strong>', html_escape(format(Sys.time(), "%H:%M")), '</strong></div>
      </div>
    </div>
    <aside class="scoreboard" aria-label="Benchmark highlights">
      <div>
        <div class="metric"><span>Base R vs C++ geomean</span><strong>', if (has_base_cpp) sprintf("%.2fx", geomean_speedup) else "n/a", '</strong></div>
        <div class="metric"><span>Largest C++ edge</span><strong>', if (has_base_cpp) sprintf("%.2fx", largest_cpp_ratio) else "n/a", '</strong></div>
        <div class="metric text-metric"><span>Fastest by task</span><strong>', html_escape(fastest_summary), '</strong></div>
      </div>
      <p class="score-note">Matrix and data.table variants are R package comparisons, not new languages. C++ remains the native baseline.</p>
    </aside>
  </section>

  <section>
    <div class="section-title">
      <h2>Median Results</h2>
      <p>Bars are ordered R base, R package variants, then C++. All R implementations use blue-family bars; C++ uses red.</p>
    </div>
    <div class="task-grid">
      ', paste(card_html, collapse = "\n      "), '
    </div>
  </section>

  <section>
    <div class="section-title">
      <h2>Timing Table</h2>
      <p>Mean and standard deviation are included to make noisy runs visible without hiding the raw measurements.</p>
    </div>
    <div class="table-wrap">
      <table>
        <thead>
          <tr><th>Task</th><th>Implementation</th><th>Median</th><th>Mean</th><th>Std. dev.</th><th>Base R / C++</th></tr>
        </thead>
        <tbody>
          ', paste(detail_rows, collapse = "\n          "), '
        </tbody>
      </table>
    </div>
  </section>

  <section>
    <div class="section-title">
      <h2>Method</h2>
      <p>This is a practical local benchmark, not a universal claim about any package or language.</p>
    </div>
    <div class="method">
      <div class="method-box">
        <h3>How to rerun</h3>
        <p>From this directory, run <code>./run_benchmarks.sh</code>. Set <code>BENCH_ITERATIONS=12</code> to collect more repetitions.</p>
        <p><code>Matrix</code> is used for dense matrix multiplication when installed. <code>data.table</code> is used for sorting and group-by variants when installed.</p>
      </div>
      <div class="method-box">
        <h3>Environment</h3>
        <div class="meta-list">
          <div><span>CPU</span><strong>', html_escape(cpu[[1]]), '</strong></div>
          <div><span>R</span><strong>', html_escape(r_version), '</strong></div>
          <div><span>Matrix</span><strong>', html_escape(matrix_version), '</strong></div>
          <div><span>data.table</span><strong>', html_escape(paste0(data_table_version, " / threads ", data_table_threads)), '</strong></div>
          <div><span>BLAS</span><strong>', html_escape(blas[[1]]), '</strong></div>
          <div><span>Compiler</span><strong>', html_escape(compiler[[1]]), '</strong></div>
          <div><span>Timestamp</span><strong>', html_escape(generated_at), '</strong></div>
        </div>
      </div>
    </div>
    <details>
      <summary>Embedded raw CSV</summary>
      <pre>', html_escape(raw_csv), '</pre>
    </details>
  </section>
</main>
</body>
</html>'
)

writeLines(html, output_path, useBytes = TRUE)
cat("Wrote ", output_path, "\n", sep = "")
