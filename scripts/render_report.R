args <- commandArgs(trailingOnly = TRUE)
input_path <- if (length(args) >= 1) args[[1]] else "results/timings.csv"
output_path <- if (length(args) >= 2) args[[2]] else "report.html"

timings <- read.csv(input_path, stringsAsFactors = FALSE, check.names = FALSE)
timings$elapsed_ms <- as.numeric(timings$elapsed_ms)

task_order <- unique(timings$task_id)
languages <- c("R", "C++")

task_meta <- list(
  matrix_multiply = list(
    scale = "1,200 x 1,200 double matrices",
    note = "R uses %*%; C++ calls cblas_dgemm through Accelerate on this Mac."
  ),
  numeric_transform = list(
    scale = "6,000,000 doubles",
    note = "sqrt(abs(x)) * sin(x) + cos(0.5 * x), reduced to one checksum."
  ),
  sorting = list(
    scale = "1,000,000 doubles",
    note = "R sort() uses optimized native sorting; C++ uses generic std::sort on a copied numeric vector."
  ),
  group_sum = list(
    scale = "5,000,000 rows, 1,000 groups",
    note = "R rowsum() versus a direct C++ accumulation loop."
  ),
  text_parse = list(
    scale = "250,000 delimited strings",
    note = "R regex extraction versus C++ delimiter scan and strtod."
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
  if (x >= 1000) {
    sprintf("%.2f s", x / 1000)
  } else if (x >= 100) {
    sprintf("%.0f ms", x)
  } else if (x >= 10) {
    sprintf("%.1f ms", x)
  } else {
    sprintf("%.2f ms", x)
  }
}

fmt_num <- function(x, digits = 2) {
  format(round(x, digits), big.mark = ",", trim = TRUE, scientific = FALSE)
}

median_for <- function(task_id, language) {
  median(timings$elapsed_ms[timings$task_id == task_id & timings$language == language])
}

mean_for <- function(task_id, language) {
  mean(timings$elapsed_ms[timings$task_id == task_id & timings$language == language])
}

sd_for <- function(task_id, language) {
  sd(timings$elapsed_ms[timings$task_id == task_id & timings$language == language])
}

summary_rows <- lapply(task_order, function(task_id) {
  task_name <- unique(timings$task_name[timings$task_id == task_id])[[1]]
  r_median <- median_for(task_id, "R")
  cpp_median <- median_for(task_id, "C++")
  speedup <- r_median / cpp_median
  list(
    task_id = task_id,
    task_name = task_name,
    r_median = r_median,
    cpp_median = cpp_median,
    speedup = speedup,
    winner = if (speedup >= 1) "C++" else "R"
  )
})

geomean_speedup <- exp(mean(log(vapply(summary_rows, function(row) row$speedup, numeric(1)))))
cpp_wins <- sum(vapply(summary_rows, function(row) row$winner == "C++", logical(1)))
fastest_row <- summary_rows[[which.max(vapply(summary_rows, function(row) row$speedup, numeric(1)))]]
closest_row <- summary_rows[[which.min(abs(log(vapply(summary_rows, function(row) row$speedup, numeric(1)))))]]

generated_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
cpu <- tryCatch(system("sysctl -n machdep.cpu.brand_string", intern = TRUE), error = function(e) "Unavailable")
if (!length(cpu)) cpu <- "Unavailable"
compiler <- tryCatch(system("g++ --version | head -n 1", intern = TRUE), error = function(e) "Unavailable")
if (!length(compiler)) compiler <- "Unavailable"
blas <- tryCatch(unname(extSoftVersion()[["BLAS"]]), error = function(e) "Unavailable")
if (is.na(blas) || !length(blas)) blas <- "Unavailable"
r_version <- R.version.string

hero_line <- sprintf(
  "C++ won %d of %d tasks; geometric mean speedup %.2fx.",
  cpp_wins,
  length(summary_rows),
  geomean_speedup
)

card_html <- vapply(summary_rows, function(row) {
  max_time <- max(row$r_median, row$cpp_median)
  r_width <- max(3, row$r_median / max_time * 100)
  cpp_width <- max(3, row$cpp_median / max_time * 100)
  speed_label <- if (row$speedup >= 1) {
    sprintf("C++ %.2fx faster", row$speedup)
  } else {
    sprintf("R %.2fx faster", 1 / row$speedup)
  }
  meta <- task_meta[[row$task_id]]

  paste0(
    '<article class="task-card">',
    '<div class="task-top">',
    '<div><p class="kicker">', html_escape(meta$scale), '</p>',
    '<h2>', html_escape(row$task_name), '</h2></div>',
    '<span class="winner ', if (row$winner == "C++") "cpp" else "r", '">', html_escape(speed_label), '</span>',
    '</div>',
    '<div class="bars" aria-label="Median timing bars for ', html_escape(row$task_name), '">',
    '<div class="bar-row r-bar"><span class="lang">R</span><span class="track"><i style="width:', sprintf("%.2f", r_width), '%"></i></span><strong>', fmt_ms(row$r_median), '</strong></div>',
    '<div class="bar-row cpp-bar"><span class="lang">C++</span><span class="track"><i style="width:', sprintf("%.2f", cpp_width), '%"></i></span><strong>', fmt_ms(row$cpp_median), '</strong></div>',
    '</div>',
    '<p class="note">', html_escape(meta$note), '</p>',
    '</article>'
  )
}, character(1))

detail_rows <- unlist(lapply(summary_rows, function(row) {
  r_mean <- mean_for(row$task_id, "R")
  cpp_mean <- mean_for(row$task_id, "C++")
  r_sd <- sd_for(row$task_id, "R")
  cpp_sd <- sd_for(row$task_id, "C++")
  c(
    paste0(
      '<tr>',
      '<td>', html_escape(row$task_name), '</td>',
      '<td>R</td>',
      '<td>', fmt_ms(row$r_median), '</td>',
      '<td>', fmt_ms(r_mean), '</td>',
      '<td>', fmt_ms(r_sd), '</td>',
      '<td rowspan="2">', if (row$speedup >= 1) sprintf("%.2fx C++", row$speedup) else sprintf("%.2fx R", 1 / row$speedup), '</td>',
      '</tr>'
    ),
    paste0(
      '<tr>',
      '<td>', html_escape(row$task_name), '</td>',
      '<td>C++</td>',
      '<td>', fmt_ms(row$cpp_median), '</td>',
      '<td>', fmt_ms(cpp_mean), '</td>',
      '<td>', fmt_ms(cpp_sd), '</td>',
      '</tr>'
    )
  )
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
  --cpp-red: #d33f2f;
  --yellow: #f1b51c;
  --green: #14835b;
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
  max-width: 710px;
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
  font-size: clamp(32px, 4.2vw, 58px);
  line-height: .94;
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
  max-width: 520px;
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
.winner {
  flex: 0 0 auto;
  border: 2px solid var(--ink);
  padding: 7px 9px;
  font-weight: 900;
  font-size: 13px;
  white-space: nowrap;
  background: var(--yellow);
}
.winner.cpp { background: var(--cpp-red); color: white; }
.winner.r { background: var(--r-blue); color: white; }
.bars {
  margin-top: 22px;
  display: grid;
  gap: 10px;
}
.bar-row {
  display: grid;
  grid-template-columns: 48px minmax(120px, 1fr) 82px;
  gap: 10px;
  align-items: center;
}
.lang {
  font-weight: 950;
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
.r-bar .track i { background: var(--r-blue); }
.cpp-bar .track i { background: var(--cpp-red); }
.bar-row strong {
  text-align: right;
  font-size: 14px;
}
.note {
  margin: 16px 0 0;
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
  min-width: 760px;
}
th, td {
  padding: 13px 14px;
  border-bottom: 1px solid var(--line);
  text-align: left;
}
td {
  font-weight: 650;
}
tr:nth-child(4n+1), tr:nth-child(4n+2) {
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
  .bar-row { grid-template-columns: 42px minmax(90px, 1fr) 72px; }
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
        <p class="lede">', html_escape(hero_line), ' Median elapsed time is used for the headline comparison across seven measured runs per task.</p>
      </div>
      <div class="stamp">
        <div><span>Tasks</span><strong>', length(summary_rows), '</strong></div>
        <div><span>Iterations</span><strong>', max(timings$iteration), '</strong></div>
        <div><span>Generated</span><strong>', html_escape(format(Sys.time(), "%H:%M")), '</strong></div>
      </div>
    </div>
    <aside class="scoreboard" aria-label="Benchmark highlights">
      <div>
        <div class="metric"><span>Geomean speedup</span><strong>', sprintf("%.2fx", geomean_speedup), '</strong></div>
        <div class="metric"><span>Largest C++ edge</span><strong>', sprintf("%.2fx", fastest_row$speedup), '</strong></div>
        <div class="metric text-metric"><span>Closest contest</span><strong>', html_escape(closest_row$task_name), '</strong></div>
      </div>
      <p class="score-note">Matrix multiplication is deliberately included, but it mostly measures calls into native BLAS rather than hand-written language loops.</p>
    </aside>
  </section>

  <section>
    <div class="section-title">
      <h2>Median Results</h2>
      <p>Shorter bars are faster. Both implementations build deterministic data before timing; checksums force the computed results to be used.</p>
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
          <tr><th>Task</th><th>Language</th><th>Median</th><th>Mean</th><th>Std. dev.</th><th>Speedup</th></tr>
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
      <p>This is a practical local benchmark, not a universal claim about either language.</p>
    </div>
    <div class="method">
      <div class="method-box">
        <h3>How to rerun</h3>
        <p>From this directory, run <code>./run_benchmarks.sh</code>. Set <code>BENCH_ITERATIONS=12</code> to collect more repetitions.</p>
        <p>The runner compiles C++ with <code>-O3 -std=c++17 -DNDEBUG</code>, executes both benchmark programs, writes <code>results/timings.csv</code>, and rebuilds this report.</p>
      </div>
      <div class="method-box">
        <h3>Environment</h3>
        <div class="meta-list">
          <div><span>CPU</span><strong>', html_escape(cpu[[1]]), '</strong></div>
          <div><span>R</span><strong>', html_escape(r_version), '</strong></div>
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
