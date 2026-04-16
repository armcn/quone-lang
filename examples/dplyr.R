library(dplyr)

students <- data.frame(
  name = c("Alice", "Bob", "Carol", "Dan", "Eve"),
  score = c(92.0, 78.0, 85.0, 64.0, 91.0),
  dept = c("math", "cs", "math", "cs", "math")
)

names_and_scores <- students |>
  dplyr::select(name, score)

high_scorers <- students |>
  dplyr::filter(score >= 80.0)

with_grade <- students |>
  dplyr::mutate(grade = score / 100.0, pass = score >= 70.0)

dept_stats <- students |>
  dplyr::group_by(dept) |>
  dplyr::summarize(avg_score = mean(score), count = n())

result <- students |>
  dplyr::filter(score > 70.0) |>
  dplyr::mutate(pct = score / 100.0) |>
  dplyr::arrange(desc(score))

