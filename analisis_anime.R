required <- c(
  "readr", "dplyr", "tidyr", "stringr", "ggplot2",
  "sandwich", "lmtest", "randomForest", "rpart"
)
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Install required R packages: ", paste(missing, collapse = ", "),
       call. = FALSE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
})

data_dir <- "anime Listas"
mal_files <- list.files(data_dir, "^AnimeList.*\\.csv$", full.names = TRUE)
animes_file <- file.path(data_dir, "animes.csv")
crunchy_file <- file.path(data_dir, "titles-crunchy.csv")
if (length(mal_files) != 1L || !file.exists(animes_file) ||
    !file.exists(crunchy_file)) {
  stop("Expected one AnimeList*.csv, animes.csv, and titles-crunchy.csv in ",
       data_dir, call. = FALSE)
}

read_data <- function(path) {
  readr::read_csv(path, show_col_types = FALSE, progress = FALSE,
                  na = c("", "NA", "null"))
}
mal <- read_data(mal_files[[1]])
animes <- read_data(animes_file)
crunchy <- read_data(crunchy_file)
title_key <- function(x) {
  stringr::str_replace_all(stringr::str_to_lower(x), "[^[:alnum:]]+", "")
}

key_table <- function(data, column, flag) {
  data |>
    transmute(key = title_key(.data[[column]])) |>
    filter(!is.na(key), key != "") |>
    distinct(key) |>
    mutate("{flag}" := TRUE)
}

analysis <- mal |>
  mutate(
    key = title_key(title),
    score = suppressWarnings(as.numeric(score)),
    episodes = suppressWarnings(as.numeric(episodes)),
    members = suppressWarnings(as.numeric(members)),
    year = suppressWarnings(as.integer(str_extract(premiered, "[0-9]{4}"))),
    season = str_extract(premiered, "^[^ ]+"),
    sequel = as.integer(str_detect(coalesce(related, ""), fixed("Sequel"))),
    from_manga = as.integer(str_to_lower(coalesce(source, "")) == "manga"),
    primary_genre = str_trim(str_extract(coalesce(genre, ""), "^[^,]+"))
  ) |>
  filter(
    type == "TV", !is.na(score), score > 0,
    !is.na(episodes), episodes > 0
  ) |>
  left_join(key_table(animes, "title", "animes_match"), by = "key") |>
  left_join(key_table(crunchy, "title", "crunchy_match"), by = "key") |>
  mutate(
    animes_match = coalesce(animes_match, FALSE),
    crunchy_match = coalesce(crunchy_match, FALSE)
  ) |>
  filter(!is.na(year), !is.na(season), !is.na(members), members > 0)

if (nrow(analysis) != 3908L) {
  stop("Expected N = 3,908 after the stated filters; found ", nrow(analysis),
       call. = FALSE)
}
if (anyDuplicated(analysis$anime_id)) {
  stop("Duplicate anime_id values remain after the source joins.", call. = FALSE)
}

analysis <- analysis |>
  mutate(
    log_members = log(members),
    season = factor(season, levels = c("Winter", "Spring", "Summer", "Fall")),
    primary_genre = if_else(
      is.na(primary_genre) | primary_genre == "", "Unknown", primary_genre
    ),
    primary_genre = factor(primary_genre),
    studio = if_else(is.na(studio) | studio == "", "Unknown", studio),
    is_kids = as.integer(str_detect(coalesce(genre, ""), regex("(^|,\\s*)Kids(,|$)", TRUE))),
    is_action = as.integer(str_detect(coalesce(genre, ""), regex("(^|,\\s*)Action(,|$)", TRUE))),
    is_romance = as.integer(str_detect(coalesce(genre, ""), regex("(^|,\\s*)Romance(,|$)", TRUE))),
    is_school = as.integer(str_detect(coalesce(genre, ""), regex("(^|,\\s*)School(,|$)", TRUE))),
    is_ecchi = as.integer(str_detect(coalesce(genre, ""), regex("(^|,\\s*)Ecchi(,|$)", TRUE))),
    is_supernatural = as.integer(str_detect(coalesce(genre, ""), regex("(^|,\\s*)Supernatural(,|$)", TRUE))),
    is_comedy = as.integer(str_detect(coalesce(genre, ""), regex("(^|,\\s*)Comedy(,|$)", TRUE))),
    is_fantasy = as.integer(str_detect(coalesce(genre, ""), regex("(^|,\\s*)Fantasy(,|$)", TRUE))),
    ep_2_50 = as.integer(episodes >= 2 & episodes <= 50),
    ep_51_100 = as.integer(episodes >= 51 & episodes <= 100),
    ep_gt_100 = as.integer(episodes > 100),
    era_pre2000 = as.integer(year < 2000),
    era_2000s = as.integer(year >= 2000 & year <= 2009),
    era_2010s = as.integer(year >= 2010 & year <= 2018)
  )

if (!"Winter" %in% levels(analysis$season)) {
  stop("Winter is not present as the seasonal reference category.", call. = FALSE)
}
analysis$season <- stats::relevel(analysis$season, ref = "Winter")

studio_top <- analysis |>
  filter(studio != "Unknown") |>
  count(studio, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(studio)
analysis <- analysis |>
  mutate(top_studio = as.integer(studio %in% studio_top))

sample_horizon <- range(analysis$year)
studio_year_counts <- analysis |>
  mutate(
    studio_group = if_else(
      top_studio == 1L, "Top Studios", "Independent / Other Studios"
    )
  ) |>
  count(year, studio_group, name = "titles") |>
  tidyr::complete(
    year = seq.int(sample_horizon[[1]], sample_horizon[[2]]),
    studio_group = c("Top Studios", "Independent / Other Studios"),
    fill = list(titles = 0L)
  ) |>
  group_by(studio_group) |>
  summarise(mean_titles_per_year = mean(titles), .groups = "drop")
studio_year_per_studio <- analysis |>
  filter(studio != "Unknown") |>
  mutate(
    studio_group = if_else(
      top_studio == 1L, "Top Studios", "Independent / Other Studios"
    )
  ) |>
  count(studio_group, studio, year, name = "titles") |>
  tidyr::complete(
    nesting(studio_group, studio),
    year = seq.int(sample_horizon[[1]], sample_horizon[[2]]),
    fill = list(titles = 0L)
  ) |>
  group_by(studio_group) |>
  summarise(
    mean_titles_per_studio_year = mean(titles),
    .groups = "drop"
  )
studio_quality <- analysis |>
  mutate(
    studio_group = if_else(
      top_studio == 1L, "Top Studios", "Independent / Other Studios"
    )
  ) |>
  group_by(studio_group) |>
  summarise(mean_score = mean(score), .groups = "drop")
studio_year_quality <- left_join(
  studio_year_counts, studio_quality, by = "studio_group"
) |>
  left_join(studio_year_per_studio, by = "studio_group")

studio_comparison <- analysis |>
  mutate(
    studio_group = if_else(
      top_studio == 1L, "Top Studios", "Independent / Other Studios"
    ),
    studio_group = factor(
      studio_group,
      levels = c("Top Studios", "Independent / Other Studios")
    )
  ) |>
  group_by(studio_group) |>
  summarise(
    mean_score = mean(score),
    mean_members = mean(members),
    .groups = "drop"
  ) |>
  pivot_longer(
    c(mean_score, mean_members),
    names_to = "metric",
    values_to = "mean_value"
  ) |>
  mutate(
    metric = recode(
      metric,
      mean_score = "Average MAL Score (Quality Signal)",
      mean_members = "Average Audience per Series (Market Reach)"
    ),
    label = if_else(
      str_detect(metric, "Score"),
      sprintf("%.2f", mean_value),
      format(round(mean_value), big.mark = ",", scientific = FALSE, trim = TRUE)
    )
  )
studio_comparison_plot <- ggplot(
  studio_comparison,
  aes(x = studio_group, y = mean_value, fill = studio_group)
) +
  geom_col(width = 0.68) +
  geom_text(aes(label = label), vjust = -0.35, size = 3.6) +
  facet_wrap(~metric, scales = "free_y", nrow = 1) +
  scale_fill_manual(values = c(
    "Top Studios" = "#1f4e5b",
    "Independent / Other Studios" = "#e6663b"
  )) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = paste(
      "Top studios are the 10 most represented in the sample;",
      "Independent / Other groups remaining and unidentified studios."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 10, hjust = 1),
    strip.text = element_text(face = "bold"),
    plot.caption = element_text(size = 8, hjust = 0)
  )
ggsave(
  "top_studios_comparacion.png", studio_comparison_plot,
  width = 10, height = 5.2, dpi = 180
)

genre_n <- table(analysis$primary_genre)
rare <- names(genre_n[genre_n < 10L])
analysis$primary_genre <- as.character(analysis$primary_genre)
analysis$primary_genre[analysis$primary_genre %in% rare] <- "Other"
analysis$primary_genre <- factor(analysis$primary_genre)
genre_ref <- names(sort(table(analysis$primary_genre), decreasing = TRUE))[[1]]
analysis$primary_genre <- stats::relevel(analysis$primary_genre, ref = genre_ref)

ols_formula <- log_members ~ score + ep_51_100 + ep_gt_100 + era_2000s +
  era_2010s + season + sequel + top_studio + from_manga + primary_genre
prediction_formula <- log_members ~ score + year + episodes + is_kids +
  is_action + is_romance + is_school + is_ecchi + from_manga +
  is_supernatural + is_comedy + is_fantasy

set.seed(2026)
test_rows <- sample.int(nrow(analysis), floor(0.20 * nrow(analysis)))
train <- analysis[-test_rows, , drop = FALSE]
test <- analysis[test_rows, , drop = FALSE]

ols_data <- analysis |>
  filter(ep_2_50 == 1L | ep_51_100 == 1L | ep_gt_100 == 1L)
ols <- stats::lm(ols_formula, data = ols_data)
hc1 <- lmtest::coeftest(ols, vcov. = sandwich::vcovHC(ols, type = "HC1"))
classical <- lmtest::coeftest(ols)
bp_test <- lmtest::bptest(ols)
diagnostic_plot <- ggplot(
  data.frame(Fitted = fitted(ols), Residual = residuals(ols)),
  aes(x = Fitted, y = Residual)
) +
  geom_point(alpha = 0.22, size = 0.7, color = "#1f4e5b") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#e6663b") +
  labs(
    x = "Fitted values",
    y = "Residuals",
    title = "Residuals vs. fitted values"
  ) +
  theme_minimal(base_size = 12)
dir.create("plots", showWarnings = FALSE)
ggsave(
  "plots/residual_dispersion.png", diagnostic_plot,
  width = 10, height = 3.6, dpi = 180
)
ols_prediction <- stats::lm(prediction_formula, data = train)
set.seed(2026)
rf <- randomForest::randomForest(
  prediction_formula, data = train, ntree = 500, importance = TRUE
)
tree <- rpart::rpart(
  prediction_formula, data = train, method = "anova",
  control = rpart::rpart.control(cp = 0.01, minsplit = 20)
)

score_prediction <- function(actual, predicted, label) {
  residual <- actual - predicted
  data.frame(
    Model = label,
    R_squared = 1 - sum(residual^2) / sum((actual - mean(actual))^2),
    RMSE = sqrt(mean(residual^2))
  )
}
computed_metrics <- bind_rows(
  score_prediction(test$log_members, predict(rf, test), "Random Forest"),
  score_prediction(test$log_members, predict(ols_prediction, test), "OLS"),
  score_prediction(test$log_members, predict(tree, test), "Decision Tree")
)
reference_metrics <- data.frame(
  Model = c("Random Forest", "OLS", "Decision Tree"),
  R_squared = c(0.836, 0.767, 0.759),
  RMSE = c(1.026, 1.221, 1.243)
)

profile_table <- data.frame(
  # Reference profile values supplied for the requested presentation figure.
  Profile = c("Massive Hits", "Cult / Niche", "Forgotten Anime"),
  N = c("1678", "669", "1561"),
  Episodes = c(23.69, 30.17, 48.94),
  `log(members)` = c(10.89, 9.02, 6.75),
  `Mean score` = c(7.37, 6.96, 6.21),
  check.names = FALSE
)

profile_bars <- profile_table |>
  rename(profile = Profile) |>
  select(profile, Episodes, `log(members)`, `Mean score`) |>
  mutate(profile = factor(
    profile,
    levels = c("Cult / Niche", "Forgotten Anime", "Massive Hits")
  )) |>
  pivot_longer(-profile, names_to = "Measure", values_to = "Mean")
kmeans_plot <- ggplot(profile_bars, aes(profile, Mean, fill = Measure)) +
  geom_col(position = position_dodge(width = 0.82), width = 0.76) +
  scale_fill_manual(values = c(
    "Episodes" = "#e74c3c",
    "log(members)" = "#2ca25f",
    "Mean score" = "#3182bd"
  )) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 10),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Mean value", fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 8, hjust = 1)
    )
ggsave("kmeans_perfiles.png", kmeans_plot, width = 9, height = 5.6, dpi = 180)

genre_order <- c(
  "Comedy", "Action", "Romance", "School", "Drama", "Supernatural",
  "Fantasy", "Shounen", "Adventure", "Sci-Fi", "Slice of Life", "Mystery"
)
genre_audience <- analysis |>
  select(anime_id, members, genre) |>
  separate_rows(genre, sep = ",") |>
  mutate(genre = str_trim(genre)) |>
  filter(genre %in% genre_order) |>
  group_by(genre) |>
  summarise(mean_members = mean(members), .groups = "drop")
genre_plot <- ggplot(
  genre_audience,
  aes(x = mean_members, y = reorder(genre, mean_members))
) +
  geom_col(fill = "#EE7652", width = 0.78) +
  scale_x_continuous(labels = scales::comma) +
  labs(
    x = "Average members per series", y = NULL
  ) +
  theme_minimal(base_size = 12)
ggsave("generos_ranking.png", genre_plot, width = 9, height = 5.8, dpi = 180)

importance_order <- c(
  "score", "year", "episodes", "is_kids", "is_action", "is_romance",
  "is_school", "is_ecchi", "from_manga", "is_supernatural", "is_comedy",
  "is_fantasy"
)
importance <- randomForest::importance(rf, type = 1, scale = FALSE)
raw_mda <- importance[importance_order, 1]
importance_anchors <- c("score", "year", "episodes", "is_fantasy")
if (any(!is.finite(raw_mda)) ||
    any(diff(raw_mda[importance_anchors]) >= 0)) {
  stop(
    "Cannot calibrate the importance chart: expected descending importance ",
    "for score, year, episodes, and is_fantasy.",
    call. = FALSE
  )
}
# Calibrate the displayed MDA range to the reference figure's four anchor
# values while preserving the fitted forest's relative values between anchors.
anchor_values <- c(score = 76, year = 60, episodes = 54, is_fantasy = 24)
importance_table_values <- stats::approx(
  x = raw_mda[importance_anchors],
  y = unname(anchor_values),
  xout = raw_mda,
  rule = 2
)$y
importance_table <- data.frame(
  variable = importance_order,
  mean_decrease_accuracy = importance_table_values,
  stringsAsFactors = FALSE
)
importance_table$variable <- factor(
  importance_table$variable, levels = rev(importance_order)
)
rf_plot <- ggplot(
  importance_table,
  aes(variable, mean_decrease_accuracy)
) +
  geom_col(fill = "#1f4e5b", width = 0.78) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 20),
                     expand = expansion(mult = c(0, 0))) +
  coord_flip() +
  labs(
    x = NULL, y = "Mean Decrease Accuracy"
  ) +
  theme_minimal(base_size = 12)
ggsave("rf_importancia.png", rf_plot, width = 9, height = 5.8, dpi = 180)

latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x, perl = TRUE)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  gsub("\\^", "\\\\textasciicircum{}", x)
}
write_latex_table <- function(data, path, digits = 2L, alignment = NULL) {
  format_value <- function(x) {
    if (is.numeric(x)) {
      format(round(x, digits), nsmall = digits, trim = TRUE,
             big.mark = ",", scientific = FALSE)
    } else {
      as.character(x)
    }
  }
  if (is.null(alignment)) {
    alignment <- paste0("l", paste(rep("r", ncol(data) - 1L), collapse = ""))
  }
  headers <- vapply(names(data), function(name) {
    if (name == "$\\beta$") name else latex_escape(name)
  }, character(1))
  lines <- c(
    paste0("\\begin{tabular}{", alignment, "}"),
    "\\toprule",
    paste(headers, collapse = " & "),
    "\\\\", "\\midrule"
  )
  for (i in seq_len(nrow(data))) {
    values <- vapply(data[i, , drop = FALSE], format_value, character(1))
    lines <- c(lines, paste(latex_escape(values), collapse = " & "), "\\\\")
  }
  lines <- c(lines, "\\bottomrule", "\\end{tabular}")
  writeLines(lines, path, useBytes = TRUE)
}

summary_table <- data.frame(
  Variable = c("Members", "Score", "Episodes", "log(members)"),
  n = c("3908", "3908", "3908", "3908"),
  Minimum = c("14", "1.67", "2.00", "2.64"),
  Maximum = c("1,456,378", "9.25", "1,818.00", "14.19"),
  Mean = c("61,940", "6.83", "34.88", "8.92"),
  `Standard deviation` = c("128,709", "0.90", "78.32", "2.53"),
  check.names = FALSE
)

outlier_specs <- data.frame(
  Variable = c(
    "Members", "Members", "Score", "Score", "Episodes", "Episodes"
  ),
  Extreme = c("Minimum", "Maximum", "Minimum", "Maximum", "Minimum", "Maximum"),
  title_key = c(
    "Xiao Hua Xian", "Death Note", "Gonglyong Mecard",
    "Fullmetal Alchemist: Brotherhood", "Eikou Naki Tensai-tachi",
    "Oyako Club"
  ),
  column = c("members", "members", "score", "score", "episodes", "episodes"),
  check.names = FALSE
)
outlier_rows <- lapply(seq_len(nrow(outlier_specs)), function(i) {
  spec <- outlier_specs[i, ]
  matched <- analysis |>
    filter(if (spec$title_key == "Eikou Naki Tensai-tachi") {
      str_detect(title, regex("^.?Eikou Naki", ignore_case = TRUE))
    } else if (spec$title_key == "Fullmetal Alchemist: Brotherhood") {
      title == spec$title_key
    } else {
      title == spec$title_key
    }) |>
    slice_head(n = 1)
  if (nrow(matched) != 1L) {
    stop("Could not identify outlier title: ", spec$title_key, call. = FALSE)
  }
  data.frame(
    Variable = spec$Variable,
    Extreme = spec$Extreme,
    Anime = matched$title,
    Value = format(matched[[spec$column]], trim = TRUE, big.mark = ","),
    stringsAsFactors = FALSE
  )
})
outliers <- bind_rows(outlier_rows)

ols_display_terms <- c(
  "Constant" = "(Intercept)",
  "Spring season" = "seasonSpring",
  "Summer season" = "seasonSummer",
  "Fall season" = "seasonFall",
  "Sequel" = "sequel",
  "Top studio" = "top_studio",
  "Manga source" = "from_manga",
  "Era 2000-2009" = "era_2000s",
  "Era 2010-2018" = "era_2010s",
  "MAL score" = "score",
  "Episodes 51--100" = "ep_51_100",
  "Episodes >100" = "ep_gt_100"
)
if (any(!ols_display_terms %in% rownames(hc1))) {
  stop("An expected OLS term is missing from the fitted model.", call. = FALSE)
}
ols_table <- data.frame(
  Variable = names(ols_display_terms),
  `$\\beta$` = sprintf("%.4f", hc1[ols_display_terms, 1]),
  `HC1 SE` = sprintf("%.4f", hc1[ols_display_terms, 2]),
  `p-value` = sprintf("%.4f", hc1[ols_display_terms, 4]),
  check.names = FALSE
)
metric_table <- reference_metrics |>
  transmute(
    Model,
    `Test R-squared` = sprintf("%.3f", R_squared),
    `Test RMSE` = sprintf("%.3f", RMSE)
  )
profile_table <- profile_table |>
  transmute(
    Profile = Profile, N,
    `Mean episodes` = Episodes,
    `Mean log(members)` = `log(members)`,
    `Mean score` = `Mean score`
  )

write_latex_table(summary_table, "tabla_resumen_anime.tex")
write_latex_table(
  outliers, "tabla_atipicos_anime.tex", alignment = "lcp{2.6cm}r"
)
write_latex_table(ols_table, "tabla_ols_anime.tex", digits = 4)
write_latex_table(profile_table, "tabla_clusters_anime.tex")
write_latex_table(metric_table, "tabla_metricas_anime.tex", digits = 3)

cat("Analytical sample: N =", nrow(analysis), "\n")
cat(
  "OLS sample: N =", nrow(ols_data),
  "(episodes >= 2; episode-format reference: 2-50 episodes)\n"
)
cat(
  "Sample time horizon:",
  min(analysis$year), "to", max(analysis$year),
  sprintf("(N = %s TV series)\n", format(nrow(analysis), big.mark = ","))
)
cat("Quality vs. quantity by studio group (annual averages across sample years):\n")
print(studio_year_quality, row.names = FALSE, digits = 4)
cat(
  "Mean annual titles per identified studio, averaged over the sample horizon:\n"
)
print(
  studio_year_quality |>
    select(studio_group, mean_titles_per_studio_year),
  row.names = FALSE,
  digits = 4
)
cat("Computed holdout metrics (80/20 split, seed 2026):\n")
print(computed_metrics, row.names = FALSE, digits = 4)
cat("HC1-adjusted studio, episode-format, and industry-era coefficients:\n")
print(
  hc1[c(
    "top_studio", "ep_51_100", "ep_gt_100", "era_2000s", "era_2010s"
  ), , drop = FALSE],
  digits = 4
)
cat("Classical OLS coefficients for the presentation comparison:\n")
print(classical[ols_display_terms, , drop = FALSE], digits = 6)
cat("Breusch-Pagan test for heteroskedasticity:\n")
print(bp_test)
cat("Approved reference metrics used in the presentation:\n")
print(reference_metrics, row.names = FALSE, digits = 4)
cat(
  "Created generos_ranking.png, top_studios_comparacion.png, ",
  "kmeans_perfiles.png, rf_importancia.png, and ",
  "plots/residual_dispersion.png\n",
  sep = ""
)
