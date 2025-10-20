
# Bank Marketing Campaign Analysis


# Install packages if needed

pkgs <- c("tidyverse","ggplot2","ggcorrplot","gridExtra","patchwork","scales",
          "caret","smotefamily","randomForest","rpart","rpart.plot","e1071",
          "nnet","xgboost","pROC","RColorBrewer","dplyr")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)

# Load required libraries
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(patchwork)
library(scales)
library(caret)
library(smotefamily)  # For SMOTE
library(randomForest)
library(rpart)
library(rpart.plot)
library(e1071)  # For SVM
library(nnet)   # For neural networks
library(xgboost)
library(pROC)
library(RColorBrewer)
library(dplyr)


# Custom theme (applies globally)

custom_theme <- theme_minimal() +
  theme(
    plot.title      = element_text(size = 14, face = "bold", hjust = 0.5),
    plot.subtitle   = element_text(size = 11, hjust = 0.5),
    axis.title      = element_text(size = 11, face = "bold"),
    axis.text       = element_text(size = 10),
    legend.title    = element_text(size = 11, face = "bold"),
    legend.position = "bottom",
    panel.grid.minor= element_blank(),
    panel.border    = element_rect(fill = NA, color = "grey70", size = 0.5)
  )
theme_set(custom_theme)


# 0) Data loading


# read.csv will NOT find files outside the working directory.

bank_raw <- read.csv("C:/Users/tamtr/Downloads/bank-additional-full.csv",
                      sep = ",", stringsAsFactors = FALSE)


# 1) EDA — detection + quick facts

# Helpers
is_cat <- function(x) is.character(x) || is.factor(x)
is_num <- function(x) is.numeric(x)

num_cols <- names(bank_raw)[vapply(bank_raw, is_num, logical(1))]
cat_cols <- names(bank_raw)[vapply(bank_raw, is_cat,  logical(1))]

# 1.1 Target imbalance
target_tab <- bank_raw %>%
  count(y) %>%
  mutate(pct = n / sum(n))
imbalance_ratio <- with(target_tab, n[y == "yes"] / n[y == "no"])
print(target_tab)
message("Class Imbalance (yes/no): ", round(imbalance_ratio, 4))
# (Spec explicitly flags major imbalance; treat accordingly.) :contentReference[oaicite:2]{index=2}

# 1.2 Skewness of numeric variables
skew_tbl <- tibble(
  variable = num_cols,
  skewness = vapply(bank_raw[num_cols], function(x) e1071::skewness(x, na.rm = TRUE), numeric(1))
) %>% arrange(desc(abs(skewness)))
print(skew_tbl)

# 1.3 Outlier flags (IQR rule) — counts only (we’ll cap optionally in Phase 2)
iqr_outlier_mask <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  (x < lower | x > upper)
}
outlier_counts <- map_int(bank_raw[num_cols], ~ sum(iqr_outlier_mask(.x), na.rm = TRUE))
print(tibble(variable = num_cols, outliers = outlier_counts) %>% arrange(desc(outliers)))

# 1.4 Missing / odd categories
na_counts <- bank_raw %>% summarise(across(everything(), ~ sum(is.na(.))))
print(na_counts[, colSums(na_counts) > 0, drop = FALSE])

unknown_counts <- bank_raw %>%
  summarise(across(all_of(cat_cols), ~ sum(. %in% c("unknown", "unk", "NA", ""))))
print(unknown_counts[, colSums(unknown_counts) > 0, drop = FALSE])

#1.5 Check for duplicate rows
duplicates <- sum(duplicated(bank_raw))
cat("Number of duplicate rows:", duplicates, "\n")



# 2) PREPROCESSING PIPELINE

bank_pp <- bank_raw

# 2.1 Remove leakage: duration
# Duration is only known post-call; exclude for any realistic model. :contentReference[oaicite:3]{index=3}
if ("duration" %in% names(bank_pp)) bank_pp <- select(bank_pp, -duration)

# 2.2 Imputation
# Numeric → median; Categorical → mode (with special handling for "unknown")
mode_of <- function(v) {
  v <- v[!is.na(v)]
  if (length(v) == 0) return(NA_character_)
  names(sort(table(v), decreasing = TRUE))[1]
}

# Treat "unknown": keep if informative (>5%), else impute mode
handle_unknown <- function(v) {
  v <- as.character(v)
  unk_mask <- v == "unknown"
  if (!any(unk_mask)) return(v)
  unk_pct <- mean(unk_mask)
  if (unk_pct > 0.05) {
    # keep as its own informative level
    v
  } else {
    # impute to mode excluding 'unknown'
    mode_val <- mode_of(v[!unk_mask])
    v[unk_mask] <- mode_val
    v
  }
}

# Apply imputation
for (nm in names(bank_pp)) {
  col <- bank_pp[[nm]]
  if (is.numeric(col)) {
    if (anyNA(col)) bank_pp[[nm]][is.na(col)] <- median(col, na.rm = TRUE)
  } else {
    # normalize empties to NA first
    col[col %in% c("", "NA")] <- NA
    # step 1: handle NA → mode
    if (anyNA(col)) {
      m <- mode_of(col)
      col[is.na(col)] <- m
    }
    # step 2: handle "unknown" rule
    col <- handle_unknown(col)
    bank_pp[[nm]] <- col
  }
}

# 2.3 Optional outlier capping (Winsorize) for *truly* continuous features
# Keep pdays raw (999 sentinel →  feature-engineer instead).
cap_winsor <- function(x, lower_p = 0.01, upper_p = 0.99) {
  lo <- quantile(x, lower_p, na.rm = TRUE)
  hi <- quantile(x, upper_p, na.rm = TRUE)
  x[x < lo] <- lo
  x[x > hi] <- hi
  x
}

cap_candidates <- c("age", "campaign", "previous", "emp.var.rate",
                    "cons.price.idx", "cons.conf.idx", "euribor3m", "nr.employed")
cap_candidates <- intersect(cap_candidates, names(bank_pp))
for (nm in cap_candidates) bank_pp[[nm]] <- cap_winsor(bank_pp[[nm]])

# 2.4 Remove duplicate rows

# (optional) keep a copy of the duplicates for audit
dup_rows <- bank_pp[duplicated(bank_pp) | duplicated(bank_pp, fromLast = TRUE), ]
# write.csv(dup_rows, "C:/Users/tamtr/Downloads/duplicates_found.csv", row.names = FALSE)

# Remove dup rows
n_before <- nrow(bank_pp)
bank_pp  <- dplyr::distinct(bank_pp)  # drop exact duplicates, keep first
n_after  <- nrow(bank_pp)
cat("Removed", n_before - n_after, "duplicate rows. New size:", n_after, "\n")

# 2.5 Create data_clean

# data_clean: imputed + winsorized + no 'duration' + duplicates removed (NO FE)
n_before_clean <- nrow(bank_raw)
data_clean <- bank_pp


# (Optional) write to CSV
write.csv(
   data_clean,
   "C:/Users/tamtr/Downloads/data_clean.csv",
   row.names = FALSE)

# 2.5 Feature Engineering
bank_pp <- bank_pp %>%
  mutate(
    age_group = cut(age,
                    breaks = c(0, 25, 35, 45, 55, 65, Inf),
                    labels = c("Young", "Early_Career", "Mid_Career",
                               "Late_Career", "Pre_Retirement", "Senior"),
                    right = FALSE),
    contact_intensity = case_when(
      campaign <= 1 ~ "Low",
      campaign <= 3 ~ "Medium",
      TRUE          ~ "High"
    ),
    economic_situation = case_when(
      emp.var.rate >  0  ~ "Growing",
      emp.var.rate < -1  ~ "Declining",
      TRUE               ~ "Stable"
    ),
    has_previous_contact = if_else(previous > 0, "Yes", "No"),
    days_since_contact = case_when(
      pdays == 999           ~ "Never",
      pdays < 7              ~ "Recent_Week",
      pdays < 30             ~ "Recent_Month",
      pdays < 90             ~ "Recent_Quarter",
      TRUE                   ~ "Old"
    ),
    y_binary = if_else(y == "yes", 1L, 0L)
  )

# 2.6 Encode categoricals (one-hot) & z-score scale numerics
# Keep a modeling matrix version; also keep a “pp” version for EDA/plots.

# One-hot encode all character/factor predictors (drop the intercept)
# Keep y_binary as outcome; exclude raw 'y' to avoid duplication.
predictors <- setdiff(names(bank_pp), c("y", "y_binary"))
mm <- model.matrix(~ . - 1, data = bank_pp[, predictors])
bank_model_df <- cbind(
  y_binary = bank_pp$y_binary,
  as.data.frame(mm, check.names = FALSE)
)

message("Modeling frame ready. Dim: ", paste(dim(bank_model_df), collapse = " x "))

# --- NORMALIZE bank_model_df ---

# Define is_binary function
is_binary <- function(x) is.numeric(x) && all(na.omit(unique(x)) %in% c(0, 1))

# Columns to scale (exclude target)
pred_cols <- setdiff(names(bank_model_df), "y_binary")

# 1) Z-score standardization
bank_model_df_z <- bank_model_df
bank_model_df_z[pred_cols] <- lapply(bank_model_df_z[pred_cols], function(col) {
  if (!is.numeric(col) || is_binary(col)) return(col)  # Skip dummy 0/1
  s <- sd(col, na.rm = TRUE)
  if (is.na(s) || s == 0) return(rep(0, length(col)))
  as.numeric(scale(col))
})

# 2) Min–max scaling to [0,1]
rng_scale <- function(x) {
  if (!is.numeric(x) || is_binary(x)) return(x)  # Skip dummy 0/1
  r <- range(x, na.rm = TRUE); if (diff(r) == 0) return(rep(0, length(x)))
  (x - r[1]) / (r[2] - r[1])
}
bank_model_df_mm <- bank_model_df
bank_model_df_mm[pred_cols] <- lapply(bank_model_df_mm[pred_cols], rng_scale)

message("Model frames ready:",
        "\n  - bank_model_df (raw one-hot)",
        "\n  - bank_model_df_z (z-score, no re-scaling of dummies)",
        "\n  - bank_model_df_mm (min-max, no re-scaling of dummies)")

# (Optional) write to CSV
write.csv(bank_model_df_z, "C:/Users/tamtr/Downloads/bank_model_df_z.csv", row.names = FALSE)
write.csv(bank_model_df_mm, "C:/Users/tamtr/Downloads/bank_model_df_mm.csv", row.names = FALSE)

# 3) VISUALS — 10 ggplot2 plots

pal_no_yes <- c("no" = "#E74C3C", "yes" = "#27AE60")
# P1: Target distribution
target_tab <- data_clean %>%
  dplyr::count(y) %>%
  dplyr::mutate(pct = n / sum(n),
                lbl = paste0(scales::percent(pct, accuracy = 0.1), "\n(n=", scales::comma(n), ")"))

p1 <- ggplot2::ggplot(target_tab, ggplot2::aes(x = "", y = pct, fill = y)) +
  ggplot2::geom_col(width = 1, color = "white") +
  ggplot2::coord_polar(theta = "y") +
  ggplot2::scale_fill_manual(values = pal_no_yes) +
  ggplot2::geom_text(ggplot2::aes(label = lbl),
                     position = ggplot2::position_stack(vjust = 0.5),
                     fontface = "bold", size = 3.5, lineheight = 0.9) +
  ggplot2::labs(
    title = "Target Distribution: Term Deposit Subscription",
    subtitle = "Severe class imbalance",
    fill = "Subscription Decision",
    x = NULL, y = NULL
  ) +
  ggplot2::theme(axis.ticks = element_blank(),
                 axis.text  = element_blank(),
                 panel.grid = element_blank(),
                 legend.position = "bottom")

p1

# P2: Age distribution by target
p2 <- ggplot2::ggplot(data_clean, ggplot2::aes(x = age, fill = y)) +
  ggplot2::geom_histogram(bins = 30, alpha = 0.7, position = "stack") +
  ggplot2::facet_wrap(~ y, scales = "free_y") +
  ggplot2::scale_fill_manual(values = pal_no_yes) +
  ggplot2::labs(
    title = "Age Distribution by Subscription Status",
    x = "Age", y = "Frequency",
    fill = "Subscription Decision"
  ) +
  ggplot2::theme(legend.position = "none")  # keep none if you prefer facets only

p2

# P3: Time Since Last Contact (only where previous > 0)
p3 <- data_clean %>%
  dplyr::filter(previous > 0) %>%
  dplyr::mutate(
    pdays_group = cut(
      pdays,
      breaks = c(0, 7, 30, 90, 180, 999),
      labels = c("Week", "Month", "Quarter", "Half-year", "Long"),
      right = TRUE, include.lowest = TRUE
    )
  ) %>%
  dplyr::group_by(pdays_group) %>%
  dplyr::summarise(
    success_rate = mean(y == "yes") * 100,
    count = dplyr::n(),
    .groups = "drop"
  ) %>%
  ggplot2::ggplot(ggplot2::aes(x = pdays_group, y = success_rate, size = count, group = 1)) +
  ggplot2::geom_point(alpha = 0.8, color = "#3498DB") +
  ggplot2::geom_line(size = 1, color = "#2C3E50") +
  ggplot2::scale_size_continuous(range = c(3, 10)) +
  ggplot2::labs(
    title = "Impact of Time Since Last Contact",
    subtitle = "Success rate varies with contact recency (previous > 0)",
    x = "Time Since Last Contact", y = "Success Rate (%)", size = "Sample Size"
  )
p3

# P4: Campaign contact frequency (focus 1–10)
campaign_summary <- data_clean %>%
  dplyr::count(campaign, y) %>%
  dplyr::filter(campaign <= 10)

p4 <- ggplot2::ggplot(campaign_summary, ggplot2::aes(campaign, n, fill = y)) +
  ggplot2::geom_col(position = "dodge", width = 0.7) +
  ggplot2::scale_x_continuous(breaks = 1:10) +
  ggplot2::scale_fill_manual(values = pal_no_yes) +
  ggplot2::labs(
    title = "Contacts During Campaign",
    x = "Number of Contacts", y = "Count",
    fill = "Subscription Decision"
  )
p4

# P5: Job success rate
job_analysis <- data_clean %>%
  dplyr::group_by(job) %>%
  dplyr::summarise(total = dplyr::n(), subscribed = sum(y == "yes"), .groups = "drop") %>%
  dplyr::mutate(success_rate = round(100 * subscribed / total, 1))

p5 <- ggplot2::ggplot(job_analysis, ggplot2::aes(x = reorder(job, success_rate), y = success_rate, fill = success_rate)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::geom_text(ggplot2::aes(label = paste0(success_rate, "%")), hjust = -0.15, size = 3) +
  ggplot2::scale_fill_gradient(low = "#FEE08B", high = "#D53E4F") +
  ggplot2::coord_flip() +
  ggplot2::labs(title = "Subscription Rate by Job", x = "Job", y = "Success Rate (%)") +
  ggplot2::theme(legend.position = "none")
p5

# P6: Education stacked by response
edu_analysis <- data_clean %>%
  dplyr::count(education, y) %>%
  dplyr::group_by(education) %>%
  dplyr::mutate(pct = n / sum(n))

p6 <- ggplot2::ggplot(edu_analysis, ggplot2::aes(stats::reorder(education, -n), pct, fill = y)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::scale_fill_manual(values = pal_no_yes) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::labs(
    title = "Education Level and Subscription",
    x = "Education", y = "Share",
    fill = "Subscription Decision"
  ) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
p6


# P7: Month activity stacked
month_order <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
data_clean <- data_clean %>% mutate(month = tolower(month)) # Ensure in-case sensitive comparison
data_clean$month <- factor(data_clean$month, levels = month_order)

month_analysis <- data_clean %>%
  dplyr::filter(!is.na(month)) %>%
  dplyr::count(month, y)

p7 <- ggplot2::ggplot(month_analysis, ggplot2::aes(month, n, fill = y)) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(values = pal_no_yes) +
  ggplot2::labs(
    title = "Campaign Activity by Month",
    x = "Month", y = "Contacts",
    fill = "Subscription Decision"
  ) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
p7

# P8: Contact method effectiveness (proportion)
contact_analysis <- data_clean %>%
  dplyr::count(contact, y) %>%
  dplyr::group_by(contact) %>%
  dplyr::mutate(pct = n / sum(n))

p8 <- ggplot2::ggplot(contact_analysis, ggplot2::aes(contact, pct, fill = y)) +
  ggplot2::geom_col(position = "fill", width = 0.6) +
  ggplot2::scale_fill_manual(values = pal_no_yes) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  ggplot2::labs(
    title = "Contact Method Effectiveness",
    x = "Method", y = "Proportion",
    fill = "Subscription Decision"
  )
p8

# P9: Correlation among economic indicators (+ binary y)
econ <- data_clean %>%
  dplyr::transmute(emp.var.rate, cons.price.idx, cons.conf.idx, euribor3m, nr.employed,
                   y_num = as.numeric(y == "yes"))
cor_mat <- stats::cor(econ, use = "complete.obs")

p9 <- ggcorrplot::ggcorrplot(
  cor_mat, method = "circle", type = "lower", lab = TRUE, lab_size = 3,
  colors = c("#E74C3C", "white", "#27AE60"),
  title = "Correlation: Economic Indicators & Subscription Decision (binary)"
)
p9

# P10: Quick feature importance (RF) — duration already absent in data_clean
set.seed(42)
rf_df <- data_clean %>%
  dplyr::mutate(across(where(is.character), factor),
                y = factor(y))
rf_sample <- rf_df[sample(nrow(rf_df), min(5000, nrow(rf_df))), ]
rf_quick <- randomForest::randomForest(y ~ ., data = rf_sample, ntree = 100,
                                       importance = TRUE, na.action = na.omit)
imp <- randomForest::importance(rf_quick)
imp_df <- tibble::tibble(variable = rownames(imp),
                         gini = imp[, "MeanDecreaseGini"]) %>%
  dplyr::arrange(dplyr::desc(gini)) %>%
  dplyr::slice_head(n = 15)

p10 <- ggplot2::ggplot(imp_df, ggplot2::aes(stats::reorder(variable, gini), gini, fill = gini)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::coord_flip() +
  ggplot2::scale_fill_gradient(low = "#FFFFCC", high = "#006837") +
  ggplot2::labs(title = "Feature Importance (Top 15, RF on data_clean)", x = "Feature", y = "Gini") +
  ggplot2::theme(legend.position = "none")
p10

# Layout (optional): combine or print individually
dashboard <- (p1 + p2) / (p3 + p4) / (p5 + p6) / (p7 + p8) / (p9 + p10) +
  patchwork::plot_annotation(
    title = "Bank Marketing Campaign — EDA & Feature Insights (data_clean)",
    subtitle = "Patterns and relationships in term deposit subscription"
  )
dashboard #export with width is 2400 and tick maitain ratio
