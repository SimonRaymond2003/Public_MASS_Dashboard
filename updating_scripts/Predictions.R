# ============================================================================
# Forecast Prep — LM Bootstrap (1 Lag, Categorical Features)
# Trains 2 LMs: exp ~ exp_lag1 + covariates, rev ~ rev_lag1 + covariates
# 100 bootstrap iterations, saves everything the Shiny app needs
# ============================================================================

library(readxl)
library(dplyr)


# ── 1. LOAD & PREP ──────────────────────────────────────────────────────────

raw <- read_excel("MASS_data/SIMON - 2025-10-24-MASS-Culture-Data-Summary-Real_Dollars.xlsx")

df <- raw %>%
  select(
    org       = `Business Number`,
    Year,
    Province,
    Category,
    Discipline,
    org_age   = `Organization Age`,
    rev       = `Total Revenue`,
    exp       = `Total Expenditures`
  ) %>%
  arrange(org, Year)

cat("Raw rows:", nrow(df), "\n")

# ── 2. TRIM OUTLIERS (5th–95th on rev & exp) ────────────────────────────────

df <- df %>% filter(!is.na(rev), !is.na(exp))

rev_05 <- quantile(df$rev, 0.05); rev_95 <- quantile(df$rev, 0.95)
exp_05 <- quantile(df$exp, 0.05); exp_95 <- quantile(df$exp, 0.95)

cat("Rev trim: $", round(rev_05), "–", round(rev_95), "\n")
cat("Exp trim: $", round(exp_05), "–", round(exp_95), "\n")

df <- df %>%
  filter(rev >= rev_05, rev <= rev_95,
         exp >= exp_05, exp <= exp_95)

cat("After trim:", nrow(df), "rows\n")

# ── 3. CREATE 1-LAG FEATURES ────────────────────────────────────────────────

df <- df %>%
  group_by(org) %>%
  mutate(
    rev_lag1 = lag(rev, 1, order_by = Year),
    exp_lag1 = lag(exp, 1, order_by = Year)
  ) %>%
  ungroup()

# Exp model data: needs exp_lag1 + org_age
df_exp <- df %>%
  filter(!is.na(exp_lag1), !is.na(org_age)) %>%
  mutate(Province   = factor(Province),
         Category   = factor(Category),
         Discipline = factor(Discipline))

# Rev model data: needs rev_lag1 + org_age
df_rev <- df %>%
  filter(!is.na(rev_lag1), !is.na(org_age)) %>%
  mutate(Province   = factor(Province),
         Category   = factor(Category),
         Discipline = factor(Discipline))

cat("\nExp model rows:", nrow(df_exp), "| Unique orgs:", n_distinct(df_exp$org))
cat("\nRev model rows:", nrow(df_rev), "| Unique orgs:", n_distinct(df_rev$org), "\n")

# ── 4. QUANTILE BINS (for error-by-size plots) ──────────────────────────────

n_bins <- 10

df_exp$exp_bin <- cut(df_exp$exp,
                      breaks = quantile(df_exp$exp, probs = seq(0, 1, length.out = n_bins + 1)),
                      include.lowest = TRUE, dig.lab = 10)

df_rev$rev_bin <- cut(df_rev$rev,
                      breaks = quantile(df_rev$rev, probs = seq(0, 1, length.out = n_bins + 1)),
                      include.lowest = TRUE, dig.lab = 10)

# ── 5. BOOTSTRAP ────────────────────────────────────────────────────────────

B <- 100
set.seed(42)

cat("\nRunning", B, "bootstrap iterations...\n")
t0 <- Sys.time()

# Storage for per-obs predictions (we sample OOB predictions across iterations)
exp_preds_all <- list()
rev_preds_all <- list()
exp_metrics   <- data.frame()
rev_metrics   <- data.frame()

for (i in 1:B) {
  if (i %% 10 == 0) cat("  Iteration", i, "\n")
  
  # ── EXP MODEL ──
  idx_e   <- sample(nrow(df_exp), replace = TRUE)
  trn_e   <- df_exp[idx_e, ]
  tst_idx <- setdiff(1:nrow(df_exp), unique(idx_e))
  tst_e   <- df_exp[tst_idx, ]
  
  lm_e <- lm(exp ~ exp_lag1 + Province + Category + Discipline + org_age, data = trn_e)
  pred_e <- predict(lm_e, newdata = tst_e)
  
  res_e <- tst_e$exp - pred_e
  exp_metrics <- rbind(exp_metrics, data.frame(
    iter = i,
    rmse = sqrt(mean(res_e^2)),
    mae  = mean(abs(res_e))
  ))
  
  exp_preds_all[[i]] <- data.frame(
    iter     = i,
    row_id   = tst_idx,
    actual   = tst_e$exp,
    predicted = pred_e,
    residual = res_e,
    exp_bin  = tst_e$exp_bin,
    exp_val  = tst_e$exp
  )
  
  # ── REV MODEL ──
  idx_r   <- sample(nrow(df_rev), replace = TRUE)
  trn_r   <- df_rev[idx_r, ]
  tst_idx_r <- setdiff(1:nrow(df_rev), unique(idx_r))
  tst_r   <- df_rev[tst_idx_r, ]
  
  lm_r <- lm(rev ~ rev_lag1 + Province + Category + Discipline + org_age, data = trn_r)
  pred_r <- predict(lm_r, newdata = tst_r)
  
  res_r <- tst_r$rev - pred_r
  rev_metrics <- rbind(rev_metrics, data.frame(
    iter = i,
    rmse = sqrt(mean(res_r^2)),
    mae  = mean(abs(res_r))
  ))
  
  rev_preds_all[[i]] <- data.frame(
    iter     = i,
    row_id   = tst_idx_r,
    actual   = tst_r$rev,
    predicted = pred_r,
    residual = res_r,
    rev_bin  = tst_r$rev_bin,
    rev_val  = tst_r$rev
  )
}

elapsed <- round(difftime(Sys.time(), t0, units = "secs"), 1)
cat("Done in", elapsed, "seconds\n\n")

exp_preds_df <- do.call(rbind, exp_preds_all)
rev_preds_df <- do.call(rbind, rev_preds_all)

# ── 6. TRAIN FINAL MODELS ON FULL DATA (for app calculator) ─────────────────

lm_exp_final <- lm(exp ~ exp_lag1 + Province + Category + Discipline + org_age, data = df_exp)
lm_rev_final <- lm(rev ~ rev_lag1 + Province + Category + Discipline + org_age, data = df_rev)

cat("Exp model R²:", round(summary(lm_exp_final)$r.squared, 4), "\n")
cat("Rev model R²:", round(summary(lm_rev_final)$r.squared, 4), "\n")

# ── 7. SUMMARY STATS ────────────────────────────────────────────────────────

summarize_metrics <- function(metrics_df, label) {
  cat(sprintf("\n%s:\n", label))
  cat(sprintf("  RMSE: Mean=%.0f  SD=%.0f  95%%CI=[%.0f, %.0f]\n",
              mean(metrics_df$rmse), sd(metrics_df$rmse),
              mean(metrics_df$rmse) - 1.96 * sd(metrics_df$rmse),
              mean(metrics_df$rmse) + 1.96 * sd(metrics_df$rmse)))
  cat(sprintf("  MAE:  Mean=%.0f  SD=%.0f  95%%CI=[%.0f, %.0f]\n",
              mean(metrics_df$mae), sd(metrics_df$mae),
              mean(metrics_df$mae) - 1.96 * sd(metrics_df$mae),
              mean(metrics_df$mae) + 1.96 * sd(metrics_df$mae)))
}

summarize_metrics(exp_metrics, "EXPENDITURE LM")
summarize_metrics(rev_metrics, "REVENUE LM")

# ── 8. SAVE EVERYTHING ──────────────────────────────────────────────────────

dir.create("updating_data/predictions", recursive = TRUE, showWarnings = FALSE)
saveRDS(lm_exp_final,  "updating_data/predictions/lm_exp_final.rds")
saveRDS(lm_rev_final,  "updating_data/predictions/lm_rev_final.rds")
saveRDS(exp_metrics,    "updating_data/predictions/exp_metrics.rds")
saveRDS(rev_metrics,    "updating_data/predictions/rev_metrics.rds")
saveRDS(exp_preds_df,   "updating_data/predictions/exp_preds.rds")
saveRDS(rev_preds_df,   "updating_data/predictions/rev_preds.rds")

# Save the quantile breakpoints for the app
saveRDS(
  list(
    exp_breaks = quantile(df_exp$exp, probs = seq(0, 1, length.out = n_bins + 1)),
    rev_breaks = quantile(df_rev$rev, probs = seq(0, 1, length.out = n_bins + 1))
  ),
  "updating_data/predictions/quantile_breaks.rds"
)

# Save factor levels so app can ensure new data has matching levels
saveRDS(
  list(
    provinces   = levels(df_exp$Province),
    categories  = levels(df_exp$Category),
    disciplines = levels(df_exp$Discipline)
  ),
  "updating_data/predictions/factor_levels.rds"
)

# ── 9. DIAGNOSTIC PLOT: MAE BY ORG SIZE ──────────────────────────────────────
# One clean chart per model showing how prediction error scales with org size.
# X-axis labels show actual dollar ranges so non-technical readers can find
# their organization's bracket and see what accuracy to expect.

NAVY    <- "#1B1464"
RED     <- "#E24B4A"
LTBLUE  <- rgb(0.12, 0.27, 0.63, 0.12)
MIDBLUE <- rgb(0.12, 0.27, 0.63, 0.35)

# ── Dollar formatter ($42K, $1.3M, etc.) ──
fmt_dollar <- function(x) {
  ifelse(x >= 1e6,
         paste0("$", formatC(x / 1e6, format = "f", digits = 1), "M"),
         paste0("$", formatC(x / 1e3, format = "f", digits = 0), "K"))
}

# ── Build MAE stats with readable dollar-range labels ──
build_mae_stats <- function(preds_df, bin_col, val_col, breaks) {
  preds_df$abs_err <- abs(preds_df$residual)
  preds_df$bin     <- preds_df[[bin_col]]
  preds_df$val     <- preds_df[[val_col]]
  
  bin_order <- preds_df %>%
    filter(!is.na(bin)) %>%
    group_by(bin) %>%
    summarise(med_val = median(val, na.rm = TRUE), .groups = "drop") %>%
    arrange(med_val) %>%
    mutate(bin_num = row_number())
  
  # Human-readable range labels from the quantile breakpoints
  n <- length(breaks)
  range_labels <- character(n - 1)
  for (j in 1:(n - 1)) {
    range_labels[j] <- paste0(fmt_dollar(breaks[j]), " – ", fmt_dollar(breaks[j + 1]))
  }
  bin_order$range_label <- range_labels[bin_order$bin_num]
  
  preds_df %>%
    filter(!is.na(bin)) %>%
    left_join(bin_order %>% select(bin, bin_num, range_label), by = "bin") %>%
    group_by(bin_num, range_label) %>%
    summarise(
      mae  = mean(abs_err, na.rm = TRUE),
      lo95 = quantile(abs_err, 0.025, na.rm = TRUE),
      hi95 = quantile(abs_err, 0.975, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(bin_num)
}

# ── Plot: MAE by org size ──
plot_mae <- function(stats, lbl, filename) {
  n     <- nrow(stats)
  y_max <- max(stats$hi95, na.rm = TRUE) / 1e3 * 1.15
  
  png(filename, width = 1050, height = 750, res = 150)
  par(mar = c(7.5, 5.5, 2.5, 1.5), family = "sans")
  
  # Empty canvas
  plot(NULL, xlim = c(0.3, n + 0.7), ylim = c(0, y_max),
       xlab = "", ylab = "",
       xaxt = "n", yaxt = "n", bty = "n")
  
  # Subtle gridlines
  y_ticks <- pretty(c(0, y_max), n = 5)
  abline(h = y_ticks, col = "grey88", lwd = 0.6)
  
  # Y axis — dollar amounts
  axis(2, at = y_ticks,
       labels = paste0("$", formatC(y_ticks, format = "f", digits = 0), "K"),
       las = 1, cex.axis = 0.75, col = "grey50", col.axis = "grey30")
  mtext("Average Prediction Error (MAE)", side = 2, line = 4, cex = 0.9, col = "grey20")
  
  # X axis — angled dollar-range labels
  axis(1, at = 1:n, labels = FALSE)
  text(x = 1:n, y = par("usr")[3] - (y_max * 0.04),
       labels = stats$range_label,
       srt = 35, adj = 1, xpd = TRUE, cex = 0.6, col = "grey30")
  mtext(paste0("Organization ", lbl, " (grouped into 10 equal-sized brackets)"),
        side = 1, line = 6, cex = 0.85, col = "grey20")
  
  # 95% prediction interval bars
  rect(xleft   = 1:n - 0.28,
       ybottom = stats$lo95 / 1e3,
       xright  = 1:n + 0.28,
       ytop    = stats$hi95 / 1e3,
       col = LTBLUE, border = MIDBLUE, lwd = 0.8)
  
  # MAE line + dots
  lines(1:n, stats$mae / 1e3, col = NAVY, lwd = 1.8)
  points(1:n, stats$mae / 1e3, pch = 19, col = NAVY, cex = 1.1)
  
  # MAE value labels
  text(1:n, stats$mae / 1e3 + (y_max * 0.035),
       labels = paste0("$", formatC(stats$mae / 1e3, format = "f", digits = 0), "K"),
       cex = 0.58, col = NAVY, font = 2)
  
  # Title
  mtext(paste0("How Prediction Error Grows with ", lbl, " Size"),
        side = 3, line = 0.8, cex = 1.05, col = "grey15", font = 2)
  
  # Legend
  legend("topleft",
         legend = c("Mean Absolute Error",
                    "95% prediction interval"),
         col    = c(NAVY, MIDBLUE),
         pch    = c(19, 15),
         pt.cex = c(1.2, 2),
         cex    = 0.7, bty = "n", text.col = "grey30")
  
  dev.off()
  cat("  Saved:", filename, "\n")
}

cat("\nGenerating diagnostic plots...\n")

qbreaks <- readRDS("updating_data/predictions/quantile_breaks.rds")

# Expenditure
exp_mae_stats <- build_mae_stats(exp_preds_df, "exp_bin", "exp_val", qbreaks$exp_breaks)
plot_mae(exp_mae_stats, "Expenditure", "updating_data/predictions/mae_exp.png")

# Revenue
rev_mae_stats <- build_mae_stats(rev_preds_df, "rev_bin", "rev_val", qbreaks$rev_breaks)
plot_mae(rev_mae_stats, "Revenue", "updating_data/predictions/mae_rev.png")
