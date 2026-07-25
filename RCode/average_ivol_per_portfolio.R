source("Rcode/UnivariatePortfolioSort.R")
rm(list = setdiff(ls(), c("mom_groups","ivol_groups")))

load("Data/monthly_ivol_252d.RData")
load("Data/mom_signal_2_1.RData")
monthly_mom <- mom_signal_2_1[-(1:12), ]
row.names(monthly_mom) <- NULL
load("Data/monthly_factors.RData")

# Annualized IVOL in percent
factor_cols_ivol <- setdiff(names(monthly_ivol), "date")
monthly_ivol[, factor_cols_ivol] <- monthly_ivol[, factor_cols_ivol] * sqrt(252) * 100

################################################################################
# Generalized function to create univariate portfolio assignments (any # groups)
################################################################################

build_univariate_groups_n <- function(df, labels) {
  
  n_groups <- length(labels)
  breakpoints <- seq(0, 1, length.out = n_groups + 1)
  
  factor_cols <- setdiff(names(df), "date")
  
  groups <- df
  groups[, factor_cols] <- NA
  
  for (i in seq_len(nrow(df))) {
    
    vals <- as.numeric(df[i, factor_cols])
    qs <- quantile(vals, probs = breakpoints, na.rm = TRUE)
    
    grp <- rep(NA_character_, length(vals))
    for (g in seq_len(n_groups)) {
      if (g == 1) {
        idx <- vals <= qs[g + 1]
      } else {
        idx <- vals > qs[g] & vals <= qs[g + 1]
      }
      grp[idx] <- labels[g]
    }
    
    groups[i, factor_cols] <- as.list(grp)
  }
  
  groups
}

################################################################################
# Helper: mean + t-stat for each column over a given set of rows
################################################################################

compute_mean_tstat <- function(df, cols, idx) {
  out <- data.frame(
    mean  = rep(NA_real_, length(cols)),
    tstat = rep(NA_real_, length(cols)),
    row.names = cols
  )
  
  for (cl in cols) {
    x <- df[idx, cl]
    x <- x[!is.na(x)]
    if (length(x) > 1) {
      tt <- t.test(x)
      out[cl, "mean"]  <- tt$estimate
      out[cl, "tstat"] <- tt$statistic
    }
  }
  out
}

print_stats <- function(stats_df, label) {
  cat(label, "\n")
  df_out <- data.frame(
    mean_pct = round(stats_df$mean * 100, 2),
    tstat    = round(stats_df$tstat, 2),
    row.names = rownames(stats_df)
  )
  print(df_out)
}

print_date_range <- function(dates, idx, label) {
  cat(label, ":", as.character(min(dates[idx])), "to", as.character(max(dates[idx])), "\n")
}

# IVOL groups (quintiles) - now sorted on annualized % IVOL
ivol_groups_5 <- build_univariate_groups_n(
  monthly_ivol,
  labels = c("IV1", "IV2", "IV3", "IV4", "IV5")
)

# Momentum groups (quintiles)
mom_groups_5 <- build_univariate_groups_n(
  monthly_mom,
  labels = c("M1", "M2", "M3", "M4", "M5")
)

#--------------------------------------------------------------------------

# Univariate portfolio formation - IV only (5 groups)
portfolio_returns_IV5 <- data.frame(
  date = ivol_groups_5$date,
  IV1 = NA, IV2 = NA, IV3 = NA, IV4 = NA, IV5 = NA
)
factor_cols <- setdiff(names(ivol_groups_5), "date")

for (i in 1:nrow(ivol_groups_5)) {
  
  current_date <- ivol_groups_5$date[i]
  ivol_grp <- as.character(ivol_groups_5[i, factor_cols])
  
  mf_row <- which(monthly_factors$date == current_date)
  returns <- as.numeric(monthly_factors[mf_row, factor_cols])
  
  for (iv in c("IV1","IV2","IV3","IV4","IV5")) {
    idx <- (ivol_grp == iv)
    avg_ret <- mean(returns[idx], na.rm = TRUE)
    portfolio_returns_IV5[i, iv] <- ifelse(is.nan(avg_ret), NA, avg_ret)
  }
}

portfolio_returns_IV5$IV_LS <- portfolio_returns_IV5$IV5 - portfolio_returns_IV5$IV1

n_months   <- nrow(portfolio_returns_IV5)
mid_point  <- floor(n_months / 2)
full_idx   <- 1:n_months
first_idx  <- 1:mid_point
second_idx <- (mid_point + 1):n_months

print_date_range(portfolio_returns_IV5$date, full_idx,   "Full period")
print_date_range(portfolio_returns_IV5$date, first_idx,  "First half")
print_date_range(portfolio_returns_IV5$date, second_idx, "Second half")

portfolio_cols_IV5 <- setdiff(names(portfolio_returns_IV5), "date")

portfolio_stats_IV5_full   <- compute_mean_tstat(portfolio_returns_IV5, portfolio_cols_IV5, full_idx)
portfolio_stats_IV5_first  <- compute_mean_tstat(portfolio_returns_IV5, portfolio_cols_IV5, first_idx)
portfolio_stats_IV5_second <- compute_mean_tstat(portfolio_returns_IV5, portfolio_cols_IV5, second_idx)

print_stats(portfolio_stats_IV5_full,   "IV5 - Full period:")
print_stats(portfolio_stats_IV5_first,  "IV5 - First half:")
print_stats(portfolio_stats_IV5_second, "IV5 - Second half:")

#----------------------------------------------------------------------

# Univariate portfolio formation - M only (5 groups)
portfolio_returns_M5 <- data.frame(
  date = mom_groups_5$date,
  M1 = NA, M2 = NA, M3 = NA, M4 = NA, M5 = NA
)
factor_cols <- setdiff(names(mom_groups_5), "date")

for (i in 1:nrow(mom_groups_5)) {
  
  current_date <- mom_groups_5$date[i]
  mom_grp <- as.character(mom_groups_5[i, factor_cols])
  
  mf_row <- which(monthly_factors$date == current_date)
  returns <- as.numeric(monthly_factors[mf_row, factor_cols])
  
  for (mo in c("M1","M2","M3","M4","M5")) {
    idx <- (mom_grp == mo)
    avg_ret <- mean(returns[idx], na.rm = TRUE)
    portfolio_returns_M5[i, mo] <- ifelse(is.nan(avg_ret), NA, avg_ret)
  }
}

portfolio_returns_M5$M_LS <- portfolio_returns_M5$M5 - portfolio_returns_M5$M1

n_months   <- nrow(portfolio_returns_M5)
mid_point  <- floor(n_months / 2)
full_idx   <- 1:n_months
first_idx  <- 1:mid_point
second_idx <- (mid_point + 1):n_months

print_date_range(portfolio_returns_M5$date, full_idx,   "Full period")
print_date_range(portfolio_returns_M5$date, first_idx,  "First half")
print_date_range(portfolio_returns_M5$date, second_idx, "Second half")

portfolio_cols_M5 <- setdiff(names(portfolio_returns_M5), "date")

portfolio_stats_M5_full   <- compute_mean_tstat(portfolio_returns_M5, portfolio_cols_M5, full_idx)
portfolio_stats_M5_first  <- compute_mean_tstat(portfolio_returns_M5, portfolio_cols_M5, first_idx)
portfolio_stats_M5_second <- compute_mean_tstat(portfolio_returns_M5, portfolio_cols_M5, second_idx)

print_stats(portfolio_stats_M5_full,   "M5 - Full period:")
print_stats(portfolio_stats_M5_first,  "M5 - First half:")
print_stats(portfolio_stats_M5_second, "M5 - Second half:")

#----------------------------------------------------------------------
library(ggplot2)
library(patchwork)

# NOTE: no reload of monthly_ivol here — it stays as annualized % IVOL
# from the conversion earlier in the script

# Generic function: average IVOL per momentum group, works for any number of groups
compute_ivol_by_mom <- function(mom_groups, monthly_ivol, group_labels) {
  
  factor_cols <- setdiff(names(mom_groups), "date")
  
  ivol_by_M <- data.frame(date = mom_groups$date)
  for (g in group_labels) ivol_by_M[[g]] <- NA
  
  for (i in seq_len(nrow(mom_groups))) {
    
    current_date <- mom_groups$date[i]
    mom_grp <- as.character(mom_groups[i, factor_cols])
    
    iv_row <- which(monthly_ivol$date == current_date)
    ivol_vals <- as.numeric(monthly_ivol[iv_row, factor_cols])
    
    for (mo in group_labels) {
      idx <- (mom_grp == mo)
      avg_ivol <- mean(ivol_vals[idx], na.rm = TRUE)
      ivol_by_M[i, mo] <- ifelse(is.nan(avg_ivol), NA, avg_ivol)
    }
  }
  
  # No extra *100 needed — monthly_ivol already annualized % from earlier conversion
  mean_ivol <- colMeans(ivol_by_M[, group_labels], na.rm = TRUE)
  data.frame(group = group_labels, ivol = as.numeric(mean_ivol))
}

# --- 3-group univariate momentum sort ---
ivol_3 <- compute_ivol_by_mom(mom_groups, monthly_ivol, c("M1", "M2", "M3"))
ivol_3$group <- factor(ivol_3$group, levels = c("M1", "M2", "M3"))

# --- 5-group univariate momentum sort ---
ivol_5 <- compute_ivol_by_mom(mom_groups_5, monthly_ivol, c("M1","M2","M3","M4","M5"))
ivol_5$group <- factor(ivol_5$group, levels = c("M1","M2","M3","M4","M5"))

# Shared plotting helper
plot_smile <- function(df, title, line_color) {
  ggplot(df, aes(x = group, y = ivol, group = 1)) +
    geom_line(color = line_color, linewidth = 1) +
    geom_point(color = line_color, size = 3) +
    labs(
      title = title,
      x = NULL,
      y = "Average Annualized IVOL (%)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      panel.grid.minor = element_blank()
    )
}

p3 <- plot_smile(ivol_3, "3-Portfolio Sort", "#1B9E77")
p5 <- plot_smile(ivol_5, "5-Portfolio Sort", "#D95F02")

combined_plot <- p3 | p5

print(combined_plot)

ggsave("png_files/volatility_smile_3v5_univariate.png", plot = combined_plot,
       width = 10, height = 5, dpi = 300, bg = "white")