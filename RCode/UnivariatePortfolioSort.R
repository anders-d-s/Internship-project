load("Data/monthly_ivol_252d.RData")
load("Data/mom_signal_12_1.RData")
monthly_mom <- mom_signal_12_1[-(1:12), ]
row.names(monthly_mom) <- NULL
load("Data/monthly_factors.RData")

################################################################################
# Function to create 30/40/30 univariate portfolio assignments
################################################################################

build_univariate_groups <- function(df,
                                    low_label = "P1",
                                    mid_label = "P2",
                                    high_label = "P3") {
  
  factor_cols <- setdiff(names(df), "date")
  
  groups <- df
  groups[, factor_cols] <- NA
  
  for (i in seq_len(nrow(df))) {
    
    vals <- as.numeric(df[i, factor_cols])
    
    p30 <- quantile(vals, 0.30, na.rm = TRUE)
    p70 <- quantile(vals, 0.70, na.rm = TRUE)
    
    grp <- ifelse(vals <= p30, low_label,
                  ifelse(vals <= p70, mid_label, high_label))
    
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

# IVOL groups
ivol_groups <- build_univariate_groups(
  monthly_ivol,
  low_label = "IV1",
  mid_label = "IV2",
  high_label = "IV3"
)

# Momentum groups
mom_groups <- build_univariate_groups(
  monthly_mom,
  low_label = "M1",
  mid_label = "M2",
  high_label = "M3"
)

#--------------------------------------------------------------------------

# Univariate portfolio formation - IV only
portfolio_returns_IV <- data.frame(
  date = ivol_groups$date,
  IV1 = NA, IV2 = NA, IV3 = NA
)
factor_cols <- setdiff(names(ivol_groups), "date")

for (i in 1:nrow(ivol_groups)) {
  
  current_date <- ivol_groups$date[i]
  ivol_grp <- as.character(ivol_groups[i, factor_cols])
  
  mf_row <- which(monthly_factors$date == current_date)
  returns <- as.numeric(monthly_factors[mf_row, factor_cols])
  
  for (iv in c("IV1", "IV2", "IV3")) {
    idx <- (ivol_grp == iv)
    avg_ret <- mean(returns[idx], na.rm = TRUE)
    portfolio_returns_IV[i, iv] <- ifelse(is.nan(avg_ret), NA, avg_ret)
  }
}

portfolio_returns_IV$IV_LS <- portfolio_returns_IV$IV3 - portfolio_returns_IV$IV1

# Define periods
n_months   <- nrow(portfolio_returns_IV)  # fixed
mid_point  <- floor(n_months / 2)
full_idx   <- 1:n_months
first_idx  <- 1:mid_point
second_idx <- (mid_point + 1):n_months

print_date_range <- function(dates, idx, label) {
  cat(label, ":", as.character(min(dates[idx])), "to", as.character(max(dates[idx])), "\n")
}

# For IV (or M, should be the same dates)
print_date_range(portfolio_returns_IV$date, full_idx,   "Full period")
print_date_range(portfolio_returns_IV$date, first_idx,  "First half")
print_date_range(portfolio_returns_IV$date, second_idx, "Second half")

# Compute mean + t-stat over each period
portfolio_cols_IV <- setdiff(names(portfolio_returns_IV), "date")

portfolio_stats_IV_full   <- compute_mean_tstat(portfolio_returns_IV, portfolio_cols_IV, full_idx)
portfolio_stats_IV_first  <- compute_mean_tstat(portfolio_returns_IV, portfolio_cols_IV, first_idx)
portfolio_stats_IV_second <- compute_mean_tstat(portfolio_returns_IV, portfolio_cols_IV, second_idx)

print_stats(portfolio_stats_IV_full,   "IV - Full period:")
print_stats(portfolio_stats_IV_first,  "IV - First half:")
print_stats(portfolio_stats_IV_second, "IV - Second half:")

#----------------------------------------------------------------------

# Univariate portfolio formation - M only
portfolio_returns_M <- data.frame(
  date = mom_groups$date,
  M1 = NA, M2 = NA, M3 = NA
)
factor_cols <- setdiff(names(mom_groups), "date")

for (i in 1:nrow(mom_groups)) {
  
  current_date <- mom_groups$date[i]
  mom_grp <- as.character(mom_groups[i, factor_cols])
  
  mf_row <- which(monthly_factors$date == current_date)
  returns <- as.numeric(monthly_factors[mf_row, factor_cols])
  
  for (mo in c("M1", "M2", "M3")) {
    idx <- (mom_grp == mo)
    avg_ret <- mean(returns[idx], na.rm = TRUE)
    portfolio_returns_M[i, mo] <- ifelse(is.nan(avg_ret), NA, avg_ret)
  }
}

portfolio_returns_M$M_LS <- portfolio_returns_M$M3 - portfolio_returns_M$M1

# Define periods
n_months   <- nrow(portfolio_returns_M)
mid_point  <- floor(n_months / 2)
full_idx   <- 1:n_months
first_idx  <- 1:mid_point
second_idx <- (mid_point + 1):n_months

print_date_range <- function(dates, idx, label) {
  cat(label, ":", as.character(min(dates[idx])), "to", as.character(max(dates[idx])), "\n")
}

# For IV (or M, should be the same dates)
print_date_range(portfolio_returns_IV$date, full_idx,   "Full period")
print_date_range(portfolio_returns_IV$date, first_idx,  "First half")
print_date_range(portfolio_returns_IV$date, second_idx, "Second half")

# Compute mean + t-stat over each period
portfolio_cols_M <- setdiff(names(portfolio_returns_M), "date")

portfolio_stats_M_full   <- compute_mean_tstat(portfolio_returns_M, portfolio_cols_M, full_idx)
portfolio_stats_M_first  <- compute_mean_tstat(portfolio_returns_M, portfolio_cols_M, first_idx)
portfolio_stats_M_second <- compute_mean_tstat(portfolio_returns_M, portfolio_cols_M, second_idx)

print_stats(portfolio_stats_M_full,   "M - Full period:")
print_stats(portfolio_stats_M_first,  "M - First half:")
print_stats(portfolio_stats_M_second, "M - Second half:")

#keep <- c(keep, "portfolio_returns_M", "portfolio_returns_IV")
#rm(list = setdiff(ls(), keep))