mom_groups <- mom_groups_2_1_252d_ivol
load("Data/monthly_factors.RData")

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

#Portfolio formation
# Factor columns (exclude date)
factor_cols <- setdiff(names(ivol_groups), "date")
# Create dataframe to store 25 bivariate portfolio returns
portfolio_returns_5x5 <- data.frame(
  date = ivol_groups$date,
  IV1_M1 = NA, IV1_M2 = NA, IV1_M3 = NA, IV1_M4 = NA, IV1_M5 = NA,
  IV2_M1 = NA, IV2_M2 = NA, IV2_M3 = NA, IV2_M4 = NA, IV2_M5 = NA,
  IV3_M1 = NA, IV3_M2 = NA, IV3_M3 = NA, IV3_M4 = NA, IV3_M5 = NA,
  IV4_M1 = NA, IV4_M2 = NA, IV4_M3 = NA, IV4_M4 = NA, IV4_M5 = NA,
  IV5_M1 = NA, IV5_M2 = NA, IV5_M3 = NA, IV5_M4 = NA, IV5_M5 = NA
)
# Loop over months
for (i in 1:nrow(ivol_groups)) {
  
  current_date <- ivol_groups$date[i]
  
  ivol_grp <- as.character(ivol_groups[i, factor_cols])
  mom_grp  <- as.character(mom_groups[i, factor_cols])
  
  # Match by date instead of row index
  mf_row <- which(monthly_factors$date == current_date)
  returns <- as.numeric(monthly_factors[mf_row, factor_cols])
  
  for (iv in c("IV1","IV2","IV3","IV4","IV5")) {
    for (mo in c("M1","M2","M3","M4","M5")) {
      idx <- (ivol_grp == iv) & (mom_grp == mo)
      port_name <- paste0(iv, "_", mo)
      avg_ret <- mean(returns[idx], na.rm = TRUE)
      portfolio_returns_5x5[i, port_name] <- ifelse(is.nan(avg_ret), NA, avg_ret)
    }
  }
}

# Momentum long-short WITHIN each IV bucket: M5 - M1, holding IV fixed
for (iv in c("IV1","IV2","IV3","IV4","IV5")) {
  col_m5 <- paste0(iv, "_M5")
  col_m1 <- paste0(iv, "_M1")
  portfolio_returns_5x5[[paste0(iv, "_M_LS")]] <- portfolio_returns_5x5[[col_m5]] - portfolio_returns_5x5[[col_m1]]
}

# IV long-short WITHIN each M bucket: IV5 - IV1, holding M fixed
for (mo in c("M1","M2","M3","M4","M5")) {
  col_iv5 <- paste0("IV5_", mo)
  col_iv1 <- paste0("IV1_", mo)
  portfolio_returns_5x5[[paste0(mo, "_IV_LS")]] <- portfolio_returns_5x5[[col_iv5]] - portfolio_returns_5x5[[col_iv1]]
}

# LS-LS intersection (diff-in-diff)
portfolio_returns_5x5$LS_LS <- portfolio_returns_5x5$IV5_M_LS - portfolio_returns_5x5$IV1_M_LS

# Exclude the date column
portfolio_cols <- setdiff(names(portfolio_returns_5x5), "date")

# Compute mean + t-stat for all portfolios and long-shorts (full period)
full_idx_5x5 <- 1:nrow(portfolio_returns_5x5)
portfolio_stats_5x5 <- compute_mean_tstat(portfolio_returns_5x5, portfolio_cols, full_idx_5x5)

# View results, rounded to 3 digits
df_out_5x5 <- data.frame(
  mean_pct = round(portfolio_stats_5x5$mean * 100, 3),
  tstat    = round(portfolio_stats_5x5$tstat, 3),
  row.names = rownames(portfolio_stats_5x5)
)
print(df_out_5x5)

#clean up
keep <- c(keep, "portfolio_returns_5x5", "portfolio_stats_5x5")
rm(list = setdiff(ls(), keep))