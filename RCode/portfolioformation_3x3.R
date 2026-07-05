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
# Create dataframe to store 9 bivariate portfolio returns
portfolio_returns_3x3 <- data.frame(
  date = ivol_groups$date,
  IV1_M1 = NA, IV1_M2 = NA, IV1_M3 = NA,
  IV2_M1 = NA, IV2_M2 = NA, IV2_M3 = NA,
  IV3_M1 = NA, IV3_M2 = NA, IV3_M3 = NA
)
# Loop over months
for (i in 1:nrow(ivol_groups)) {
  
  current_date <- ivol_groups$date[i]
  
  ivol_grp <- as.character(ivol_groups[i, factor_cols])
  mom_grp  <- as.character(mom_groups[i, factor_cols])
  
  # Match by date instead of row index
  mf_row <- which(monthly_factors$date == current_date)
  returns <- as.numeric(monthly_factors[mf_row, factor_cols])
  
  for (iv in c("IV1","IV2","IV3")) {
    for (mo in c("M1","M2","M3")) {
      idx <- (ivol_grp == iv) & (mom_grp == mo)
      port_name <- paste0(iv, "_", mo)
      avg_ret <- mean(returns[idx], na.rm = TRUE)
      portfolio_returns_3x3[i, port_name] <- ifelse(is.nan(avg_ret), NA, avg_ret)
    }
  }
}

# Momentum long-short WITHIN each IV bucket: M3 - M1, holding IV fixed
portfolio_returns_3x3$IV1_M_LS <- portfolio_returns_3x3$IV1_M3 - portfolio_returns_3x3$IV1_M1
portfolio_returns_3x3$IV2_M_LS <- portfolio_returns_3x3$IV2_M3 - portfolio_returns_3x3$IV2_M1
portfolio_returns_3x3$IV3_M_LS <- portfolio_returns_3x3$IV3_M3 - portfolio_returns_3x3$IV3_M1

# IV long-short WITHIN each M bucket: IV3 - IV1, holding M fixed
portfolio_returns_3x3$M1_IV_LS <- portfolio_returns_3x3$IV3_M1 - portfolio_returns_3x3$IV1_M1
portfolio_returns_3x3$M2_IV_LS <- portfolio_returns_3x3$IV3_M2 - portfolio_returns_3x3$IV1_M2
portfolio_returns_3x3$M3_IV_LS <- portfolio_returns_3x3$IV3_M3 - portfolio_returns_3x3$IV1_M3

# LS-LS intersection (diff-in-diff, bottom-right corner of the table)
portfolio_returns_3x3$LS_LS <- portfolio_returns_3x3$IV3_M_LS - portfolio_returns_3x3$IV1_M_LS

# Exclude the date column
portfolio_cols <- setdiff(names(portfolio_returns_3x3), "date")

# Compute mean + t-stat for all portfolios and long-shorts (full period)
full_idx_3x3 <- 1:nrow(portfolio_returns_3x3)
portfolio_stats_3x3 <- compute_mean_tstat(portfolio_returns_3x3, portfolio_cols, full_idx_3x3)

# View results, rounded to 3 digits
df_out_3x3 <- data.frame(
  mean_pct = round(portfolio_stats_3x3$mean * 100, 3),
  tstat    = round(portfolio_stats_3x3$tstat, 3),
  row.names = rownames(portfolio_stats_3x3)
)
print(df_out_3x3)

#clean up
keep <- c(keep, "portfolio_returns_3x3")
rm(list = setdiff(ls(), keep))