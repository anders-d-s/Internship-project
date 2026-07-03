mom_groups <- mom_groups_12_1_252d_ivol
load("Data/monthly_factors.RData")

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

# Exclude the date column
portfolio_cols <- setdiff(names(portfolio_returns_5x5), "date")

# Compute column-wise mean
portfolio_means <- colMeans(portfolio_returns_5x5[, portfolio_cols], na.rm = TRUE)

# View results
print(portfolio_means*100)

#clean up
keep <- c(keep, "portfolio_returns_5x5", "portfolio_means")
rm(list = setdiff(ls(), keep))