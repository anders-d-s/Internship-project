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
  
  # IVOL and MOM groups for this month
  ivol_grp <- as.character(ivol_groups[i, factor_cols])
  mom_grp  <- as.character(mom_groups[i, factor_cols])
  
  # Returns for this month
  returns <- as.numeric(monthly_factors[i, factor_cols])
  
  # Loop over all combinations of IVOL × MOM
  for (iv in c("IV1","IV2","IV3")) {
    for (mo in c("M1","M2","M3")) {
      # Logical index for factors in both groups
      idx <- (ivol_grp == iv) & (mom_grp == mo)
      
      # Portfolio name
      port_name <- paste0(iv, "_", mo)
      
      # Average return
      portfolio_returns_3x3[i, port_name] <- mean(returns[idx], na.rm = TRUE)
    }
  }
}


# Exclude the date column
#portfolio_cols <- setdiff(names(portfolio_returns_bi), "date")

# Compute column-wise mean
#portfolio_means <- colMeans(portfolio_returns_bi[, portfolio_cols], na.rm = TRUE)

# View results
#portfolio_means*100

#keep only output variable
rm(list = setdiff(ls(), c("portfolio_returns_3x3","ivol_groups","mom_groups","monthly_factors")))
