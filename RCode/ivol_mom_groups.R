load("monthly_ivol.RData")
load("monthly_factors.RData")

#remove mkt factor
monthly_factors <- monthly_factors[,setdiff(names(monthly_factors), "mkt")]
row.names(monthly_factors) <- NULL

#################################################################################
#IVOL
#30p/70p

# Create empty dataframe for groups
ivol_groups <- monthly_ivol
ivol_groups[, -which(names(ivol_groups) == "date")] <- NA

# Factor columns (exclude date)
factor_cols <- setdiff(names(monthly_ivol), "date")

# Loop over rows (months)
for (i in 1:nrow(monthly_ivol)) {
  
  # Extract row values
  row_vals <- as.numeric(monthly_ivol[i, factor_cols])
  
  # Compute breakpoints
  p30 <- quantile(row_vals, probs = 0.3, na.rm = TRUE)
  p70 <- quantile(row_vals, probs = 0.7, na.rm = TRUE)
  
  # Assign S / M / L
  groups <- ifelse(row_vals <= p30, "IV1",
                   ifelse(row_vals <= p70, "IV2", "IV3"))
  
  # Store result
  ivol_groups[i, factor_cols] <- as.list(groups)
}

save(ivol_groups, file = "ivol_groups.RData")

###########################################################################
#MOM
factor_cols <- setdiff(names(monthly_factors), "date")

# Keep date column, set others to NA
mom_groups <- monthly_factors[, c("date", factor_cols)]
mom_groups[, factor_cols] <- NA
mom_groups <- mom_groups[-(1:12),]
row.names(mom_groups) <- NULL

for (i in 1:nrow(monthly_factors)-12) {
  
# Current month values
#monthly_factor i+11 = 1992-04-30 (momentum is lagged return)
#ivol_groups i = 1992-05-31
mom_vals <- as.numeric(monthly_factors[i+11, factor_cols])
ivol_grp <- as.character(ivol_groups[i, factor_cols])

# Initialize MOM row
mom_row <- rep(NA, length(mom_vals))

for (iv in c("IV1","IV2","IV3")) {
  
  # Indices for this IVOL group
  idx_iv <- which(ivol_grp == iv)
  
  if (length(idx_iv) > 0) {
    # MOM values within IVOL group
    mom_subset <- mom_vals[idx_iv]
    
    # Compute 30/70 breakpoints within IVOL group
    p30 <- quantile(mom_subset, 0.3, na.rm = TRUE)
    p70 <- quantile(mom_subset, 0.7, na.rm = TRUE)
    
    # Assign MOM groups
    mom_row[idx_iv] <- ifelse(mom_subset <= p30, "M1",
                              ifelse(mom_subset <= p70, "M2", "M3"))
  }
}
  # Store dependent MOM assignment in factor columns only
mom_groups[i, factor_cols] <- mom_row
}

save(mom_groups, file = "mom_groups.RData")


###########################################################################
#Test

# Create table counts first
tab <- table(
  IVOL = as.vector(as.matrix(ivol_groups[, factor_cols])),
  MOM  = as.vector(as.matrix(mom_groups[, factor_cols]))
)

# Convert to percentages within each IVOL row
tab_percent <- prop.table(tab, margin = 1) * 100

# View results
tab_percent
