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

# Compute means over each period
portfolio_cols_IV <- setdiff(names(portfolio_returns_IV), "date")

portfolio_means_IV_full   <- colMeans(portfolio_returns_IV[full_idx,   portfolio_cols_IV], na.rm = TRUE)
portfolio_means_IV_first  <- colMeans(portfolio_returns_IV[first_idx,  portfolio_cols_IV], na.rm = TRUE)
portfolio_means_IV_second <- colMeans(portfolio_returns_IV[second_idx, portfolio_cols_IV], na.rm = TRUE)

cat("IV - Full period:\n");  print(round(portfolio_means_IV_full   * 100, 2))
cat("IV - First half:\n");   print(round(portfolio_means_IV_first  * 100, 2))
cat("IV - Second half:\n");  print(round(portfolio_means_IV_second * 100, 2))

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

# Compute means over each period
portfolio_cols_M <- setdiff(names(portfolio_returns_M), "date")

portfolio_means_M_full   <- colMeans(portfolio_returns_M[full_idx,   portfolio_cols_M], na.rm = TRUE)
portfolio_means_M_first  <- colMeans(portfolio_returns_M[first_idx,  portfolio_cols_M], na.rm = TRUE)
portfolio_means_M_second <- colMeans(portfolio_returns_M[second_idx, portfolio_cols_M], na.rm = TRUE)

cat("M - Full period:\n");  print(round(portfolio_means_M_full   * 100, 2))
cat("M - First half:\n");   print(round(portfolio_means_M_first  * 100, 2))
cat("M - Second half:\n");  print(round(portfolio_means_M_second * 100, 2))

keep <- c(keep, "portfolio_returns_M", "portfolio_returns_IV")
rm(list = setdiff(ls(), keep))
