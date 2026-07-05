# Exclude irrelevant columns
factors <- monthly_factors[, !names(monthly_factors) %in% c("date", "mkt")]
# Replace NAs with column means
factors_imputed <- as.data.frame(lapply(factors, function(x) {
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}))
# Run PCA
pca_result <- prcomp(factors_imputed, scale. = TRUE, center = TRUE)
# Extract first principal component
pc1 <- pca_result$x[, 1]
pc1_df <- data.frame(date = monthly_factors$date, pc1 = pc1)
portfolio_returns_3x3 <- merge(portfolio_returns_3x3, pc1_df, by = "date")

# Build long-short and LS-LS return series (regressed directly on PC1)
portfolio_returns_3x3$IV1_M_LS <- portfolio_returns_3x3$IV1_M3 - portfolio_returns_3x3$IV1_M1
portfolio_returns_3x3$IV2_M_LS <- portfolio_returns_3x3$IV2_M3 - portfolio_returns_3x3$IV2_M1
portfolio_returns_3x3$IV3_M_LS <- portfolio_returns_3x3$IV3_M3 - portfolio_returns_3x3$IV3_M1

portfolio_returns_3x3$M1_IV_LS <- portfolio_returns_3x3$IV3_M1 - portfolio_returns_3x3$IV1_M1
portfolio_returns_3x3$M2_IV_LS <- portfolio_returns_3x3$IV3_M2 - portfolio_returns_3x3$IV1_M2
portfolio_returns_3x3$M3_IV_LS <- portfolio_returns_3x3$IV3_M3 - portfolio_returns_3x3$IV1_M3

portfolio_returns_3x3$LS_LS <- portfolio_returns_3x3$IV3_M_LS - portfolio_returns_3x3$IV1_M_LS

# Extend port_cols to include the long-short legs and LS-LS
port_cols_all <- c("IV1_M1", "IV1_M2", "IV1_M3",
                   "IV2_M1", "IV2_M2", "IV2_M3",
                   "IV3_M1", "IV3_M2", "IV3_M3",
                   "IV1_M_LS", "IV2_M_LS", "IV3_M_LS",
                   "M1_IV_LS", "M2_IV_LS", "M3_IV_LS",
                   "LS_LS")

results_controlled_for_pc1 <- data.frame(
  portfolio = port_cols_all,
  alpha        = sapply(port_cols_all, function(port) {
    fit <- lm(portfolio_returns_3x3[[port]] ~ portfolio_returns_3x3$pc1)
    coef(fit)["(Intercept)"]
  }),
  t_stat_alpha = sapply(port_cols_all, function(port) {
    fit <- lm(portfolio_returns_3x3[[port]] ~ portfolio_returns_3x3$pc1)
    summary(fit)$coefficients["(Intercept)", "t value"]
  }),
  p_value_alpha = sapply(port_cols_all, function(port) {
    fit <- lm(portfolio_returns_3x3[[port]] ~ portfolio_returns_3x3$pc1)
    summary(fit)$coefficients["(Intercept)", "Pr(>|t|)"]
  }),
  beta         = sapply(port_cols_all, function(port) {
    fit <- lm(portfolio_returns_3x3[[port]] ~ portfolio_returns_3x3$pc1)
    coef(fit)["portfolio_returns_3x3$pc1"]
  }),
  t_stat_beta  = sapply(port_cols_all, function(port) {
    fit <- lm(portfolio_returns_3x3[[port]] ~ portfolio_returns_3x3$pc1)
    summary(fit)$coefficients["portfolio_returns_3x3$pc1", "t value"]
  })
)

# Add significance stars for alpha: *** p<0.01, ** p<0.05, * p<0.10
results_controlled_for_pc1$sig_alpha <- cut(
  results_controlled_for_pc1$p_value_alpha,
  breaks = c(-Inf, 0.01, 0.05, 0.10, Inf),
  labels = c("***", "**", "*", "")
)

# Round numeric columns to 3 digits
numeric_cols <- c("alpha", "t_stat_alpha", "p_value_alpha", "beta", "t_stat_beta")
results_controlled_for_pc1[numeric_cols] <- round(results_controlled_for_pc1[numeric_cols], 3)

# Optional: combined display column showing alpha with stars, e.g. "0.325***"
results_controlled_for_pc1$alpha_display <- paste0(
  format(results_controlled_for_pc1$alpha, nsmall = 3),
  results_controlled_for_pc1$sig_alpha
)

print(results_controlled_for_pc1)