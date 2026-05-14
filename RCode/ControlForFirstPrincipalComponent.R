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

port_cols <- c("IV1_M1", "IV1_M2", "IV1_M3",
               "IV2_M1", "IV2_M2", "IV2_M3",
               "IV3_M1", "IV3_M2", "IV3_M3")

results_controlled_for_pc1 <- data.frame(
  portfolio = port_cols,
  alpha     = sapply(port_cols, function(port) {
    fit <- lm(portfolio_returns_3x3[[port]] ~ portfolio_returns_3x3$pc1)
    coef(fit)["(Intercept)"]
  }),
  t_stat_alpha = sapply(port_cols, function(port) {
    fit <- lm(portfolio_returns_3x3[[port]] ~ portfolio_returns_3x3$pc1)
    summary(fit)$coefficients["(Intercept)", "t value"]
  }),
  beta      = sapply(port_cols, function(port) {
    fit <- lm(portfolio_returns_3x3[[port]] ~ portfolio_returns_3x3$pc1)
    coef(fit)["portfolio_returns_3x3$pc1"]
  }),
  t_stat_beta = sapply(port_cols, function(port) {
    fit <- lm(portfolio_returns_3x3[[port]] ~ portfolio_returns_3x3$pc1)
    summary(fit)$coefficients["portfolio_returns_3x3$pc1", "t value"]
  })
)

#See alpha in results


#clean up
keep <- c(keep, "results_controlled_for_pc1")
rm(list = setdiff(ls(), keep))