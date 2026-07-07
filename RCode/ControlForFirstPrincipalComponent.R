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

# Build long-short and LS-LS return series
portfolio_returns_3x3$IV1_M_LS <- portfolio_returns_3x3$IV1_M3 - portfolio_returns_3x3$IV1_M1
portfolio_returns_3x3$IV2_M_LS <- portfolio_returns_3x3$IV2_M3 - portfolio_returns_3x3$IV2_M1
portfolio_returns_3x3$IV3_M_LS <- portfolio_returns_3x3$IV3_M3 - portfolio_returns_3x3$IV3_M1
portfolio_returns_3x3$LS_LS    <- portfolio_returns_3x3$IV3_M_LS - portfolio_returns_3x3$IV1_M_LS

# Only the 4 key long-short portfolios
port_cols_ls <- c("IV1_M_LS", "IV2_M_LS", "IV3_M_LS", "LS_LS")

# Build a long-format table: one row per (portfolio, variable) combo, FF3-style
pc1_reg_table <- do.call(rbind, lapply(port_cols_ls, function(port) {
  
  fit <- lm(portfolio_returns_3x3[[port]] ~ portfolio_returns_3x3$pc1)
  
  nw <- coeftest(fit, vcov = NeweyWest(fit, lag = 6, prewhite = FALSE))
  
  data.frame(
    Portfolio   = port,
    Variable    = rownames(nw),
    Coefficient = round(nw[, "Estimate"], 3),
    Std.Error   = round(nw[, "Std. Error"], 3),
    t_stat      = round(nw[, "t value"], 3),
    p_value     = round(nw[, "Pr(>|t|)"], 3),
    row.names   = NULL
  )
}))

print(pc1_reg_table)