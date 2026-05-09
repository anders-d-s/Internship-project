
# ---- 1. Prepare data ----
factors <- monthly_factors %>%
  select(-date, -mkt)

# ---- 2. Function to get AR(1) coef + SE ----
get_ar1_info <- function(x) {
  x <- na.omit(x)
  
  if (length(x) < 5) {
    return(c(ar1 = NA, se = NA))
  }
  
  model <- arima(x, order = c(1, 0, 0))
  
  ar1 <- coef(model)["ar1"]
  se  <- sqrt(diag(model$var.coef))["ar1"]
  
  c(ar1 = ar1, se = se)
}

# ---- 3. Apply to all columns ----
ar1_results <- map(factors, get_ar1_info)

results_df <- do.call(rbind, ar1_results) %>% as.data.frame()

results_df$Factor <- rownames(results_df)
rownames(results_df) <- NULL

# ---- 4. Compute 95% confidence intervals ----
results_df <- results_df %>%
  mutate(
    lower = ar1.ar1 - 1.96 * se.ar1,
    upper = ar1.ar1 + 1.96 * se.ar1
  )

# ---- 5. Plot (sorted low → high) ----
ggplot(results_df, aes(x = reorder(Factor, ar1.ar1), y = ar1.ar1)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  theme_minimal() +
  labs(
    title = "AR(1) Coefficients with 95% Confidence Intervals",
    x = "Factor",
    y = "AR(1) Slope"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rm(list = setdiff(ls(), c("portfolio_returns_3x3","ivol_groups","mom_groups","monthly_factors")))
