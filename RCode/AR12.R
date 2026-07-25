# ---- 1. Prepare data ----
factors <- monthly_factors %>%
  select(-date, -mkt)

# ---- 2. Function to get AR(12) coef + SE ----
get_ar12_info <- function(x) {
  x <- na.omit(x)
  
  if (length(x) < 20) {  # need enough obs for a 12-lag model to be estimable
    return(c(ar12 = NA, se = NA))
  }
  
  model <- arima(x, order = c(12, 0, 0))
  
  ar12 <- coef(model)["ar12"]
  se   <- sqrt(diag(model$var.coef))["ar12"]
  
  c(ar12 = ar12, se = se)
}

# ---- 3. Apply to all columns ----
ar12_results <- map(factors, get_ar12_info)
results_df <- do.call(rbind, ar12_results) %>% as.data.frame()
results_df$Factor <- rownames(results_df)
rownames(results_df) <- NULL

# ---- 4. Compute 95% confidence intervals ----
results_df <- results_df %>%
  mutate(
    lower = ar12.ar12 - 1.96 * se.ar12,
    upper = ar12.ar12 + 1.96 * se.ar12
  )

# ---- 5. Plot (sorted low → high) ----
plot <- ggplot(results_df, aes(x = reorder(Factor, ar12.ar12), y = ar12.ar12)) +
  geom_bar(stat = "identity", fill = "grey") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
  theme_minimal() +
  labs(
    title = "AR(12) Coefficients with 95% Confidence Intervals",
    x = "Factor",
    y = "AR(12) Slope"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot)

#clean up
rm(list = setdiff(ls(), keep))