# drop date and mkt
factors <- monthly_factors %>%
  select(-date, -mkt)

expected_returns <- data.frame(
  Factor = names(factors),
  Mean = sapply(factors, function(x) mean(x, na.rm = TRUE))
)

# optional: sort
expected_returns <- expected_returns %>%
  arrange(desc(Mean))

sharpe_ratios <- data.frame(
  Factor = names(factors),
  Sharpe = sapply(factors, function(x) {
    m <- mean(x, na.rm = TRUE)
    s <- sd(x, na.rm = TRUE)
    m / s
  })
)

# optional: sort
sharpe_ratios <- sharpe_ratios %>%
  arrange(desc(Sharpe))

#sharpe_ratios$Sharpe_annualized <- sharpe_ratios$Sharpe * sqrt(12)

plot1 <- ggplot(expected_returns, aes(x = reorder(Factor, Mean), y = Mean)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(
    title = "Expected Monthly Returns of Factors",
    x = "Factor",
    y = "Mean Return"
  ) +
  coord_flip()

plot2 <- ggplot(sharpe_ratios, aes(x = reorder(Factor, Sharpe), y = Sharpe)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  theme_minimal() +
  labs(
    title = "Sharpe Ratios of Factors (Monthly)",
    x = "Factor",
    y = "Sharpe Ratio"
  ) +
  coord_flip()

print(plot1)
print(plot2)

#clean up
rm(list = setdiff(ls(), keep))
