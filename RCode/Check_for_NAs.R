
load("Data/daily_factors.RData")
load("Data/monthly_factors.RData")

na_count <- monthly_factors %>%
  mutate(n_nulls = rowSums(is.na(select(., -date))))

plot.ts(na_count$n_nulls)

na_count$n_nulls[na_count$date == as.Date("1991-05-31")]
which(na_count$date == as.Date("1991-05-31"))

na_count <- daily_factors %>%
  mutate(n_nulls = rowSums(is.na(select(., -date))))

plot.ts(na_count$n_nulls)

na_count$n_nulls[na_count$date == as.Date("1991-05-01")]
which(na_count$date == as.Date("1991-05-01"))

#start date is 1991-05-31 for monthly
#start date is 1991-05-01 for daily


na_counts <- colSums(is.na(daily_factors))
na_counts_filtered <- na_counts[na_counts > 400]
barplot(
  na_counts_filtered,
  las = 2,        # rotate labels so they are readable
  col = "steelblue",
  main = "Factors with more than 400 NA values",
  ylab = "Number of NA values"
)


#Daily
# Count NA's after first non-NA for each factor
na_after_first <- sapply(daily_factors[, -1], function(x) {
  first_non_na <- which(!is.na(x))[1]        # first non-NA index
  sum(is.na(x[first_non_na:length(x)]))      # count NAs after that
})


# Convert to data frame
na_df <- data.frame(
  factor = names(na_after_first),
  n_NA_after_first = as.numeric(na_after_first)
)

# Sort descending by number of NAs
na_df <- na_df[order(na_df$n_NA_after_first, decreasing = TRUE), ]

# View the sorted data frame
print(na_df)

plot.ts(daily_factors$eqpo_me)


#Monthly
# Count NA's after first non-NA for each factor
na_after_first <- sapply(monthly_factors[, -1], function(x) {
  first_non_na <- which(!is.na(x))[1]        # first non-NA index
  sum(is.na(x[first_non_na:length(x)]))      # count NAs after that
})


# Convert to data frame
na_df <- data.frame(
  factor = names(na_after_first),
  n_NA_after_first = as.numeric(na_after_first)
)

# Sort descending by number of NAs
na_df <- na_df[order(na_df$n_NA_after_first, decreasing = TRUE), ]

# View the sorted data frame
print(na_df)

plot.ts(monthly_factors$eqpo_me)
