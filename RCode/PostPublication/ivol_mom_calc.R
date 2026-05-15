load("Data/daily_factors.RData")
load("Data/monthly_factors.RData")

#create a lagged mkt factor
daily_factors$mkt_lagged <- lag(daily_factors$mkt)

#Extract factor names
factors <- colnames(monthly_factors)
factors <- setdiff(factors, c("date", "mkt", "mkt_lagged")) #Remove Date, mkt and mkt_lagged

#compute market factor return (cross sectional avg of the factors for each t)
#Not for the date and mkt column
daily_factors$market <- apply(
  daily_factors[, !(names(daily_factors) %in% c("date", "mkt", "mkt_lagged"))],
  1,
  function(x) mean(x, na.rm = TRUE)
)


#Lagged market + remove row 1 due to NA in lagged market and reset row index
daily_factors$market_lagged <- lag(daily_factors$market)
daily_factors <- daily_factors[-1,]
row.names(daily_factors) <- NULL

#Create empty dataframe for ivol
ivol_df <- daily_factors
ivol_df[,-1] <- NA


for (factor in factors) {

#data is from first non NA of the factor with 12 months of data (assuming 252 trading days a year)
start_index <- which(!is.na(daily_factors[[factor]]))[1]
length <- length((start_index + 251):nrow(daily_factors))
cat("Factor:", factor, "\n")

for (i in 1:length) {

df <- daily_factors[(start_index + i - 1):(start_index + 251 + i - 1),c("date",factor,"mkt", "mkt_lagged")]

fit <- lm(as.formula(paste(factor, "~ mkt + mkt_lagged")), data = df)

ivol_df[(start_index + 251 + i - 1),factor] <- sqrt(var(fit[["residuals"]]))

  }
}

save(ivol_df, file = "daily_ivol.RData")

#load("Data/daily_ivol.RData")

####Convert to monthly data, (end_of_month)

monthly_ivol <- ivol_df %>%
  mutate(date = as.Date(date),
         year_month = format(date, "%Y-%m")) %>%
  group_by(year_month) %>%
  slice_max(order_by = date, n = 1) %>%   # last day of each month
  ungroup() %>%
  select(-year_month)

monthly_ivol[["date"]] <- monthly_factors[["date"]]

# Subset dataframe
monthly_ivol <- monthly_ivol[, setdiff(names(monthly_ivol), c("market", "market_lagged", "mkt", "mkt_lagged"))]

monthly_ivol <- monthly_ivol[-(1:12),]
row.names(monthly_ivol) <- NULL


save(monthly_ivol, file = "Data/monthly_ivol.RData")

