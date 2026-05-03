################################################################################
############################# Monthly data #####################################
################################################################################
setwd("C:/Users/Ander/Desktop/Praktik/Data")

#Import data
data <- read.table(file = "[dnk]_[all_factors]_[monthly]_[vw_cap].csv",
                          sep = ",", dec = ".",
                          header = TRUE,
                          na.strings = "null")
#Market return
mkt <- read.table(file = "[dnk]_[mkt]_[monthly]_[vw_cap].csv",
                   sep = ",", dec = ".",
                   header = TRUE,
                   na.strings = "null")
data <- rbind(data, mkt)
rm(mkt)


#Count number of different names
names <- unique(data[,2])
min(data$date)
max(data$date)
sorted_dates <- sort(unique(data[,8]), decreasing = FALSE)

empty_date_dataframe <- data.frame(date = sorted_dates)
# Start with the empty date frame
loc_df <- empty_date_dataframe

# Loop over all factor names
for (nm in names) {
  sub_df <- data[data$name == nm, c("date", "ret")]
  
  # Rename ret column to factor name
  colnames(sub_df)[colnames(sub_df) == "ret"] <- nm
  
  # Merge into main dataframe
  loc_df <- merge(loc_df, sub_df, by = "date", all = TRUE)
}


monthly_factors <- loc_df

#Remove data before 1991-05-01 due to missing data (64 obs)
monthly_factors <- monthly_factors[monthly_factors$date >= as.Date("1991-05-31"), ]
row.names(monthly_factors) <- NULL

#Remove factors with too many NA's
monthly_factors <- monthly_factors[, setdiff(names(monthly_factors),
                                             c("iskew_ff3_21d", "ivol_ff3_21d", "resff3_6_1", "resff3_12_1"))]

######################## Handle NA s###########################################
#6 month rolling mean

#Extract factor names
factors <- colnames(monthly_factors)
factors <- setdiff(factors, c("date")) #Remove Date

na_padding <- monthly_factors[rep(1,5), ]
na_padding[,] <- NA

# prepend it
monthly_factors <- rbind(na_padding, monthly_factors)
rownames(monthly_factors) <- NULL

for (factor in factors) {
  
  start_index <- which(!is.na(monthly_factors[[factor]]))[1]
  end_index <- tail(which(!is.na(monthly_factors[[factor]])), 1)
  
  for (i in (start_index):(end_index-1)) {
    
    rolling_mean <- mean(monthly_factors[[factor]][(i-5):i], na.rm = TRUE)
    
    if (is.na(monthly_factors[[factor]][i+1])) {
      monthly_factors[[factor]][i+1] <- rolling_mean
    }
  }
}

monthly_factors <- monthly_factors[-c(1:5),]
rownames(monthly_factors) <- NULL

# Save your data_list into an .RData file
save(monthly_factors, file = "monthly_factors.RData")

################################################################################
############################# Daily data #######################################
################################################################################

rm(list = ls())

#Import data
data <- read.table(file = "[dnk]_[all_factors]_[daily]_[vw_cap].csv",
                   sep = ",", dec = ".",
                   header = TRUE,
                   na.strings = "null")
#Market return
mkt <- read.table(file = "[dnk]_[mkt]_[daily]_[vw_cap].csv",
                  sep = ",", dec = ".",
                  header = TRUE,
                  na.strings = "null")
data <- rbind(data, mkt)
rm(mkt)


#Count number of different names
names <- unique(data[,2])
min(data$date)
max(data$date)
sorted_dates <- sort(unique(data[,8]), decreasing = FALSE)

empty_date_dataframe <- data.frame(date = sorted_dates)
# Start with the empty date frame
loc_df <- empty_date_dataframe

# Loop over all factor names
for (nm in names) {
  sub_df <- data[data$name == nm, c("date", "ret")]
  
  # Rename ret column to factor name
  colnames(sub_df)[colnames(sub_df) == "ret"] <- nm
  
  # Merge into main dataframe
  loc_df <- merge(loc_df, sub_df, by = "date", all = TRUE)
}


daily_factors <- loc_df

#Remove data before 1991-05-01 due to missing data
daily_factors <- daily_factors[daily_factors$date >= as.Date("1991-05-01"), ]
row.names(daily_factors) <- NULL

#Remove factors with too many NA's
daily_factors <- daily_factors[, setdiff(names(daily_factors),
                                             c("iskew_ff3_21d", "ivol_ff3_21d", "resff3_6_1", "resff3_12_1"))]

######################## Handle NA s###########################################
#20 day rolling mean

#Extract factor names
factors <- colnames(daily_factors)
factors <- setdiff(factors, c("date")) #Remove Date

na_padding <- daily_factors[rep(1,19), ]
na_padding[,] <- NA

# prepend it
daily_factors <- rbind(na_padding, daily_factors)
rownames(daily_factors) <- NULL

for (factor in factors) {
  
  start_index <- which(!is.na(daily_factors[[factor]]))[1]
  end_index <- tail(which(!is.na(daily_factors[[factor]])), 1)
  
  for (i in (start_index):(end_index-1)) {
    
    rolling_mean <- mean(daily_factors[[factor]][(i-19):i], na.rm = TRUE)
    
    if (is.na(daily_factors[[factor]][i+1])) {
      daily_factors[[factor]][i+1] <- rolling_mean
    }
  }
}

daily_factors <- daily_factors[-c(1:19),]
rownames(daily_factors) <- NULL

# Save your data_list into an .RData file
save(daily_factors, file = "daily_factors.RData")


