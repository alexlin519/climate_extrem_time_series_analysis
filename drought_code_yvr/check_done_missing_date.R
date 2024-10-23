
load("../output/Rdata/YVR_imputed.RData")
#YVR_imputed

load("../output/Rdata/YVR-temp-precip.RData")
#YVR_df 
df_date<- YVR_df
# non exist fun:

# Ensure the date column is in Date format
df_date$LOCAL_DATE <- as.Date(df_date$yyyymmdd, format="%Y-%m-%d")
# Remove NA values if any
df_date <- df_date[!is.na(df_date$LOCAL_DATE), ]
# Create a complete sequence of dates
complete_dates <- data.frame(LOCAL_DATE = seq(min(df_date$LOCAL_DATE), max(df_date$LOCAL_DATE), by="day"))
missing_dates <- setdiff(seq(min(df_date$LOCAL_DATE), max(df_date$LOCAL_DATE), by="day"), 
                         df_date$LOCAL_DATE)

# Check if the dataframe is already complete
if (nrow(df_date) == nrow(complete_dates)) {
  # If complete, return NA and print a message
  print("The dataframe is already complete. No missing dates.")
}
missing_dates
