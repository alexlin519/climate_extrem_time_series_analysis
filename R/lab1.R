# Call the function with the path to your CSV file
temp_precip_path<- process_data("../output/Kelowna_raw_filtered_columns.csv","Kelowna")

percentiles90th_path<- process_base_years(temp_precip_path,"Kelowna")

imputed_temp_data <- impute_temps(temp_precip_path)
#save the imputed data
write.csv(imputed_temp_data, file = "../output/202407_Kelowna_imputed_data.csv")
#--------------------------------------------------------------
#assume that the imputed_temp_data is already loaded

#df_dropna <- imputed_temp_data[!is.na(imputed_temp_data$totprec), ]
df_dropna <- imputed_temp_data

precip <- df_dropna$totprec
#precip[is.na(precip)] <- 0
i1ormore <- (precip >= 1) # dry days is lower than 1 mm
n <- length(precip)
nn <- n
notdry <- nn+1
consec <- rep(0, nn)

#old version
# if(i1ormore[nn]) { consec[nn] <- 0; notdry <- nn } else { consec[nn] <- 1 }
# for(i in (nn-1):1) {
#   if(i1ormore[i]) { consec[i] <- 0; notdry <- i }
#   else { consec[i] <- consec[i+1]+1 }
# }

# my new version of the code in July 20th, 
#break the consec count if the value is NA
if (is.na(i1ormore[nn])) {
  consec[nn] <- 0
} else if (i1ormore[nn]) {
  consec[nn] <- 0
  notdry <- nn
} else {
  consec[nn] <- 1
}

# Loop through the rest of the elements
for (i in (nn - 1):1) {
  if (is.na(i1ormore[i])) {
    consec[i] <- 0
  } else if (i1ormore[i]) {
    consec[i] <- 0
    notdry <- i
  } else {
    consec[i] <- consec[i + 1] + 1
  }
}
df_dropna$consec <- consec
df_dropna$yrmon <- 100*df_dropna$year + df_dropna$month

write.csv(df_dropna, file = "../output/202407_Kelowna_consec_data.csv")
#--------------------------------------------------------------

#2015-06-24 to 2016-03-31
#filter to have this outlier consec dry days date range
df_spesific <- df_dropna[df_dropna$yrmon %in% c(201506,201507,201508,201509,201510,201511,201512,201601,201602,201603),]

summary(df_spesific$totprec)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0000  0.0000  0.0000  0.1512  0.0000 17.8000      29 

#unique values of the total precipitation and the number of occurences
table(df_spesific$totprec)
# 0  0.3  0.5  1.8  3.5  3.8    4  6.1 17.8 
# 245    4    1    1    1    1    1    1    1

