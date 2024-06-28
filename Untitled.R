# Sample data
df_wrangling_lab2 <- data.frame(MAX_TEMPERATURE = c(1,4,2, 2, NA, 4,5,8,NA, NA, 6,7,9, NA,11))

maxtmp = df_wrangling_lab2$MAX_TEMPERATURE

missing_maxtem <- which(is.na(maxtmp))

if (length(missing_maxtem) > 0) {
  for (i in missing_maxtem) {
    i1 = i - 1
    while ( is.na(maxtmp[i1])) { i1 = i1 - 1 }
    # Ensure i1 is within bounds
    #if (i1 <= 0) next
    
    mx1 = maxtmp[i1]
    
    i2 = i + 1
    while ( is.na(maxtmp[i2])) { i2 = i2 + 1 }
    # Ensure i2 is within bounds
    #if (i2 > length(maxtmp)) next
    
    mx2 = maxtmp[i2]
    
    # Ensure both mx1 and mx2 are valid before averaging
    if (!is.na(mx1) && !is.na(mx2)) {
      maxtmp[i] <- (mx1 + mx2) / 2
    }
  }
  }

#
# Update the original column with the modified values
df_wrangling_lab2$MAX_TEMPERATURE <- maxtmp


print(df_wrangling_lab2)


