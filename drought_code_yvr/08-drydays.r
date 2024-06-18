# Indicator of days with >=1 mm of rain,
# then number of consecutive days < 1mm of rain
# YVR has no precipitation data for first two months of 1937: 59 days

load("yvr-temp-precip.RData")

#======================================================================

# impute missing daily max and min

imax = missing_maxtem
icheckmax = sort(c(imax,imax-1,imax+1))
icheckmax = unique(icheckmax)
icheckmax

imin = missing_mintem
icheckmin = sort(c(imin,imin-1,imin+1))
icheckmin = unique(icheckmin)
#icheckmin

icheck = c(icheckmin, icheckmax)
icheck = unique(icheck)
#length(icheck)
#[1] 143

# impute temperatures

maxtmp = yvr$maxtemp
# Missing values be imputed with two neighbors.
maxtemp = maxtmp
# Below works if first and last values are not missing
for(i in imax)
{ i1 = i-1
  while(is.na(maxtmp[i1])) { i1 = i1-1 }
  max1 = maxtmp[i1]
  i2 = i+1
  while(is.na(maxtmp[i2])) { i2 = i2+1 }
  max2 = maxtmp[i2]
  maxtemp[i]=(max1+max2)/2
  #cat(maxtemp[(i-1):(i+1)],"\n")
}

mintmp = yvr$mintemp
# Missing values be imputed with two neighbors.
mintemp = mintmp
# Below works if first and last values are not missing
for(i in imin)
{ i1 = i-1
  while(is.na(mintmp[i1])) { i1 = i1-1 }
  min1 = mintmp[i1]
  i2 = i+1
  while(is.na(mintmp[i2])) { i2 = i2+1 }
  min2 = mintmp[i2]
  mintemp[i]=(min1+min2)/2
  #cat(mintemp[(i-1):(i+1)],"\n")
}

#======================================================================

iomit = 1:59
yvr2 = yvr[-iomit,]
# first look at distribution of precipitation over days
precip = yvr2$totprec

summary(precip)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.000   0.000   0.000   3.098   3.400  91.600      63

precip[is.na(precip)] = 0
#summary(precip)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.000   0.000   0.000   3.092   3.400  91.600 

#length(precip)
# 31819

# percent ==0
#sum(precip==0)
# 17564   17564/31819 = 0.552

# percent <=1 mm (check that unit is mm) (true for German data)

#sum(precip<1)
# 20156    20156/31819  = 0.633

#mintemp=mintemp[-iomit]
#maxtemp=maxtemp[-iomit]

# days>= 1mm

i1ormore = (precip>=1)

n=length(precip)  # 31819

# go backwards to get number of consecutive dry days
#nn = 20 
#nn = 19 
nn = n
notdry = nn+1
consec = rep(0,nn)
if(i1ormore[nn]) { consec[nn] = 0; notdry = nn } else { consec[nn] = 1; }
for(i in (nn-1):1)
{ if(i1ormore[i]) { consec[i] = 0; notdry = i }
  else { consec[i] = consec[i+1]+1 }
}

# cbind(i1ormore[1:nn],consec)

yvr2$consec = consec
yvr2$yrmon = 100*yvr2$year+yvr2$month

tail(yvr2)
#      year month   yyyymmdd mintemp meantemp maxtemp totprec consec  yrmon
#31873 2024     5 2024-05-06     8.0     11.6    15.1     0.2      6 202405
#31874 2024     5 2024-05-07     8.1     13.2    18.3     0.0      5 202405
#31875 2024     5 2024-05-08     6.2     11.0    15.7     0.0      4 202405
#31876 2024     5 2024-05-09     5.7     11.9    18.1     0.0      3 202405
#31877 2024     5 2024-05-10     7.8     14.5    21.1     0.0      2 202405
#31878 2024     5 2024-05-11     9.7     15.1    20.5     0.0      1 202405

library(tibble)
library(dplyr)
library(magrittr)

byMonth2 = as_tibble(yvr2) %>% 
   dplyr::group_by(yrmon) %>%
   dplyr::summarise(max_consec=max(consec))

head(byMonth2)
#   <dbl>      <dbl>
#1 193703          3
#2 193704          6
#3 193705         21
#4 193706         47
#5 193707         40
#6 193708         26

tail(byMonth2)
#   yrmon max_consec
#   <dbl>      <dbl>
#1 202312          3
#2 202401          5
#3 202402          8
#4 202403          7
#5 202404          8
#6 202405          6

byMonth2[byMonth2$yrmon>=202101 & byMonth2$yrmon<=202112,]
#    yrmon max_consec
#    <dbl>      <dbl>
# 1 202101          7
# 2 202102          6
# 3 202103          6
# 4 202104         16
# 5 202105          8
# 6 202106         52
# 7 202107         37
# 8 202108         12
# 9 202109          6
#10 202110          4
#11 202111          5
#12 202112          6

write.csv(file="yvr-max-consec-drydays-bymonth.csv", byMonth2, row.names=F)

