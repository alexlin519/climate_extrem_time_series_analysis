
# extract variables and rename
# maxtemp, mintemp, totprec


# merging YVR1108447-daily1937to1964.csv YVR1108447-daily1964to1991.csv
# YVR1108447-daily1991to2013.csv YVR1108447-daily2013to2024.csv

#wc -l YVR*csv
#  10001 YVR1108447-daily1937to1964.csv
#  10001 YVR1108447-daily1964to1991.csv
#   7900 YVR1108447-daily1991to2013.csv
#   3980 YVR1108447-daily2013to2024.csv
#  31882 total
# Last file has different header format, 
# Use skip lines and read the merged file twice

library(dplyr)  # needed for filter, mutate, rename, select
library(magrittr)  # forward pipe operator %>%
library(tibble)  


yvra = read.csv("YVR1108447-1937to2024.csv",header=T,nrow=27899)
yvrb = read.csv("YVR1108447-1937to2024.csv",header=T,skip=27902,nrow=3979)

yvr2013 = yvra %>%
  rename(year=LOCAL_YEAR, month=LOCAL_MONTH, date=LOCAL_DATE, 
   mintemp=MIN_TEMPERATURE, meantemp=MEAN_TEMPERATURE, maxtemp=MAX_TEMPERATURE,
   totprec=TOTAL_PRECIPITATION) %>%
  mutate(yyyymmdd=as.Date(date)) %>%
  select(year,month,yyyymmdd,mintemp,meantemp,maxtemp,totprec)

yvr2024 = yvrb %>%
  rename(year=LOCAL_YEAR, month=LOCAL_MONTH, date=LOCAL_DATE, 
   mintemp=MIN_TEMPERATURE, meantemp=MEAN_TEMPERATURE, maxtemp=MAX_TEMPERATURE,
   totprec=TOTAL_PRECIPITATION) %>%
  mutate(yyyymmdd=as.Date(date)) %>%
  select(year,month,yyyymmdd,mintemp,meantemp,maxtemp,totprec)

tail(yvr2013)
tail(yvr2024)

yvr = rbind(yvr2013,yvr2024)

class(yvr)
#[1] "data.frame"

dim(yvr)
# [1] 31878     7

head(yvr)
#  year month   yyyymmdd mintemp meantemp maxtemp totprec
#1 1937     1 1937-01-01    -6.1     -1.1     3.9      NA
#2 1937     1 1937-01-02    -5.6     -2.8     0.0      NA
#3 1937     1 1937-01-03    -1.7      2.2     6.1      NA
#4 1937     1 1937-01-04    -4.4      0.3     5.0      NA
#5 1937     1 1937-01-05   -11.1     -6.1    -1.1      NA
#6 1937     1 1937-01-06    -8.3     -5.6    -2.8      NA

tail(yvr)
#      year month   yyyymmdd mintemp meantemp maxtemp totprec
#31873 2024     5 2024-05-06     8.0     11.6    15.1     0.2
#31874 2024     5 2024-05-07     8.1     13.2    18.3     0.0
#31875 2024     5 2024-05-08     6.2     11.0    15.7     0.0
#31876 2024     5 2024-05-09     5.7     11.9    18.1     0.0
#31877 2024     5 2024-05-10     7.8     14.5    21.1     0.0
#31878 2024     5 2024-05-11     9.7     15.1    20.5     0.0

# last file starts from 2013-06-13

yvr[27895:27905,]
#      year month   yyyymmdd mintemp meantemp maxtemp totprec
#27895 2013     6 2013-06-08    10.4     14.8    19.2     0.0
#27896 2013     6 2013-06-09    11.5     14.9    18.2     0.6
#27897 2013     6 2013-06-10     9.2     14.1    19.0     0.0
#27898 2013     6 2013-06-11    11.9     15.4    18.9     0.0
#27899 2013     6 2013-06-12    10.8     14.6    18.4     2.0
#27900 2013     6 2013-06-13     8.9     14.1    19.3     0.0
#27901 2013     6 2013-06-14    11.5     14.9    18.3     0.4
#27902 2013     6 2013-06-15     7.8     13.9    19.9     0.0
#27903 2013     6 2013-06-16    13.2     17.8    22.4     0.0
#27904 2013     6 2013-06-17    14.1     18.5    22.8     0.0
#27905 2013     6 2013-06-18    13.0     16.5    19.9     0.4

# check definition is meantemp

sub = yvr[27895:27905,]
print((sub$mintemp+sub$maxtemp)/2)
# [1] 14.80 14.85 14.10 15.40 14.60 14.10 14.90 13.85 17.80 18.45 16.45
# rounded up


# missing data information

imissbycol = apply(is.na(yvr),2,sum)
print(imissbycol)
#    year    month yyyymmdd  mintemp meantemp  maxtemp  totprec 
#       0        0        0       41       54       50      122 

mintem = yvr$mintemp
maxtem = yvr$maxtemp
meantem = yvr$meantemp
prec = yvr$totprec

# Check pattern of missing values, omit early years if too many missing
# Can impute missing variable if few of them 
missing_mintem = which(is.na(mintem))
missing_maxtem = which(is.na(maxtem))
missing_meantem = which(is.na(meantem))
missing_prec = which(is.na(prec))

print(missing_mintem) 
# [1]   135   321  2013 21496 21497 21498 21499 21669 21749 21846 23819 29100
#[13] 29785 30351 30359 30365 30783 30817 30822 30973 31142 31211 31238 31239
#[25] 31259 31260 31278 31300 31470 31473 31521 31523 31700 31705 31755 31763
#[37] 31806 31811 31816 31852 31866
 
print(missing_maxtem) 
# [1]   135   446   447   448   449 21496 21497 21498 21499 21669 21749 21846
#[13] 23819 28090 28243 28625 28873 29013 29303 29488 29501 29502 30351 30359
#[25] 30365 30783 30817 30822 30973 31142 31211 31238 31239 31259 31260 31278
#[37] 31300 31470 31473 31521 31523 31700 31705 31755 31763 31806 31811 31816
#[49] 31852 31866

print(missing_meantem)
# [1]   135   321   446   447   448   449  2013 21496 21497 21498 21499 21669
#[13] 21749 21846 23819 28090 28243 28625 28873 29013 29100 29303 29488 29501
#[25] 29502 29785 30351 30359 30365 30783 30817 30822 30973 31142 31211 31238
#[37] 31239 31259 31260 31278 31300 31470 31473 31521 31523 31700 31705 31755
#[49] 31763 31806 31811 31816 31852 31866

print(missing_prec) 
#  [1]     1     2     3     4     5     6     7     8     9    10    11    12
# [13]    13    14    15    16    17    18    19    20    21    22    23    24
# [25]    25    26    27    28    29    30    31    32    33    34    35    36
# [37]    37    38    39    40    41    42    43    44    45    46    47    48
# [49]    49    50    51    52    53    54    55    56    57    58    59   335
# [61]   336   337   338   339   340   341   342   343   344   345   346   347
# [73]   348   349   350   351   352   353   354   355   356   357   358   359
# [85]   360   361   362   363   364   365 21496 21497 21498 21499 27913 28090
# [97] 28243 28301 28304 28310 28333 28425 28554 28584 28625 28691 28850 28873
#[109] 28967 28982 29013 29303 29488 29501 29742 31015 31071 31557 31594 31605
#[121] 31606 31811

imissbyrow = apply(is.na(yvr),1,sum)
print(table(imissbyrow))
#imissbyrow
#    0     1     2     3     4 
#31715   109     9    40     5 

save(file="yvr-temp-precip.RData", yvr, imissbycol, imissbyrow,
  missing_mintem, missing_maxtem, missing_meantem, missing_prec)

ii = missing_prec[91:122]
icheck = sort(c(ii,ii-1,ii+1))
icheck = unique(icheck)  # 89 cases

write.csv(file="yvr-missing-precip.csv",yvr[icheck,],col.names=T)
