# Average difference with 90th by month, 90th based on 1961-1990
# Combine Feb 29 with Feb 28

load("yvr-temp-precip.RData")
load("yvrstat.RData")  # yvr_q90 by day


imx = missing_maxtem
#icheckmx = sort(c(imx,imx-1,imx+1))
#icheckmx = unique(icheckmx)
#icheckmx
# No more than 4 missing values in a row

maxtmp = yvr$maxtemp
# Missing values be imputed with two neighbors.
maxtemp = maxtmp
# Below works if first and last values are not missing
for(i in imx)
{ i1 = i-1
  while(is.na(maxtmp[i1])) { i1 = i1-1 }
  mx1 = maxtmp[i1]
  i2 = i+1
  while(is.na(maxtmp[i2])) { i2 = i2+1 }
  mx2 = maxtmp[i2]
  maxtemp[i]=(mx1+mx2)/2
  #cat(maxtemp[(i-1):(i+1)],"\n")
}

# Find cases of exceeding 90th percentile of the dayofyear
library(timeDate)
dayofyear = dayOfYear(timeDate(yvr$yyyymmdd))
yr = yvr$year
dayofyear2 = dayofyear
ileap = (yr%%4==0 & dayofyear==60)
ii = (yr%%4==0 & dayofyear>60)
# handling leap days in every 4th year (including year 2000)
dayofyear2[ileap] = 59
dayofyear2[ii] = dayofyear2[ii]-1
# dayofyear2 for Feb 29 will be 59 instead of 60
# check above

print(dayofyear2[1153:1158])
#1940-02-27 1940-02-28 1940-02-29 1940-03-01 1940-03-02 1940-03-03 
#        58         59         59         60         61         62 


# Function to extract year, month, day from a timeDate vector 
# Use library(lubridate) instead
#extrdatenum = function(dateobj)
#{ n = length(dateobj)
#  str = toString(dateobj)
#  tem = unlist(strsplit(str,","))
#  tem = unlist(strsplit(tem,"-"))
#  ymd = matrix(as.numeric(tem),3,n)
#  t(ymd)
#}


# dif with 90th percentile from 1961-1990 , keep positive cases
diffw90 = maxtemp-yvr_q90[dayofyear2]
diffw90 = pmax(0,diffw90)

library(lubridate)
year = lubridate::year(yvr$yyyymmdd)
month = lubridate::month(yvr$yyyymmdd)
yrmon = 100*year+month
# summarize average by month to get a monthly time series
diff = data.frame(diffw90=diffw90, year=year, month=month,
  yrmon=100*year+month)

sub =subset(diff, year==1941 & month==7)
print(sub[14:17,])
#1656    1.70 1941     7 194107
#1657    3.20 1941     7 194107
#1658    4.29 1941     7 194107
#1659    4.78 1941     7 194107

sum(sub$diffw90)
#[1] 15.66
# 15.66/4
#[1] 3.915


library(tibble)
library(dplyr)
library(magrittr)

# use dplyr to get group averages and counts?

byMonth = as_tibble(diff) %>% 
   dplyr::group_by(yrmon) %>%
   dplyr::summarise(nexc =sum(diffw90>0),
    avgexc = sum(diffw90)/max(1,sum(diffw90>0)))

head(byMonth)
# A tibble: 6 × 3
#   yrmon  nexc  avgexc
#   <dbl> <int>   <dbl>
#1 193701     0   0
#2 193702     0   0
#3 193703     5   2.65   # at most 3 consecutive
#4 193704     1   1.10 
#5 193705     2   0.990  # 2 consecutive days
#6 193706     5   2.10  # a couple of 2-day heat events

exc = data.frame(avgexc=byMonth$avgexc, nexc=byMonth$nexc,
   year=floor(byMonth$yrmon/100), month=byMonth$yrmon%%100)

head(exc)
#  avgexc nexc year month
#1  0.000    0 1937     1
#2  0.000    0 1937     2
#3  2.654    5 1937     3
#4  1.100    1 1937     4
#5  0.990    2 1937     5
#6  2.100    5 1937     6

exc[exc$year==1941,]
#     avgexc nexc year month
#49 2.056250    8 1941     1
#50 1.676667    6 1941     2
#51 2.486000   10 1941     3
#52 1.372500    8 1941     4
#53 0.000000    0 1941     5
#54 2.800000    1 1941     6
#55 3.132000    5 1941     7
#56 1.895000    2 1941     8
#57 0.000000    0 1941     9
#58 1.150000    2 1941    10
#59 1.267000   10 1941    11
#60 1.678000    5 1941    12

exc[exc$year==2021,]
#        avgexc nexc year month
#1009 0.8833333    6 2021     1
#1010 0.0000000    0 2021     2
#1011 1.1900000    1 2021     3
#1012 1.9071429    7 2021     4
#1013 1.3900000    1 2021     5
#1014 5.9285714    7 2021     6
#1015 0.7528571    7 2021     7
#1016 1.7300000    6 2021     8
#1017 1.2380000    5 2021     9
#1018 2.3900000    1 2021    10
#1019 0.8428571    7 2021    11
#1020 1.9000000    1 2021    12

exc[exc$year==1960,]
#      avgexc nexc year month
#277 0.872500    4 1960     1
#278 0.995000    2 1960     2
#279 3.996667    3 1960     3
#280 2.565000    6 1960     4
#281 2.800000    1 1960     5
#282 0.000000    0 1960     6
#283 1.923333    9 1960     7
#284 4.466667    3 1960     8
#285 0.700000    1 1960     9
#286 1.250000    2 1960    10
#287 0.000000    0 1960    11
#288 1.695000    2 1960    12

#plot.ts(exc$avgexc)
summary(exc$avgexc)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.000   0.420   1.145   1.186   1.778   5.929 

#plot.ts(exc$nexc)

summary(exc$nexc)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.000   1.000   2.000   3.125   5.000  16.000

ii = which(exc$nexc==16)
#[1]  259 1030
#exc[ii,]
#       avgexc nexc year month
#259  1.949375   16 1958     7
#1030 1.225625   16 2022    10

sub195807 =subset(diff, year==1958 & month==7)
#sub195807
#     diffw90 year month  yrmon
#7852    0.00 1958     7 195807
#7853    0.60 1958     7 195807
#7854    0.19 1958     7 195807
#7855    2.30 1958     7 195807
#7856    3.30 1958     7 195807
#7857    3.77 1958     7 195807
#7858    2.39 1958     7 195807
#7859    0.00 1958     7 195807
#7860    0.00 1958     7 195807
#7861    0.00 1958     7 195807
#7862    0.09 1958     7 195807
#7863    0.00 1958     7 195807
#7864    0.00 1958     7 195807
#7865    1.10 1958     7 195807
#7866    3.20 1958     7 195807
#7867    3.19 1958     7 195807
#7868    1.98 1958     7 195807
#7869    0.00 1958     7 195807
#7870    0.00 1958     7 195807
#7871    0.00 1958     7 195807
#7872    1.10 1958     7 195807
#7873    0.00 1958     7 195807
#7874    0.00 1958     7 195807
#7875    0.00 1958     7 195807
#7876    0.60 1958     7 195807
#7877    0.00 1958     7 195807
#7878    2.59 1958     7 195807
#7879    4.30 1958     7 195807
#7880    0.49 1958     7 195807
#7881    0.00 1958     7 195807
#7882    0.00 1958     7 195807

sub202210 =subset(diff, year==2022 & month==10)
#      diffw90 year month  yrmon
#31290    0.00 2022    10 202210
#31291    0.30 2022    10 202210
#31292    1.60 2022    10 202210
#31293    0.50 2022    10 202210
#31294    0.00 2022    10 202210
#31295    1.10 2022    10 202210
#31296    2.19 2022    10 202210
#31297    1.40 2022    10 202210
#31298    0.60 2022    10 202210
#31299    1.49 2022    10 202210
#31300    0.24 2022    10 202210
#31301    0.00 2022    10 202210
#31302    0.50 2022    10 202210
#31303    1.30 2022    10 202210
#31304    1.00 2022    10 202210
#31305    3.50 2022    10 202210
#31306    1.89 2022    10 202210
#31307    0.50 2022    10 202210
#31308    0.00 2022    10 202210
#31309    1.50 2022    10 202210
#31310    0.00 2022    10 202210
#31311    0.00 2022    10 202210
#31312    0.00 2022    10 202210
#31313    0.00 2022    10 202210
#31314    0.00 2022    10 202210
#31315    0.00 2022    10 202210
#31316    0.00 2022    10 202210
#31317    0.00 2022    10 202210
#31318    0.00 2022    10 202210
#31319    0.00 2022    10 202210
#31320    0.00 2022    10 202210

#maxtemp[year==2022 & month==10]
# [1] 19.50 19.70 21.00 19.40 18.10 19.40 20.40 19.40 18.40 19.10 17.55 16.00
#[13] 17.70 18.30 17.70 20.20 18.20 16.60 13.90 17.10 13.00 12.30 11.90 13.30
#[25] 10.20 10.20 12.20 14.20 12.80 11.90 12.30

# combine with length of heatwave?

# want to get average exceedance during heat waves? if one exists
# all take average exceedance use diff = max(0,maxdif-q90) ?

exc$avgexc = round(exc$avgexc,3)
diff$diffw90 = round(diff$diffw90,3)
write.csv(file="yvr-maxtemp-avgexc-bymonth.csv", exc, row.names=F)
write.csv(file="yvr-maxtemp-exc-over90th.csv", diff, row.names=F)
