# 90th percentiles of maxtemp in base period : years 1961 to 1990

load("yvr-temp-precip.RData")

baseyears0 = subset(yvr,year>=1960 & year<=1991)

baseyears = baseyears0[360:11330,]

dim(baseyears)
#[1] 10971     7

# get 90th percentiles from rows: 8 to 10963

# need two variables maxtemp and day of year
library(timeDate)
maxtmp = baseyears$maxtemp
dayofyear = dayOfYear(timeDate(baseyears$yyyymmdd))

# For simplicity, delete leap days 

# 8 dayofyear=366: 1960, 1964, 1968, 1972, 1976, 1980, 1984, 1988

# How to handle leap years
# days 1 to 59, days 60 to 365 in non-leap years
# days 1 to 59, 61 to 366 in leap years

yr = baseyears$year
dayofyear2 = dayofyear
ileap = (yr%%4==0 & dayofyear==60)
ii = (yr%%4==0 & dayofyear>60)
dayofyear2[ileap] = -1  # code for leap days Feb 29
dayofyear2[ii] = dayofyear2[ii]-1

df = data.frame(maxtmp=maxtmp, yr=yr, day=dayofyear2)

# function to get 90th percentile of 15-day window
# day 1: need 359-365, 1-8
# day 2: need 360-365, 1-9
#
# day 7: need 365, 1-14
# day 8: need 1-15
# day 9: need 2-16

# day 358: need 351-365
# day 359: need 352-365, 1
# day 360: need 353-365, 1-2
#
# day 364: need 357-365, 1-6
# day 365: need 358-365, 1-7

q90=rep(0,365)
for(i in 1:365)
{ iwin = (i-7):(i+7)
  iday = iwin%%365
  iday[iday==0] = 365
  subdf = df$maxtmp[df$day %in% iday]
  # should be between 30*15 = 450 and 464
  if(i<=14) subdf = subdf[1:450]
  if(i>=352)
  { ilen=length(subdf);  subdf = subdf[(ilen-449):ilen] }
  qtem = quantile(subdf,0.90, na.rm=T)
  cat(i, length(subdf),qtem, "\n")
  q90[i] = qtem
}

# plot(1:365,q90,type="l")

yvr_q90 = q90
save(file="yvrstat.RData", yvr_q90)

