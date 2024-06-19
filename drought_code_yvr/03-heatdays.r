library(timeDate)
library(dplyr)

process_heat_periods <- function(input_path_temp_precip, input_path_stat, station_name) {
  # Load data
  load(input_path_temp_precip)
  load(input_path_stat)
  
  # imx <- missing_maxtem
  # icheckmx <- sort(c(imx, imx - 1, imx + 1))
  # icheckmx <- unique(icheckmx)
  # 
  # write.csv(file = paste0("../drought_code_yvr/data/", station_name, "_missing_maxtemp.csv"), yvr[icheckmx,])
  
  # Impute missing values
  maxtmp <- yvr$maxtemp
  maxtemp <- maxtmp
  for (i in imx) {
    i1 <- i - 1
    while (is.na(maxtmp[i1])) { i1 <- i1 - 1 }
    mx1 <- maxtmp[i1]
    i2 <- i + 1
    while (is.na(maxtmp[i2])) { i2 <- i2 + 1 }
    mx2 <- maxtmp[i2]
    maxtemp[i] <- (mx1 + mx2) / 2
    #cat(maxtemp[(i - 1):(i + 1)], "\n")
  }
  
  # Find cases of exceeding 90th percentile of the dayofyear
  dayofyear <- dayOfYear(timeDate(yvr$yyyymmdd))
  yr <- yvr$year
  dayofyear2 <- dayofyear
  ileap <- (yr %% 4 == 0 & dayofyear == 60)
  ii <- (yr %% 4 == 0 & dayofyear > 60)
  dayofyear2[ileap] <- 59
  dayofyear2[ii] <- dayofyear2[ii] - 1
  
  iexceed <- (maxtemp > yvr_q90[dayofyear2])
  
  n <- length(iexceed)
  begin_ex <- NULL
  end_ex <- NULL
  iex <- iexceed[1]
  if (iex) { begin_ex <- c(begin_ex, 1); iend <- 1 }
  
  for (i in 2:n) {
    if (iex && iexceed[i]) {
      iend <- i
    } else if (iex && !iexceed[i]) {
      end_ex <- c(end_ex, iend)
      iex <- FALSE
    } else if (!iex && iexceed[i]) {
      begin_ex <- c(begin_ex, i)
      iend <- i
      iex <- TRUE
    }
  }
  if (iex) { end_ex <- c(end_ex, iend) }
  periods_ex <- cbind(begin_ex, end_ex)
  periods_heat <- periods_ex[end_ex - begin_ex >= 2,]
  
  dates_heatwave <- data.frame(
    begin = yvr$yyyymmdd[periods_heat[, 1]],
    end = yvr$yyyymmdd[periods_heat[, 2]],
    starttemp = maxtemp[periods_heat[, 1]],
    endtemp = maxtemp[periods_heat[, 2]]
  )
  
  # Function to extract year, month, day from a timeDate vector
  extrdatenum <- function(dateobj) {
    n <- length(dateobj)
    str <- toString(dateobj)
    tem <- unlist(strsplit(str, ","))
    tem <- unlist(strsplit(tem, "-"))
    ymd <- matrix(as.numeric(tem), 3, n)
    t(ymd)
  }
  
  oneyear <- yvr$yyyymmdd[1:365]
  ymd <- extrdatenum(oneyear)
  
  q90_df <- data.frame(q90 = yvr_q90, month = ymd[, 2], day = ymd[, 3])
  q90_csv_path <- paste0("../drought_code_yvr/data/", station_name, "_q90.csv")
  write.csv(file = q90_csv_path, q90_df)
  
  ymd_heat <- extrdatenum(dates_heatwave$begin)
  dates_heatwave$year <- ymd_heat[, 1]
  dates_heatwave$month <- ymd_heat[, 2]
  heatwave_csv_path <- paste0("../drought_code_yvr/data/", station_name, "_dates_heatwave.csv")
  write.csv(file = heatwave_csv_path, dates_heatwave, row.names = FALSE)
  
  
  return(list(q90_csv_path = q90_csv_path, heatwave_csv_path = heatwave_csv_path))
}




# 
# # Heat periods with 3 or more consecutive exceedances
# 
# load("yvr-temp-precip.RData")
# load("yvrstat.RData")
# 
# imx = missing_maxtem
# icheckmx = sort(c(imx,imx-1,imx+1))
# icheckmx = unique(icheckmx)
# icheckmx
# #  [1]   134   135   136   445   446   447   448   449   450 21495 21496 21497
# # [13] 21498 21499 21500 21668 21669 21670 21748 21749 21750 21845 21846 21847
# # [25] 23818 23819 23820 28089 28090 28091 28242 28243 28244 28624 28625 28626
# # [37] 28872 28873 28874 29012 29013 29014 29302 29303 29304 29487 29488 29489
# # [49] 29500 29501 29502 29503 30350 30351 30352 30358 30359 30360 30364 30365
# # [61] 30366 30782 30783 30784 30816 30817 30818 30821 30822 30823 30972 30973
# # [73] 30974 31141 31142 31143 31210 31211 31212 31237 31238 31239 31240 31258
# # [85] 31259 31260 31261 31277 31278 31279 31299 31300 31301 31469 31470 31471
# # [97] 31472 31473 31474 31520 31521 31522 31523 31524 31699 31700 31701 31704
# #[109] 31705 31706 31754 31755 31756 31762 31763 31764 31805 31806 31807 31810
# #[121] 31811 31812 31815 31816 31817 31851 31852 31853 31865 31866 31867
# 
# write.csv(file="yvr-missing-maxtemp.csv",yvr[icheckmx,])
# # No more than 4 missing values in a row
# 
# maxtmp = yvr$maxtemp
# # Missing values be imputed with two neighbors.
# 
# maxtemp = maxtmp
# # Below works if first and last values are not missing
# for(i in imx)
# { i1 = i-1
#   while(is.na(maxtmp[i1])) { i1 = i1-1 }
#   mx1 = maxtmp[i1]
#   i2 = i+1
#   while(is.na(maxtmp[i2])) { i2 = i2+1 }
#   mx2 = maxtmp[i2]
#   maxtemp[i]=(mx1+mx2)/2
#   cat(maxtemp[(i-1):(i+1)],"\n")
# }
# 
# # Find cases of exceeding 90th percentile of the dayofyear
# library(timeDate)
# dayofyear = dayOfYear(timeDate(yvr$yyyymmdd))
# yr = yvr$year
# dayofyear2 = dayofyear
# ileap = (yr%%4==0 & dayofyear==60)
# ii = (yr%%4==0 & dayofyear>60)
# # handling leap days in every 4th year (including year 2000)
# dayofyear2[ileap] = 59
# dayofyear2[ii] = dayofyear2[ii]-1
# 
# iexceed = (maxtemp>yvr_q90[dayofyear2]) 
# 
# length(iexceed)
# # 31878
# 
# sum(iexceed)
# # 3278 (about 1/10)
# 
# # Find periods with three of more consecutive exceedances
# n = length(iexceed)
# begin_ex = NULL; end_ex=NULL
# iex = iexceed[1]
# if(iex) { begin_ex = c(begin_ex,1); iend = i }
# #for(i in 2:1000) # for testing loop
# for(i in 2:n)
# { if(iex & iexceed[i]) { iend = i; }
#   else if(iex & !(iexceed[i])) { end_ex = c(end_ex,iend); iex = F }
#   else if(!iex & iexceed[i]) { begin_ex = c(begin_ex,i); iend = i; iex = T }
#   #else(!inew & !(iexceed[i]))  { do nothing }
# }
# if(iex)  { end_ex = c(end_ex,iend) }
# periods_ex = cbind(begin_ex,end_ex)
# periods_heat = periods_ex[end_ex-begin_ex>=2,]
# # convert to dates
# dates_heatwave = data.frame(begin=yvr$yyyymmdd[periods_heat[,1]],
#   end=yvr$yyyymmdd[periods_heat[,2]], starttemp=maxtemp[periods_heat[,1]],
#   endtemp=maxtemp[periods_heat[,2]])
# 
# # 327 cases
# 
# #----------------------------------------------------------------------
# 
# # Function to extract year, month, day from a timeDate vector 
# # Alternative with functions in library(libridate)
# extrdatenum = function(dateobj)
# { n = length(dateobj)
#   str = toString(dateobj)
#   tem = unlist(strsplit(str,","))
#   tem = unlist(strsplit(tem,"-"))
#   ymd = matrix(as.numeric(tem),3,n)
#   t(ymd)
# }
# #----------------------------------------------------------------------
# 
# oneyear =  yvr$yyyymmdd[1:365]
# ymd = extrdatenum(oneyear)
# 
# # compare with csv file
# q90_df = data.frame(q90=yvr_q90, month=ymd[,2], day=ymd[,3])
# #View(q90_df)
# write.csv(file="q90-yvr.csv", q90_df)
# 
# #----------------------------------------------------------------------
# 
# # add month to dates_heatwave dataframe
# ymd_heat = extrdatenum(dates_heatwave$begin)
# dates_heatwave$year = ymd_heat[,1]
# dates_heatwave$month = ymd_heat[,2]
# 
# #sink("yvr_dates_heatwave.txt")
# #print(dates_heatwave)
# # 327 cases (average of 327/87 = 3.8 per year)
# #sink()
# 
# write.csv(file="yvr_dates_heatwave.csv", dates_heatwave, row.names=F)
# 
# table(dates_heatwave$year)
# 
# #1937 1938 1939 1940 1941 1942 1943 1944 1945 1947 1949 1950 1951 1952 1953 1956 
# #   4    1    6    5    6    3    1    3    3    3    4    4    4    6    3    5 
# #1957 1958 1959 1960 1961 1962 1963 1965 1966 1967 1968 1969 1970 1971 1972 1973 
# #   3    8    2    3    5    4    4    1    3    5    8    1    1    1    3    1 
# #1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 
# #   5    2    2    6    4    4    3    3    1    4    2    3    3   10    1    5 
# #1990 1991 1992 1993 1994 1995 1996 1997 1998 2001 2002 2003 2004 2005 2006 2008 
# #   7    3    6    4    6    8    4    4    6    2    2    7    7    8    6    2 
# #2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022 2023 2024 
# #   2    7    2    1    2    7    8    7    4    5    5    2    7    7    6    1 
# 
# table(dates_heatwave$month)
# # 1  2  3  4  5  6  7  8  9 10 11 12 
# #35 18 22 31 35 27 40 27 21 18 26 27 
# 
# # 40 heat waves in July from 1937-2023: 87 years
# 
# # Maybe better to look at monthly maximum temp in May, June, July, August,
# # September for each year
# 
# # Do this for YVR, Kelowna, Prince George, Fort Nelson to check
# # on correlations over months and locations for 87 years of weather data.
