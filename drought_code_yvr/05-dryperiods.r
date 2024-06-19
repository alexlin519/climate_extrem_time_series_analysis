
extract_dry_periods <- function(input_path_temp_precip,station_name,
                                dry_threshold = 1, min_dry_days = 9) {
  ###
  #dry_threshold = 1 mm
  #min_dry_days = 9 days, if dry day longer than 9 days, defined as drought
  ###
  
  
  # Load data
  load(input_path_temp_precip)
  
  # Remove cases in early years if too many missing values
  # Impute missing daily max and min
  
  # Impute missing temperatures
  maxtmp <- yvr$maxtemp
  maxtemp <- maxtmp
  imax = missing_maxtem
  for (i in imax) {
    i1 <- i - 1
    while (is.na(maxtmp[i1])) { i1 <- i1 - 1 }
    max1 <- maxtmp[i1]
    i2 <- i + 1
    while (is.na(maxtmp[i2])) { i2 <- i2 + 1 }
    max2 <- maxtmp[i2]
    maxtemp[i] <- (max1 + max2) / 2
  }
  
  mintmp <- yvr$mintemp
  mintemp <- mintmp
  imin = missing_mintem
  for (i in imin) {
    i1 <- i - 1
    while (is.na(mintmp[i1])) { i1 <- i1 - 1 }
    min1 <- mintmp[i1]
    i2 <- i + 1
    while (is.na(mintmp[i2])) { i2 <- i2 + 1 }
    min2 <- mintmp[i2]
    mintemp[i] <- (min1 + min2) / 2
  }
  
  iomit <- 1:59  # data cleaning: first 60 rows has missing precipation
  yvr2 <- yvr[-iomit,]
  precip <- yvr2$totprec
  
  precip[is.na(precip)] <- 0
  
  mintemp <- mintemp[-iomit]
  maxtemp <- maxtemp[-iomit]
  
  n <- length(precip)
  smin <- 0
  smax <- 0
  kcount <- 0
  begin_no <- NULL
  end_no <- NULL
  tempmin_no <- NULL
  tempmax_no <- NULL
  
  # Find periods with precip <= dry_threshold
  idry <- (precip[1] < dry_threshold)
  
  if (idry) {
    begin_no <- c(begin_no, 1)
    iend <- 1
    smin <- smin + mintemp[1]
    smax <- smax + maxtemp[1]
    kcount <- 1
  }
  
  for (i in 2:n) {
    if (idry & (precip[i] <= dry_threshold)) {
      iend <- i
      smin <- smin + mintemp[i]
      smax <- smax + maxtemp[i]
      kcount <- kcount + 1
    } else if (idry & (precip[i] >dry_threshold)) {
      end_no <- c(end_no, iend)
      tempmin_no <- c(tempmin_no, smin / kcount)
      tempmax_no <- c(tempmax_no, smax / kcount)
      idry <- FALSE
      smin <- 0
      smax <- 0
      kcount <- 0
    } else if (!idry & (precip[i] <= dry_threshold)) {
      begin_no <- c(begin_no, i)
      iend <- i
      idry <- TRUE
      smin <- smin + mintemp[i]
      smax <- smax + maxtemp[i]
      kcount <- 1
    }
  }
  
  if (idry) {
    end_no <- c(end_no, iend)
    tempmin_no <- c(tempmin_no, smin / kcount)
    tempmax_no <- c(tempmax_no, smax / kcount)
  }
  
  yyyymmdd <- yvr2$yyyymmdd
  periods_no <- data.frame(
    ibegin = begin_no, iend = end_no,
    length = end_no - begin_no + 1,
    begin = yyyymmdd[begin_no], end = yyyymmdd[end_no],
    avgmin = tempmin_no, avgmax = tempmax_no
  )
  
  
  periods_dry <- periods_no[end_no - begin_no >= min_dry_days, ] 
  
  extrdatenum <- function(dateobj) {
    n <- length(dateobj)
    str <- toString(dateobj)
    tem <- unlist(strsplit(str, ","))
    tem <- unlist(strsplit(tem, "-"))
    ymd <- matrix(as.numeric(tem), 3, n)
    t(ymd)
  }
  
  ymd_dry <- extrdatenum(periods_dry$begin)
  periods_dry$year <- ymd_dry[, 1]
  periods_dry$month <- ymd_dry[, 2]
  
  periods_dry$avgmin <- round(periods_dry$avgmin, 2)
  periods_dry$avgmax <- round(periods_dry$avgmax, 2)
  
  #
  
  # Define the specific path where you want to save the data (replace with your actual path)
  save_path <- "../drought_code_yvr/data"
  # Construct the file name with station name
  file_name <- paste0(station_name, "_dates_dryperiods.csv")
  # Create the full path to the file using file.path()
  full_file_path <- file.path(save_path, file_name)
  
  # Save the data and missing information
  write.csv(file = full_file_path, periods_dry, row.names = FALSE)
  return(full_file_path)
}



# 
# 
# # extract dry periods : consecutive days rainfall < 1mm
# # no precip data for first two months of 1937: 59 days
# 
# load("yvr-temp-precip.RData")
# 
# #======================================================================
# 
# # Remove cases in early years if too many missing values
# # Impute missing daily max and min 
# 
# imax = missing_maxtem
# icheckmax = sort(c(imax,imax-1,imax+1))
# icheckmax = unique(icheckmax)
# icheckmax
# 
# imin = missing_mintem
# icheckmin = sort(c(imin,imin-1,imin+1))
# icheckmin = unique(icheckmin)
# icheckmin
# #  [1]   134   135   136   320   321   322  2012  2013  2014 21495 21496 21497
# # [13] 21498 21499 21500 21668 21669 21670 21748 21749 21750 21845 21846 21847
# # [25] 23818 23819 23820 29099 29100 29101 29784 29785 29786 30350 30351 30352
# # [37] 30358 30359 30360 30364 30365 30366 30782 30783 30784 30816 30817 30818
# # [49] 30821 30822 30823 30972 30973 30974 31141 31142 31143 31210 31211 31212
# # [61] 31237 31238 31239 31240 31258 31259 31260 31261 31277 31278 31279 31299
# # [73] 31300 31301 31469 31470 31471 31472 31473 31474 31520 31521 31522 31523
# # [85] 31524 31699 31700 31701 31704 31705 31706 31754 31755 31756 31762 31763
# # [97] 31764 31805 31806 31807 31810 31811 31812 31815 31816 31817 31851 31852
# #[109] 31853 31865 31866 31867
# 
# 
# icheck = c(icheckmin, icheckmax)
# icheck = unique(icheck)
# #length(icheck)
# #[1] 143
# 
# #write.csv(file="yvr-missing-temp.csv",yvr[icheck,])
# # in most cases, little difference for temperatures available before and after
# 
# # Impute missing temperatures
# 
# maxtmp = yvr$maxtemp
# # Missing values be imputed with two neighbors.
# maxtemp = maxtmp
# # Below works if first and last values are not missing
# for(i in imax)
# { i1 = i-1
#   while(is.na(maxtmp[i1])) { i1 = i1-1 }
#   max1 = maxtmp[i1]
#   i2 = i+1
#   while(is.na(maxtmp[i2])) { i2 = i2+1 }
#   max2 = maxtmp[i2]
#   maxtemp[i]=(max1+max2)/2
#   #cat(maxtemp[(i-1):(i+1)],"\n")
# }
# 
# mintmp = yvr$mintemp
# # Missing values be imputed with two neighbors.
# mintemp = mintmp
# # Below works if first and last values are not missing
# for(i in imin)
# { i1 = i-1
#   while(is.na(mintmp[i1])) { i1 = i1-1 }
#   min1 = mintmp[i1]
#   i2 = i+1
#   while(is.na(mintmp[i2])) { i2 = i2+1 }
#   min2 = mintmp[i2]
#   mintemp[i] = (min1+min2)/2
#   #cat(mintemp[(i-1):(i+1)],"\n")
# }
# 
# 
# #======================================================================
# 
# iomit = 1:59  # missing precipation
# yvr2 = yvr[-iomit,]
# # first look at distribution of precipitation over days
# precip = yvr2$totprec
# 
# summary(precip)
# #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# #  0.000   0.000   0.000   3.098   3.400  91.600      63
# 
# precip[is.na(precip)] = 0
# summary(precip)
# #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# #  0.000   0.000   0.000   3.092   3.400  91.600 
# 
# #length(precip)
# # 31819
# 
# # percent ==0
# #sum(precip==0)
# # 17564   17564/31819 = 0.552
# 
# # percent <1 mm (check that unit is mm) (true for German data)
# 
# #sum(precip<1)
# # <1 20156    20156/31819  = 0.633
# 
# mintemp = mintemp[-iomit]
# maxtemp = maxtemp[-iomit]
# 
# # Find periods of precip all <= 1mm
# # get average daily max temp and daily min temp during period
# n = length(precip)  # 31819
# smin = 0; smax = 0; kcount = 0
# begin_no = NULL; end_no = NULL
# tempmin_no = NULL; tempmax_no = NULL
# idry = (precip[1]<1)
# if(idry) 
# { begin_no = c(begin_no,1); iend = 1; 
#   smin = smin+mintemp[1]; smax = smax+maxtemp[1]; kcount = 1
# }
# #for(i in 2:100) # for testing
# for(i in 2:n)
# { if(idry & (precip[i]<=1)) 
#   { iend = i; smin = smin+mintemp[i]; smax = smax+maxtemp[i]; kcount = kcount+1 }
#   else if(idry & (precip[i]>1)) 
#   { end_no = c(end_no,iend);
#     tempmin_no = c(tempmin_no,smin/kcount)
#     tempmax_no = c(tempmax_no,smax/kcount)
#     idry = F; smin = 0; smax = 0; kcount = 0
#   }
#   else if(!idry & (precip[i]<=1)) 
#   { begin_no = c(begin_no,i); iend = i; idry = T 
#     smin = smin+mintemp[i]; smax = smax+maxtemp[i]; kcount = 1 
#   }
# }
# if(idry)  
# { end_no = c(end_no,iend) 
#   tempmin_no = c(tempmin_no,smin/kcount)
#   tempmax_no = c(tempmax_no,smax/kcount)
# }
# 
# yyyymmdd=yvr2$yyyymmdd
# periods_no = data.frame(ibegin=begin_no, iend=end_no,
#   length=end_no-begin_no+1,
#   begin=yyyymmdd[begin_no], end=yyyymmdd[end_no],
#   avgmin=tempmin_no, avgmax=tempmax_no)
# 
# dim(periods_no)
# #[1] 4698    7
# 
# library(timeDate)
# 
# tem = periods_no$end-periods_no$begin+1
# class(tem)
# #[1] "difftime"
# 
# summary(as.numeric(tem))
# #   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# #  1.000   1.000   2.000   4.395   5.000  75.000 
# 
# quantile(tem,c(0.9,0.95,0.99))
# #90% 95% 99% 
# # 10  14  30 
# 
# #print(periods_no[1:18,]) # first 100 days
# #   ibegin iend length      begin        end    avgmin   avgmax
# #1       2    3      2 1937-03-02 1937-03-03  3.900000 13.05000
# #2       6    7      2 1937-03-06 1937-03-07 -0.550000 10.55000
# #3      10   12      3 1937-03-10 1937-03-12  4.800000 10.16667
# #4      14   16      3 1937-03-14 1937-03-16  1.833333 11.66667
# #5      18   19      2 1937-03-18 1937-03-19  2.800000 10.00000
# #6      21   24      4 1937-03-21 1937-03-24  3.075000 10.30000
# #7      26   28      3 1937-03-26 1937-03-28  1.466667 10.93333
# #8      31   31      1 1937-03-31 1937-03-31  1.100000 13.90000
# #9      33   33      1 1937-04-02 1937-04-02  3.300000 10.00000
# #10     38   39      2 1937-04-07 1937-04-08  6.400000 14.70000
# #11     46   48      3 1937-04-15 1937-04-17  5.200000 12.60000
# #12     52   53      2 1937-04-21 1937-04-22  4.450000 11.10000
# #13     55   56      2 1937-04-24 1937-04-25  6.150000 12.20000
# #14     58   63      6 1937-04-27 1937-05-02  5.366667 14.35000
# #15     65   67      3 1937-05-04 1937-05-06  4.066667 14.63333
# #16     72   72      1 1937-05-11 1937-05-11  7.200000 16.10000
# #17     75   77      3 1937-05-14 1937-05-16  3.900000 15.30000
# #18     79  100     22 1937-05-18 1937-06-08  7.672727 18.71364
# 
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
# 
# 
# periods_dry = periods_no[end_no-begin_no>=9,]
# dim(periods_dry)
# #[1] 539   7
# 
# 
# #----------------------------------------------------------------------
# 
# # Add month to dates_dry dataframe
# ymd_dry = extrdatenum(periods_dry$begin)
# periods_dry$year = ymd_dry[,1]
# periods_dry$month = ymd_dry[,2]
# 
# periods_dry$avgmin = round(periods_dry$avgmin,2)
# periods_dry$avgmax = round(periods_dry$avgmax,2)
# 
# #sink("yvr_dates_drought.txt")
# #print(periods_dry)
# #sink()
# 
# dim(periods_dry)
# # 539 9
# 
# write.csv(file="yvr_dates_dryperiods.csv", periods_dry, row.names=F)
# 
# table(periods_dry$year)
# #1937 1938 1939 1940 1941 1942 1943 1944 1945 1946 1947 1948 1949 1950 1951 1952 
# #   5    8    5    7    8    5    6    7    7    5    4    7    7    5    5    6 
# #1953 1954 1955 1956 1957 1958 1959 1960 1961 1962 1963 1964 1965 1966 1967 1968 
# #   7    7    7    8    9    6    7    6    6    6    7    3    7    7    9    4 
# #1969 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979 1980 1981 1982 1983 1984 
# #   6    9    2    5   10    4    6    7    4    6    9    5    3    7    6    7 
# #1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 
# #   5    4    8    5    8    6    8   10    5   10    8    7    5    5    5    8 
# #2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 
# #   5    6    7    5    7    5    6    3    9    4    4    6    6    9    7    5 
# #2017 2018 2019 2020 2021 2022 2023 
# #   7    6    7    6    3    5    5 
# 
# 
# table(periods_dry$month)
# # 1  2  3  4  5  6  7  8  9 10 11 12 
# #22 24 23 37 70 64 87 78 71 28 14 21
# 
