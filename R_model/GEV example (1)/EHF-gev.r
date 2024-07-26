# GEV for max EHF June to September, by location

library(evd)
source("../R_model/GEV example (1)/gev.R")

#current path
getwd()
setwd("/Users/alexlin/summer_stat/climate_extreme_RA/R")
ehf0609 = read.csv("../R_model/GEV example (1)/ehf0609.csv", header=T)
#names(ehf0609)

cat("Summary statistics\n")
print(summary(ehf0609))

sp = cor(ehf0609, method="spearman")
cat("\nSpearman correlation matrix\n")
round(sp,2)
#     year   fn   pg   ke   ab   yv
#year 1.00 0.06 0.18 0.09 0.38 0.35
#fn   0.06 1.00 0.48 0.41 0.54 0.39
#pg   0.18 0.48 1.00 0.54 0.65 0.68
#ke   0.09 0.41 0.54 1.00 0.57 0.58
#ab   0.38 0.54 0.65 0.57 1.00 0.81
#yv   0.35 0.39 0.68 0.58 0.81 1.00

# permute columns (pg and ke)
cat("\nSpearman correlation matrix, permuted\n")
iperm=c(1,2,4,3,5,6)
sp2 = sp[iperm,iperm]
round(sp2,2)


cat("\nregression on scaled year, by location\n")

#par(mfrow=c(3,3))
plot(fn~year, data=ehf0609)
FN_trend = lm(fn ~ I((year-1941)/10), data=ehf0609)
summary(FN_trend)
cat("\n------------------------------------------------------------\n\n")

plot(pg~year, data=ehf0609)
PG_trend = lm(pg ~ I((year-1941)/10), data=ehf0609)
summary(PG_trend)
cat("\n------------------------------------------------------------\n\n")

plot(ke~year, data=ehf0609)
KE_trend = lm(ke ~ I((year-1941)/10), data=ehf0609)
summary(KE_trend)
cat("\n------------------------------------------------------------\n\n")

plot(ab~year, data=ehf0609)
AB_trend = lm(ab ~ I((year-1941)/10), data=ehf0609)
summary(AB_trend)
cat("\n------------------------------------------------------------\n\n")

plot(yv~year, data=ehf0609)
YV_trend = lm(yv ~ I((year-1941)/10), data=ehf0609)
summary(YV_trend)
cat("\n------------------------------------------------------------\n\n")

# What are better methods to assess trend?
# Create the plot
ggplot(ehf0609, aes(x = year, y = yv)) +
  geom_point() +  # Plot the data points
  geom_smooth(method = "lm", formula = y ~ I((x - 1941) / 10), color = "blue") +  # Add the linear regression line
  labs(title = "Trend of YV Over Years",
       x = "Year",
       y = "YV") +
  theme_minimal()
# Example: LOESS smoothing
library(ggplot2)
ggplot(ehf0609, aes(x=year, y=fn)) + 
  geom_point() + 
  geom_smooth(method="loess")
3.# Example: Seasonal Decomposition
library(forecast)
decomposed = stl(ts(ehf0609$fn, frequency=12), s.window="periodic")
plot(decomposed)

# ============================================================

# serial correlation for signed cube root

cuberoot = function(x) { sign(x)*(abs(x)^(1/3)) }

for(j in 2:6)
{ cat("\nacf for cuberoot of ", names(ehf0609)[j],"\n")
  acfvec = acf(cuberoot(ehf0609[,j]), lag.max=10, plot=F)
  print(round(c(acfvec$acf),3))
}


# ============================================================

cat("\nFit GEV = generalized extreme value distribution by location\n")

# For GEV, assume a random sample (but a trend could be
# added to the location parameter if nlm is use for numerical MLE)

FN_gev = fgev(ehf0609$fn)
#print(FN_gev)
#Estimates
#  loc  scale  shape  
#1.953  5.372  0.396  

#Standard Errors
#   loc   scale   shape  
#0.7218  0.6448  0.1346  

# tail parameter = shape >0 implies heavy-tailed

#names(FN_gev)
#[1] "estimate"    "std.err"     "fixed"       "param"       "deviance"   
#[6] "corr"        "var.cov"     "convergence" "counts"      "message"    
#[11] "data"        "tdata"       "nsloc"       "n"           "prob"       
#[16] "loc"         "call"    

mle = FN_gev$estimate
# qqplot

n = nrow(ehf0609)
ii = ((1:n)-0.5)/n
quant_gev = qgev(ii, xi=mle[3], mu=mle[1], sigma=mle[2])
plot(quant_gev,sort(ehf0609$fn)); abline(c(0,1)) # fit is OK

# wrapper function

wrap_gev = function(y,name="y")
{ gevobj = fgev(y)
  cat("\n",name,"\n")
  print(gevobj)
  mle = gevobj$estimate
  n = length(y)
  ii = ((1:n)-0.5)/n
  quant_gev = qgev(ii, xi=mle[3], mu=mle[1], sigma=mle[2])
  plot(quant_gev,sort(y)); title(name); abline(c(0,1)) 
  cat("\n============================================================\n")
  mle
}
#special permatrix cases


par(mfrow=c(3,3))
FN_gev = wrap_gev(ehf0609$fn,"FN")
PG_gev = wrap_gev(ehf0609$pg,"PG")
KE_gev = wrap_gev(ehf0609$ke,"Kelowna")
AB_gev = wrap_gev(ehf0609$ab,"Abbotsford")
YV_gev = wrap_gev(ehf0609$yv,"YVR")
