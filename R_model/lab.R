install.packages("climextRemes")
library(climextRemes)
data(Fort, package = 'extRemes')
install.packages("ismev")
library(ismev)

#ismev package
#an introductin to statisital modeling of enxtrem values
#stuart coles



#yearly max of the prec
FortMax <- aggregate(Prec ~ year, data = Fort, max)
# stationary fit
out <- fit_gev(FortMax$Prec, returnPeriod = 20, returnValue = 3.5,
                getParams = TRUE, bootSE = FALSE)
print(out)


# $mle
# location     scale     shape 
# 1.3467234 0.5328195 0.1736623 
# $se_mle
# location      scale      shape 
# 0.06169221 0.04878825 0.09195978 
# $nllh
# [1] 104.9645
# $returnValue
# [1] 3.417704
# $se_returnValue
# [1] 0.3329517
# $logReturnProb
# [1] -3.08499
# $se_logReturnProb
# [1] 0.3811034
# 
# $logReturnPeriod
# [1] 3.08499
# $se_logReturnPeriod
# [1] 0.3811034
# 
# $info
# $info$convergence
# [1] 0
# $info$counts
# function gradient 
# 80       NA 
# 
# $info$message
# NULL
# 
# $info$failure
# [1] FALSE

# nonstationary fit with location linear in year
out <- fit_gev(FortMax$Prec, x = data.frame(years = FortMax$year),
                locationFun = ~years, returnPeriod = 20, returnValue = 3.5,
                getParams = TRUE, xNew = data.frame(years = range(FortMax$year)), bootSE = FALSE)

out




returnPeriod <- 20

