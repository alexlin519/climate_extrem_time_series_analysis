# generalized extreme value

# revised from library(evir)
# For all functions below,
# xi = tail parameter, 
# mu = location parameter, 
# sigma = scale parameter

# x = vector or scalar 
dgev=function(x, xi=1, mu=0, sigma=1) 
{ tmp=1+(xi*(x-mu))/sigma
  tmpxi=tmp^(-1/xi)
  (as.numeric(tmp > 0) * (tmpxi/tmp) * exp(-tmpxi))/sigma
}

# x = vector or scalar
pgev=function(x, xi=1, mu=0, sigma=1) 
{ tem=pmax(0, 1+(xi*(x-mu))/sigma)
  exp(-tem^(-1/xi)) 
}

# p = vector or scalar with values in the interval (0,1)
qgev=function(p, xi=1, mu=0, sigma=1) 
{ mu+(sigma/xi)*((-log(p))^(-xi)-1) }

# This function assumes tmp>0 below
# x = vector or scalar 
logdgev=function(x, xi=1, mu=0, sigma=1)
{ tmp=1+(xi*(x-mu))/sigma
  lgtmp=log(tmp)
  tmpxi=exp(-lgtmp/xi)
  (-1/xi-1)*lgtmp -log(sigma) - tmpxi
}

myexample = function() {
# checks
xi=.5
mu=1
sigma=2
pdf=dgev(1:5,xi,mu,sigma)
lgpdf=logdgev(1:5,xi,mu,sigma)
cat("pdf, logpdf, exp(logpdf,\n")
print(cbind(pdf,lgpdf,exp(lgpdf)))
x=1:5; eps=1.e-5
cdf1=pgev(x,xi,mu,sigma)
cdf2=pgev(x+eps,xi,mu,sigma)
cat("numerical derivative of cdf\n")
print((cdf2-cdf1)/eps)
qq=qgev(cdf1,xi,mu,sigma)
cat("inverse of quantile\n")
print(qq)
invisible(0)
}

#myexample()
