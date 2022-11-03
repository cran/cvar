### R code from vignette source 'Guide_cvar.Rnw'

###################################################
### code chunk number 1: Guide_cvar.Rnw:57-59
###################################################
library(cvar)
pd <- packageDescription("cvar")


###################################################
### code chunk number 2: Guide_cvar.Rnw:205-208
###################################################
cvar::VaR(qnorm, p_loss = 0.05, dist.type = "qf")
cvar::VaR(qnorm, p_loss = 0.05)
cvar::VaR(qnorm)


###################################################
### code chunk number 3: Guide_cvar.Rnw:212-213
###################################################
cvar::VaR(qnorm, p_loss = c(0.01, 0.05))


###################################################
### code chunk number 4: Guide_cvar.Rnw:220-222
###################################################
muA <- 0.006408553
sigma2A <- 0.0004018977


###################################################
### code chunk number 5: Guide_cvar.Rnw:225-226
###################################################
res1 <- cvar::VaR(qnorm, p_loss = 0.05, mean = muA, sd = sqrt(sigma2A))


###################################################
### code chunk number 6: Guide_cvar.Rnw:231-233
###################################################
res2 <- cvar::VaR(qnorm, p_loss = 0.05, intercept = muA, slope = sqrt(sigma2A))
abs((res2 - res1)) # 0, intercept/slope equivalent to mean/sd


###################################################
### code chunk number 7: Guide_cvar.Rnw:237-244
###################################################
## with cdf the precision depends on solving an equation
res1a <- cvar::VaR(pnorm, p_loss = 0.05, dist.type = "cdf",
                   mean = muA, sd = sqrt(sigma2A))
res2a <- cvar::VaR(pnorm, p_loss = 0.05, dist.type = "cdf",
                   intercept = muA, slope = sqrt(sigma2A))
abs((res1a - res2)) # 3.287939e-09
abs((res2a - res2)) # 5.331195e-11, intercept/slope better numerically


###################################################
### code chunk number 8: Guide_cvar.Rnw:250-257
###################################################
## as above, but increase the precision, this is probably excessive
res1b <- cvar::VaR(pnorm, p_loss = 0.05, dist.type = "cdf",
                   mean = muA, sd = sqrt(sigma2A), tol = .Machine$double.eps^0.75)
res2b <- cvar::VaR(pnorm, p_loss = 0.05, dist.type = "cdf",
                   intercept = muA, slope = sqrt(sigma2A), tol = .Machine$double.eps^0.75)
abs((res1b - res2)) # 6.938894e-18 # both within machine precision
abs((res2b - res2)) # 1.040834e-16


###################################################
### code chunk number 9: Guide_cvar.Rnw:261-263
###################################################
abs((res1b - res2)/res2) # 2.6119e-16 # both within machine precision
abs((res2b - res2)/res2) # 3.91785e-15


###################################################
### code chunk number 10: Guide_cvar.Rnw:272-277
###################################################
## if(require("PerformanceAnalytics")){
data(edhec, package = "PerformanceAnalytics")
mu <- apply(edhec, 2, mean)
sigma2 <- apply(edhec, 2, var)
musigma2 <- cbind(mu, sigma2)


###################################################
### code chunk number 11: Guide_cvar.Rnw:281-285
###################################################
## analogous calc. with PerformanceAnalytics::VaR
vPA <- apply(musigma2, 1, function(x)
    PerformanceAnalytics::VaR(p = .95, method = "gaussian", invert = FALSE,
                              mu = x[1], sigma = x[2], weights = 1))


###################################################
### code chunk number 12: Guide_cvar.Rnw:291-295
###################################################
vAz1 <- cvar::VaR(qnorm, p_loss = 0.05, mean = mu, sd = sqrt(sigma2))
vAz2 <- cvar::VaR(qnorm, p_loss = 0.05, intercept = mu, slope = sqrt(sigma2))
max(abs((vPA - vAz1))) # 5.551115e-17
max(abs((vPA - vAz2))) #   ""


###################################################
### code chunk number 13: Guide_cvar.Rnw:299-305
###################################################
vAz1a <- cvar::VaR(pnorm, p_loss = 0.05, dist.type = "cdf",
                   mean = mu, sd = sqrt(sigma2))
vAz2a <- cvar::VaR(pnorm, p_loss = 0.05, dist.type = "cdf",
                   intercept = mu, slope = sqrt(sigma2))
max(abs((vPA - vAz1a))) # 3.287941e-09
max(abs((vPA - vAz2a))) #  1.465251e-10, intercept/slope better


###################################################
### code chunk number 14: Guide_cvar.Rnw:309-317
###################################################
vAz1b <- cvar::VaR(pnorm, p_loss = 0.05, dist.type = "cdf",
                   mean = mu, sd = sqrt(sigma2),
                   tol = .Machine$double.eps^0.75)
vAz2b <- cvar::VaR(pnorm, p_loss = 0.05, dist.type = "cdf",
                   intercept = mu, slope = sqrt(sigma2),
                   tol = .Machine$double.eps^0.75)
max(abs((vPA - vAz1b))) # 4.374869e-13
max(abs((vPA - vAz2b))) # 3.330669e-16


###################################################
### code chunk number 15: Guide_cvar.Rnw:329-332
###################################################
cvar::ES(qnorm, p_loss = 0.05, dist.type = "qf")
cvar::ES(qnorm, p_loss = 0.05)
cvar::ES(qnorm)


###################################################
### code chunk number 16: Guide_cvar.Rnw:336-337
###################################################
cvar::ES(qnorm, p_loss = c(0.01, 0.05))


###################################################
### code chunk number 17: Guide_cvar.Rnw:341-343
###################################################
muA <- 0.006408553
sigma2A <- 0.0004018977


###################################################
### code chunk number 18: Guide_cvar.Rnw:349-352
###################################################
res1 <- cvar::ES(qnorm, p_loss = 0.05, mean = muA, sd = sqrt(sigma2A))
res2 <- cvar::ES(qnorm, p_loss = 0.05, intercept = muA, slope = sqrt(sigma2A))
abs((res2 - res1)) 


###################################################
### code chunk number 19: Guide_cvar.Rnw:356-363
###################################################
## with cdf the precision depends on solving an equation
res1a <- cvar::ES(pnorm, p_loss = 0.05, dist.type = "cdf",
                   mean = muA, sd = sqrt(sigma2A))
res2a <- cvar::ES(pnorm, p_loss = 0.05, dist.type = "cdf",
                   intercept = muA, slope = sqrt(sigma2A))
abs((res1a - res2)) # 
abs((res2a - res2)) # intercept/slope better numerically


###################################################
### code chunk number 20: Guide_cvar.Rnw:368-375
###################################################
## as above, but increase the precision, this is probably excessive
res1b <- cvar::ES(pnorm, p_loss = 0.05, dist.type = "cdf",
                   mean = muA, sd = sqrt(sigma2A), tol = .Machine$double.eps^0.75)
res2b <- cvar::ES(pnorm, p_loss = 0.05, dist.type = "cdf",
                   intercept = muA, slope = sqrt(sigma2A), tol = .Machine$double.eps^0.75)
abs((res1b - res2))
abs((res2b - res2))


###################################################
### code chunk number 21: Guide_cvar.Rnw:379-381
###################################################
abs((res1b - res2)/res2)
abs((res2b - res2)/res2)


