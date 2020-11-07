#LET US SIMULATE SOME DATA FROM THE LINEAR MODEL
set.seed(1)
n = 50
x = rnorm(n)
beta1 = 0
beta2 = 1
L = beta1 + beta2*x
y = L + rnorm(n, sd = 1)
plot(x,y)

#FITTING THE BASIC LINEAR MODEL
basic.lm = lm(y ~ x)
abline(basic.lm)
summary(basic.lm)

#FITTING THE LINEAR MODEL WITH Hmsc
library(Hmsc)
Y = as.matrix(y)
XData = data.frame(x = x)

hmsc.lm = Hmsc(Y = Y, XData = XData, XFormula = ~x)

hmsc.lm = sampleMcmc(hmsc.lm, thin = 1, samples = 1000, 
                     transient = 500, nChains = 2)

mpost = convertToCodaObject(hmsc.lm)
plot(mpost$Beta)
effectiveSize(mpost$Beta)
gelman.diag(mpost$Beta,multivariate=FALSE)$psrf

summary(mpost$Beta)
summary(basic.lm)

preds = computePredictedValues(hmsc.lm)
MF = evaluateModelFit(hM=hmsc.lm, predY=preds)
MF$R2

#THIS IS HOW ONE COULD FIT E.G. A PROBIT MODEL
#m.probit = Hmsc(Y=Y, XData=XData, XFormula=~x, distr="probit")













# 
set.seed(1)
n <- 50
x <- rnorm(n)
beta1 <- 0
beta2 <- 1
L <- beta1 + beta2*x
y <- L + rnorm(n, sd = 1)
plot(x, y, pch  = 20, cex = 2)

# 
basic.lm <- lm(y ~ x)
abline(basic.lm, col = "red", lwd = 5)
summary(basic.lm)

# 
library(Hmsc)
Y <- as.matrix(y)
XData <- data.frame(x = x)
hmsc.lm <- Hmsc(Y = Y, XData = XData, XFormula = ~ x)
hmsc.lm

hmsc.lm <- sampleMcmc(hmsc.lm, 
                      thin = 1, 
                      samples = 1000,
                      transient = 500, 
                      nChains = 2)
hmsc.lm

mpost <- convertToCodaObject(hmsc.lm)
plot(mpost$Beta)
effectiveSize(mpost$Beta)
gelman.diag(mpost$Beta, multivariate = FALSE)$psrf

summary(mpost$Beta)
summary(basic.lm)

preds <- computePredictedValues(hmsc.lm)
preds
MF <- evaluateModelFit(hM = hmsc.lm, predY = preds)
MF

MF$RMSE # sqrt se
MF$R2 # r2

# probit
# m.probit <- 
