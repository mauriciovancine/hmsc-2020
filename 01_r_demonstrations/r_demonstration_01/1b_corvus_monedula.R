setwd("C:/LocalData/ovaskain/all stuff/TEACHING/HMSC/2020 November/R-demonstration-1")
library(Hmsc)
library(abind)
library(ggplot2)
library(gridExtra)

set.seed(1)

#READING, SELECTING AND FORMATTING THE DATA
da = read.csv("bird data\\data.csv")
da = droplevels(subset(da,Year==2014))
da$Habitat = as.factor(da$Habitat)
XData = data.frame(hab=da$Habitat, clim = da$AprMay)
Y = as.matrix(1*(da$Corvus_monedula>0))
colnames(Y) = "Corvus monedula"
xy = as.matrix(cbind(da$x,da$y))

#PLOTTING THE DATA
mapData=data.frame(x=xy[,1],y=xy[,2],O=Y[,1],H=XData$hab, C=XData$clim)
ggplot(data = mapData, aes(x=x, y=y))+geom_point(size=1.5,aes(color=H)) + ggtitle("Habitat") + scale_color_discrete(name="") + theme(legend.position="bottom")
ggplot(data = mapData, aes(x=x, y=y, color=C))+geom_point(size=1.5) + ggtitle("Climate") + scale_color_gradient(low="blue", high="red", name="") + theme(legend.position="bottom")+labs(y="")
ggplot(data = mapData, aes(x=x, y=y, color=O))+geom_point(size=1.5) +  ggtitle("Corvus monedula") + scale_color_gradient(low="blue", high="red", name="")+theme(legend.position="bottom")+labs(y="")

#SETTING UP THE HMSC-MODEL
studyDesign = data.frame(route = as.factor(da$Route))
rownames(xy)=studyDesign[,1]
rL = HmscRandomLevel(sData=xy)
XFormula = ~ hab + poly(clim,degree = 2,raw = TRUE)

m1 = Hmsc(Y=Y, XData = XData, XFormula=XFormula,
          distr="probit", studyDesign=studyDesign,
          ranLevels=list(route=rL))

m2 = Hmsc(Y=Y, XData = XData, XFormula=XFormula,
          distr="probit", studyDesign=studyDesign)

m3 = Hmsc(Y=Y, XData = XData, XFormula=~1,
          distr="probit", studyDesign=studyDesign,
          ranLevels=list(route=rL))

#FITTING THE MODEL
nChains = 4
samples = 250
thin = 10
transient = round(0.5*samples*thin)
m1 = sampleMcmc(m1, thin = thin, samples = samples,
           transient = transient, nChains = nChains)
m2 = sampleMcmc(m2, thin = thin, samples = samples,
                transient = transient, nChains = nChains)
m3 = sampleMcmc(m3, thin = thin, samples = samples,
                transient = transient, nChains = nChains)
models = list(m1,m2,m3)
filename =paste0("bird models/model_chains_",as.character(nChains),
                 "_samples_",as.character(samples),
                 "_thin_",as.character(thin))
save(models,file = filename)

#READING IN THE FITTED MODEL
nChains = 2
samples = 20
thin = 1
filename =paste0("bird models/model_chains_",as.character(nChains),
                 "_samples_",as.character(samples),
                 "_thin_",as.character(thin))
load(filename)
models

#EXAMINING MCMC CONVERGENCE
m = models[[1]]
mpost = convertToCodaObject(m)
effectiveSize(mpost$Beta)
gelman.diag(mpost$Beta,multivariate=FALSE)$psrf
effectiveSize(mpost$Alpha[[1]])
gelman.diag(mpost$Alpha[[1]],multivariate=FALSE)$psrf

#EVALUATING MODEL FIT
m = models[[1]]
preds = computePredictedValues(m)
evaluateModelFit(hM=m, predY=preds)

#COMPUTING VARIANCE PARTITIONING
m = models[[1]]
VP = computeVariancePartitioning(m)
VP$vals

#EXAMINING PARAMETER VALUES
m = models[[1]]
mpost = convertToCodaObject(m)
summary(mpost$Beta, quantiles = c(0.025, 0.5, 0.975))[[2]]
summary(mpost$Alpha[[1]], quantiles = c(0.025, 0.5, 0.975))[[2]]

#PREDICTIONS OVER ENVIRONMENTAL GRADIENTS
m = models[[1]]
Gradient = constructGradient(m,focalVariable = "clim",
                             non.focalVariables = list(hab = 1))
predY = predict(m, Gradient=Gradient,expected = TRUE)
plotGradient(m, Gradient, pred=predY, measure="Y", index = 1, showData = TRUE)

Gradient = constructGradient(m,focalVariable = "hab",
                             non.focalVariables = list(clim = 1))
predY = predict(m, Gradient=Gradient,expected = TRUE)
plotGradient(m, Gradient, pred=predY, measure="Y", index = 1, showData = TRUE)

#PREDICTIONS OVER SPACE
grid = read.csv("bird data\\grid_1000.csv")
grid = droplevels(subset(grid,!(Habitat=="Ma")))
xy.grid = as.matrix(cbind(grid$x,grid$y))
XData.grid = data.frame(hab=as.factor(grid$Habitat), clim=grid$AprMay)

m = models[[1]]
Gradient = prepareGradient(m, XDataNew = XData.grid, sDataNew = list(route=xy.grid))
predY = predict(m, Gradient=Gradient, predictEtaMean = TRUE, expected = TRUE)
length(predY)
length(predY[[1]])
EpredY = apply(abind(predY,along=3),c(1,2),mean)
length(EpredY)

mapData=data.frame(x=xy.grid[,1],y=xy.grid[,2], EpredY,H=XData.grid$hab, C=XData.grid$clim)
ggplot(data = mapData, aes(x=x, y=y, color=H))+geom_point(size=1.5) +
  ggtitle("Habitat") + scale_color_discrete(name="") +
  theme(legend.position="bottom")
ggplot(data = mapData, aes(x=x, y=y, color=C))+geom_point(size=1.5) +
  ggtitle("Climate") + scale_color_gradient(low="blue", high="red", name = "") +
  theme(legend.position="bottom") +labs(y="")
ggplot(data = mapData, aes(x=x, y=y, color=Corvus.monedula))+geom_point(size=1.5) + 
  ggtitle("Corvus monedula")+ scale_color_gradient(low="blue", high="red", name = "") +
  theme(legend.position="bottom")+labs(y="")
