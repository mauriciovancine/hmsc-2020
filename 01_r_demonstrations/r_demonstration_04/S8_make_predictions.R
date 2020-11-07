#setwd("") # set directory to the folder where the folders "data", "models" and "panels" are
library(Hmsc)
library(ggplot2)

localDir = "."

nChains = 4
samples = 5
thin = 1
filename = paste("models/models_thin_", as.character(thin),
                 "_samples_", as.character(samples),
                 "_chains_",as.character(nChains),
                 ".Rdata",sep = "")
load(filename)
nm = length(models)

filename =  paste("panels/predictions.pdf")
pdf(file = filename)
for(j in 1:nm){
  m = models[[j]]
  covariates = all.vars(m$XFormula)
  ex.sp = which.max(colMeans(m$Y,na.rm = TRUE)) #most common species as example species
  if(m$distr[1,1]==2){
    ex.sp = which.min(abs(colMeans(m$Y,na.rm = TRUE)-0.5)) #for probit models the species with prevalence closest to 0.5
  }
  for(k in 1:(length(covariates))){
    covariate = covariates[[k]]
    Gradient = constructGradient(m,focalVariable = covariate)
    Gradient2 = constructGradient(m,focalVariable = covariate,non.focalVariables = 1)
    predY = predict(m, Gradient=Gradient, expected = TRUE)  
    predY2 = predict(m, Gradient=Gradient2, expected = TRUE)  
    par(mfrow=c(2,1))
    pl = plotGradient(m, Gradient, pred=predY, yshow = 0, measure="S", showData = TRUE, 
                      main = paste0(modelnames[[j]],": summed response (total effect)"))
    if(inherits(pl, "ggplot")){
      print(pl + labs(title=paste0(modelnames[[j]],": summed response (total effect)")))
    }
    pl = plotGradient(m, Gradient2, pred=predY2, yshow = 0, measure="S", showData = TRUE, 
                      main = paste0(modelnames[[j]],": summed response (marginal effect)"))
    if(inherits(pl, "ggplot")){
      print(pl + labs(title=paste0(modelnames[[j]],": summed response (marginal effect)")))
    }
    par(mfrow=c(2,1))
    pl = plotGradient(m, Gradient, pred=predY, yshow = if(m$distr[1,1]==2){c(-0.1,1.1)}else{0}, measure="Y",index=ex.sp, showData = TRUE, 
                      main = paste0(modelnames[[j]],": example species (total effect)"))
    if(inherits(pl, "ggplot")){
      print(pl + labs(title=paste0(modelnames[[j]],": example species (total effect)")))
    }
    pl = plotGradient(m, Gradient2, pred=predY2, yshow = if(m$distr[1,1]==2){c(-0.1,1.1)}else{0}, measure="Y",index=ex.sp, showData = TRUE, 
                      main = paste0(modelnames[[j]],": example species (marginal effect)"))
    if(inherits(pl, "ggplot")){
      print(pl + labs(title=paste0(modelnames[[j]],": example species (marginal effect)")))
    }
    if(m$nt>1){
      for(l in 2:m$nt){
        par(mfrow=c(2,1))
        pl = plotGradient(m, Gradient, pred=predY, measure="T",index=l, showData = TRUE,yshow = 0,
                          main = paste0(modelnames[[j]],": community weighted mean trait (total effect)"))
        if(inherits(pl, "ggplot")){
          print(pl + labs(title=paste0(modelnames[[j]],": community weighted mean trait (total effect)")))
        }
        pl = plotGradient(m, Gradient2, pred=predY2, measure="T",index=l, showData = TRUE, yshow = 0,
                          main = paste0(modelnames[[j]],": community weighted mean trait (marginal effect)"))
        if(inherits(pl, "ggplot")){
          print(pl + labs(title=paste0(modelnames[[j]],": community weighted mean trait (marginal effect)")))
        }
      }
    }
  }
}
dev.off()
