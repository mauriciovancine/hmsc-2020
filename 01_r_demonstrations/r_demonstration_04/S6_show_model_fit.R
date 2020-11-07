#setwd("") # set directory to the folder where the folders "data", "models" and "panels" are
library(Hmsc)

#This script shows model fit for probit and linear models.
#For Poisson models, you may plot e.g. pseudoR2, see e.g. the book for examples

thin = 1
samples = 5
nChains = 4

filename = paste("models/MF_thin_", as.character(thin),
                 "_samples_", as.character(samples),
                 "_chains_",as.character(nChains),
                 ".Rdata",sep = "")
load(filename)
nm = length(MF)
filename = paste("panels/model_fit.pdf")
pdf(file = filename)
for(j in 1:nm){
  cMF = MF[[j]]
  cMFCV = MFCV[[j]]
  if(!is.null(cMF$TjurR2)){
    plot(cMF$TjurR2,cMFCV$TjurR2,xlim=c(-1,1),ylim=c(-1,1),
         xlab = "explanatory power",
         ylab = "predictive power",
         main=paste0(modelnames[[j]],", thin = ",as.character(thin),", samples = ",as.character(samples),": Tjur R2"))
    abline(0,1)
    abline(v=0)
    abline(h=0)
  }
  if(!is.null(cMF$R2)){
    plot(cMF$R2,cMFCV$R2,xlim=c(-1,1),ylim=c(-1,1),
         xlab = "explanatory power",
         ylab = "predictive power",
         main=paste0(modelnames[[j]],", thin = ",as.character(thin),", samples = ",as.character(samples),": R2"))
    abline(0,1)
    abline(v=0)
    abline(h=0)
  }
  if(!is.null(cMF$AUC)){
    plot(cMF$AUC,cMFCV$AUC,xlim=c(0,1),ylim=c(0,1),
         xlab = "explanatory power",
         ylab = "predictive power",
         main=paste0(modelnames[[j]],", thin = ",as.character(thin),", samples = ",as.character(samples),": AUC"))
    abline(0,1)
    abline(v=0.5)
    abline(h=0.5)
  }
}
dev.off()

