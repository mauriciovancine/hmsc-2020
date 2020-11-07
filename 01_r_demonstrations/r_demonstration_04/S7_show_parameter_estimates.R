#setwd("") # set directory to the folder where the folders "data", "models" and "panels" are
library(Hmsc)
library(Hmsc)
library(colorspace)
library(corrplot)
library(writexl)

nChains = 4
samples = 5
thin = 1

filename = paste("models/models_thin_", as.character(thin),
                 "_samples_", as.character(samples),
                 "_chains_",as.character(nChains),
                 ".Rdata",sep = "")
load(filename)
nm = length(models)

filename = paste("panels/parameter_estimates.pdf")

pdf(file = filename)
for(j in 1:nm){
  m = models[[j]]
  
  VP = computeVariancePartitioning(m)
  vals = VP$vals
  mycols = rainbow(nrow(VP$vals))
  plotVariancePartitioning(hM=m, VP=VP,cols = mycols, args.leg=list(bg="white",cex=0.7),
                           main = paste0("Proportion of explained variance, ",modelnames[[j]]),cex.main=0.8)
  preds = computePredictedValues(m)
  MF = evaluateModelFit(hM=m, predY=preds)
  
  R2 = NULL
  if(!is.null(MF$TjurR2)){
    TjurR2 = MF$TjurR2
    vals = rbind(vals,TjurR2)
    R2=TjurR2
  }
  if(!is.null(MF$R2)){
    R2=MF$R2
    vals = rbind(vals,R2)
  }
  
  filename =  paste0("panels/parameter_estimates_VP_",modelnames[[j]],".csv")
  write.csv(vals,file=filename)
  
  if(!is.null(R2)){
    VPr = VP
    for(k in 1:m$ns){
      VPr$vals[,k] = R2[k]*VPr$vals[,k]
    }
    
    VPr$vals = VPr$vals[,order(-R2)]
    plotVariancePartitioning(hM=m, VP=VPr,cols = mycols, args.leg=list(bg="white",cex=0.7),ylim=c(0,1),
                             main=paste0("Proportion of raw variance, ",modelnames[[j]]),cex.main=0.8)
  }
  
}

for(j in 1:nm){
  m = models[[j]]
  postBeta = getPostEstimate(m, parName="Beta")
  show.sp.names = (is.null(m$phyloTree) && m$ns<=20) 
  plotBeta(m, post=postBeta, supportLevel = 0.95,param="Sign",
           plotTree = !is.null(m$phyloTree),
           covNamesNumbers = c(TRUE,FALSE),
           spNamesNumbers=c(show.sp.names,FALSE),
           cex=c(0.6,0.6,0.8))
  mymain = paste0("BetaPlot, ",modelnames[[j]])
  if(!is.null(m$phyloTree)){
    mpost = convertToCodaObject(m)
    rhovals = unlist(poolMcmcChains(mpost$Rho))
    mymain = paste0(mymain,", E[rho] = ",round(mean(rhovals),2),", Pr[rho>0] = ",round(mean(rhovals>0),2))
  }
  title(main=mymain, line=2.5, cex.main=0.8)
  
  me = as.data.frame(t(postBeta$mean))
  me = cbind(m$spNames,me)
  colnames(me) = c("Species",m$covNames)
  po = as.data.frame(t(postBeta$support))
  po = cbind(m$spNames,po)
  colnames(po) = c("Species",m$covNames)
  ne = as.data.frame(t(postBeta$supportNeg))
  ne = cbind(m$spNames,ne)
  colnames(ne) = c("Species",m$covNames)
  vals = list("Posterior mean"=me,"Pr(x>0)"=po,"Pr(x<0)"=ne)
  filename = paste0("panels/parameter_estimates_Beta_",modelnames[j],".xlsx")
  writexl::write_xlsx(vals,path = filename)
}

for(j in 1:nm){
  if(m$nt>1){
    m = models[[j]]
    postGamma = getPostEstimate(m, parName="Gamma")
    plotGamma(m, post=postGamma, supportLevel = 0.9, param="Sign",
              covNamesNumbers = c(TRUE,FALSE),
              trNamesNumbers=c(m$nt<21,FALSE),
              cex=c(0.6,0.6,0.8))
    title(main=paste0("GammaPlot ",modelnames[[j]]), line=2.5,cex.main=0.8)
  }
}

for(j in 1:nm){
  m = models[[j]]
  OmegaCor = computeAssociations(m)
  supportLevel = 0.95
  for (r in 1:m$nr){
    plotOrder = corrMatOrder(OmegaCor[[r]]$mean,order="AOE")
    toPlot = ((OmegaCor[[r]]$support>supportLevel) + (OmegaCor[[r]]$support<(1-supportLevel))>0)*sign(OmegaCor[[r]]$mean)
    if(m$ns>20){
      colnames(toPlot)=rep("",m$ns)
      rownames(toPlot)=rep("",m$ns)
    }
    mymain = paste0("Associations, ",modelnames[[j]], ": ",names(m$ranLevels)[[r]])
    if(m$ranLevels[[r]]$sDim>0){
      mpost = convertToCodaObject(m)
      alphavals = unlist(poolMcmcChains(mpost$Alpha[[1]][,1]))
      mymain = paste0(mymain,", E[alpha1] = ",round(mean(alphavals),2),", Pr[alpha1>0] = ",round(mean(alphavals>0),2))
    }
    corrplot(toPlot[plotOrder,plotOrder], method = "color",
             col=colorRampPalette(c("blue","white","red"))(3),
             mar=c(0,0,1,0),
             main=mymain,cex.main=0.8)
    
    me = as.data.frame(OmegaCor[[r]]$mean)
    me = cbind(m$spNames,me)
    colnames(me)[1] = ""
    po = as.data.frame(OmegaCor[[r]]$support)
    po = cbind(m$spNames,po)
    colnames(po)[1] = ""
    ne = as.data.frame(1-OmegaCor[[r]]$support)
    ne = cbind(m$spNames,ne)
    colnames(ne)[1] = ""
    vals = list("Posterior mean"=me,"Pr(x>0)"=po,"Pr(x<0)"=ne)
    filename = paste0("panels/parameter_estimates_Omega_",modelnames[[j]],"_",names(m$ranLevels)[[r]],".xlsx")
    writexl::write_xlsx(vals,path = filename)
  }
}
dev.off()
