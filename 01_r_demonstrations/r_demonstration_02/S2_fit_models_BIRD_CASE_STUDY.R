# THIS SCRIPT CONSTRUCTS AND FITS HMSC MODELS FOR THE BIRD EXAMPLE (SECTION 11.1) OF THE BOOK
# Ovaskainen, O. and Abrego, N. 2020. Joint Species Distribution Modelling - With Applications in R. Cambridge University Press.

# We first set the working directory to the directory that has all the files for this case study
# We recommend that you create always the subdirectories "data" and "models" to the main directory
# If running the script in another computer, all that needs to be changed in the script below is the main directory

wd = "C:/LocalData/ovaskain/all stuff/TEACHING/HMSC/2020 ISEC/Section_11_1_birds"
setwd(wd)

localDir = "."
data.directory = file.path(localDir, "data")
model.directory = file.path(localDir, "models")

# We load the Hmsc package, and set the random seed, so that all results from this script are reproducable

library(Hmsc)
set.seed(1)

# We read in the data, set the factors to factors, and have a look at it

da = read.csv(file.path(data.directory, "data.csv"), stringsAsFactors=TRUE)
da$Route = as.factor(da$Route)
head(da)

# We next pre-processing the data to make a number of simplifing choices.
# First, we select to use data from the year 2014 only even if the original data would include repeated surveys to the same locations.

da = droplevels(subset(da,Year==2014))

# Second, we selected to include as candidate environmental covariates only habitat type and spring temperature,
# even if also many other environmental variables could be expected to influence the commmunity.

XData = data.frame(Route = da$Route, hab=da$Habitat, clim = da$AprMay)

# Third, we truncate the species data to presence-absence, and thus ignore abundance variation.

Y = as.matrix(da[,-c(1:9)])>0

# Even if the model definition HMSC accepts logical values (TRUE/FALSE) as the response matrix Y for presence-absence data,
# some of the post-processing functions assume that Y has numerical 0/1 values. So it is safer to 
# convert Y to numerical either as Y = 1*Y or as

Y = apply(Y,MARGIN = 2,FUN = as.numeric)

# While a more comprehensive analyses of these data might explore also other options,
# we have made these choices to keep our analyses simple and thus to be able to focus on the key points we wish to illustrate.

# The data contains also the x- and y-coordinates of the routes. We store these as the xy-matrix to be able to fit a spatial model

xy = as.matrix(cbind(da$x,da$y))
rownames(xy)=da$Route
colnames(xy)=c("x-coordinate","y-coordinate")

# We next read the datafile containing species traits, and include in the TrData dataframe data on migratory strategy and body mass

alltraits = read.csv(file.path(data.directory, "traits.csv"), stringsAsFactors = TRUE)
TrData = data.frame(Species = alltraits$Species, Migration=alltraits$Migration, LogMass = log(alltraits$Mass))

# As a final piece of data, we read in the phylogenetic tree of the species. To do so, we use the read.tree function from the package ape

library(ape)
phyloTree <- read.tree(file.path(data.directory, "CTree.tre"))

# The data that we will utilize for the Hmsc model consist of the community data matrix Y,
# the environmental data frame XData,
# the coordinates of the sampling locations xy,
# the species trait data frame TrData,
# and the phylogenetic tree PhyloTree.
# It is always a good idea to eyeball the data. Let us first have a look at the community data.

dim(Y)

# There are data for 137 sampling units and 50 species

head(Y[,c(1,25,50)])

# As we have truncated the data to presence-absences, the data matrix Y consists of zeros (FALSE) and ones (TRUE).
# The species are actually ordered in terms of their prevalence, so that species 1 (Phylloscopus trochilus) is the
# most common one and species 50 (Corvus monedula) is the rarest one in these data.

# We next explore the distribution of species richness S as the row sums, and the distribution species prevalence as column means

S = rowSums(Y)
P = colMeans(Y)
range(S)
range(P)
par(mfrow=c(1,2))
hist(S, xlab = "Species richness (S)")
hist(P, xlab = "Species prevalence (P)",xlim=c(0,1))

# For species richness, the y-axis (Frequency) corresponds to the number of sampling units,
# and the variable in the x-axis (S) is the number of species found from each sampling unit.
# For species prevalence, the y-axis (Frequency) corresponds to the number of species,
# and the variable in the x-axis (P) is the fraction of sampling units in which the species is present.
# The prevalence of the species range from 27% to 99%, reflecting the fact that we have included only the most common species in these data.
# Species richness varies greatly among the sampling units, ranging from 0 to 47.

# Let us then look at the environmental and spatial data

head(XData)

# the following also show the types (factor, num) of variables:
str(XData)
# and a full summary
summary(XData)

# In the environmental data, the sampling units are labelled with the variable
# Route that correponds to the identity number of the permanent survey route.
# The environmental covariates include a classification to habitat types (hab)
# and the mean temperature in April-May (clim) recorded in the year 2014 and
# thus corresponding to the timing of when the community data were collected.

head(xy)
par(mfrow=c(1,1))
plot(xy, asp=1) # show the map (NB., equal aspect ratio in the map)

# The matrix xy contains the the coordinates of the survey routes.
# The identities of the survey routes are given by row names
# Let us then look at the data on species traits and phylogenetic relationships.

head(TrData)

# For each of the 50 species, the data frame includes data on the body mass
# (which is log-transformed to make it suit better as a predictor),
# and their migratory strategy classified as long-distance migrant (L),
# short-distance migrant (S), or resident species (R).

phyloTree
plot(phyloTree,cex = 0.5)

# The phylogenetic tree includes the 50 species as LEAF tips.
# It includes branch lengths which is necessary for its use in the HMSC context.
# The tree can be visualized with the plot function of the ape package.

# We are now ready to define the HMSC model. While the book defines three models (m.FULL, m.ENV, and m.SPACE),
# here we define the full model only. The remaining models are special cases of the FULL model and thus
# the script can be modified easily to fit those alternative models.
# Thus, the model (to be called here simply as m) includes both the environmental covariates as well as the spatial random effect of the route.

# To define a spatial random effect at the level of the route, we need to include the route id:s in the studyDesign

studyDesign = data.frame(Route = XData$Route)

# We next define the random level object. If we would define an unstructured random effect, we would use the
# units = ... argument as we did in the fungal example of Section 7.9. As we wish to define a spatial
# random effect, we use instead the sData argument.

rL = HmscRandomLevel(sData=xy)

# Note that the row names of xy correspond to the units of the studyDesign. This is necessary to make
# Hmsc understand how the units of the random effect (rows of xy) correspond to the sampling units
# (rows of studyDesign, Y and XData). While in this example the dimensions of xy and studyDesign match,
# this is not necessarily the case. For example, if we would have included multiple surveys to the same sites,
# each survey would be a row of studyDesign, but sData (or units in case of unstructured random effect)
# should have each site only once.

# As environmental covariates, we include the habitat type as the categorical variable, and the second order response
# to climatic conditions. We use the poly function to defined the second order response. We further use in that
# raw = TRUE to make the model formulation consistent with the way in which predictions over environmental
# gradients are made in Hmsc.

XFormula = ~ hab + poly(clim,degree = 2,raw = TRUE)

# As trait formula, we include migratory strategy and linear effect of log(mass).

TrFormula = ~Migration + LogMass

# We are now ready to define the model. Note that in ranLevels=list(Route=rL), "Route" refers to a column name
# of the studyDesign

m = Hmsc(Y=Y, XData = XData, XFormula=XFormula,
         phyloTree = phyloTree, 
         TrData = TrData, TrFormula = TrFormula,
         distr="probit", studyDesign=studyDesign,
         ranLevels=list(Route=rL))

# As always, it is a good idea to explore the model object

m

# This should give "Hmsc object with 137 sampling units, 50 species, 7 covariates, 4 traits and 1 random levels"

head(m$X)

# The factor of habitat type has been expanded to dummy variables, and the model includes first and second orders of climate

head(m$XScaled)

# In the computations, scaled versions of climate will be used

head(m$Tr)

# The factor of migratory strategy has been expanded to dummy variables, and the model includes the linear effect of log(mass)

head(m$TrScaled)

# In the computations, scaled versions of log(mass) will be used

# This shows a visualization of the phylogenetic correlation matrix C computed frmo the phylogenetic tree.

library(Matrix)
image(Matrix(m$C))

# Note that the 'white cross' corresponds to species 39
# which is Tetrao_tetrix that in the phyloTree
# does not share any phylogenetic history with the rest of the species

# We will store 100 posterior samples for each of two chains
# We note that for more "final" results, one might wish to have e.g. 1000 samples for each of four chains
# After fitting all models, we save the model object to a file
# Loading the fitted model then serves as the starting point for exploring the results
# The script runs over a loop where thin is first 1, then 10, then 100, and so on
# Thin is the thinning parameter of the MCMC chain.
# The transient (also called burn-in) is set to 50*thin
# When thin = 1, there will be 50 burn-in and 100 actual iterations. All actual iterations are stored.
# When thin = 10, there will be 500 burn-in and 1000 actual iterations. The actual iterations are thinned by 10, so 100 are stored.
# When thin = 100, there will be 5000 burn-in and 10000 actual iterations. The actual iterations are thinned by 100, so 100 are stored.
# A long MCMC chain is needed to achieve convergence
# Thinning is applied to avoid storing model objects of very large size
# Even if in the end thin = 1000 is required to achieve converge, We recommend to run the loop thin = 1, 10, 100, 1000
# This is for several reasons.
# First of all, it will not be known beforehand how much thinning is needed to achieve satisfactory convergence
# Second, thin = 1 will run very fast, whereas thin = 1000 will take very long (1000 times longer)
# After thin = 1 is completed, it is already possible to develop all the remaining scripts that explore the fitted model
# When exploring the fitted model, often one realizes changes that need to be made, even if the fitting has not converged
# Third, running the model fitting for thin = 1, 10, 100, 1000 does not take much longer than running it just for thin = 1000 (it takes ca. 12% longer)
# Thus, in summary, running the model fitting for thin = 1, 10, 100, 1000 typically saves a lot of time,
# as it allows one to proceed fast in writing (and revising) all the scripts that are needed from defining the model to producing the result tables and figures
# The idea is not to run the entire loop in one go, as that would take a lot of time. Just run thin = 1, and then move to develop the next scripts.
# You may then leave the remaining part of the loop (e.g. thin = 10, 100, 1000) to run e.g. overnight

nChains = 1
nParallel = 1
samples = 10
transient = 5
thin = 1
m = sampleMcmc(m, thin = thin, samples = samples, transient = transient,
               nChains = nChains, initPar = "fixed effects",
               nParallel = nParallel)
filename=file.path(model.directory, paste0("model_chains_",as.character(nChains),"_samples_",as.character(samples),"_thin_",as.character(thin)))
save(m,file=filename)

nChains = 2
nParallel = 2 # optional setting of nParallel
samples = 100
for (thin in c(1,10,100,1000))
{
  transient = 50*thin
  m = sampleMcmc(m, thin = thin, samples = samples, transient = transient,
                 nChains = nChains, initPar = "fixed effects",
                 nParallel = nParallel)
  filename=file.path(model.directory, paste0("model_chains_",as.character(nChains),"_samples_",as.character(samples),"_thin_",as.character(thin)))
  save(m,file=filename)
}

# MCMC convergence can be difficult to achieve especially in those models that are not based on normal distribution.
# For this reason, in the script above we initialize model with
# initPar="fixed effects", with which option the MCMC chains are not started from locations randomized from the prior
# but from a maximum likelihood solution to the fixed-effects part of the model
