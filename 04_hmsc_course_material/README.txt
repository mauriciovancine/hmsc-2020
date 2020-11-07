# MODIFY THIS README FILE TO CORRESPOND TO YOUR DATA
# KEEP IT BRIEF; THIS IS NOT INTENDED TO BE A COMPLETE DESCRIPTION OF YOUR DATA
# BUT JUST A BRIEF OVERVIEW TO GET A QUICK IDEA
# FOLLOW THE STRUCTURE AND STYLE OF THIS FILE AS MUCH AS POSSIBLE

This README.text file includes the basic information about the dataset coded as
surname_firstname #change this line!

# MAIN QUESTION/AIM OF THE STUDY IN ONE SENTENCE:
How the abundance variation of birds depends on species traits, phylogenetic relationships, 
spatial predictors and environmental predictors.

# SPECIES DATA (Y)
The species data consists of counts of 50 bird species.

# STUDY DESIGN (S)
The study design contains repeated visits to routes.
The route id is in the column "Route" and its coordinates are in columns "Route_x" and "Route_y".
The year of visit is in the column "Year".

# COVARIATES (X)
Effort (continuous): length of transect line
Habitat (factor): habitat type
JunJul (continuous): climatic variable
DJF (continuous): climatic variable
AprMay (continuous): climatic variable

# HOW TO SET UP A REASONABLE PILOT MODEL FOR XFormula:
The three (or less than three) most important predictors are: Effort, Habitat, AprMay
Transformations suggested for continuous variables:
-log for Effort
-Include also second order term of AprMay

# TRAITS (Tr)
Migration (factor): migratory strategy
Mass (factor): body mass

# HOW TO SET UP A REASONABLE PILOT MODEL FOR TrFormula:
The three (or less than three) most important traits are: Migration, Mass
Transformations suggested for continuous variables:
-log for Mass

# PHYLOGENY (P)
The data includes a phylogenetic tree

# CAN WE USE YOUR DATA AS EXAMPLE IN THE COURSE: YES # change to NO as needed
Using the data as example means that we would use it to demonstrate how HMSC models are fitted to these data. 
This would involve showing how HMSC is fitted to your data and what kind of results come out.
Note that we will not share your data with anyone outside the course instructors,
so even if we you allow us to use your data for demonstration purposes, the other participants
will not have access to it. 

# DO YOU POTENTIALLY WISH TO COLLABORATE WITH YOUR DATA ANALYSES AT THE LEVEL OF CO-AUTHORSHIP: YES # change to "NO" or "MAYBE" as needed
The default plan is that we will advise you during the course with your data analyses,
including setting up and fitting the models, interpreting the results, and writing the paper.
If you find our advice useful, we would like you to mention that in the acknowledgements. 

If you wish, we may also be able to take a more active role in your manuscript preparation,
so that a co-authorship becomes justified. If you answer "YES", this means that in principle
you would welcome such a contribution. Of course, whether an instructor from the course eventually
becomes a co-author or not will depend on their actual level of contribution to the manuscript.
