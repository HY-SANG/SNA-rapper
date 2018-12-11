
rm(list = ls(all = TRUE))
library(sna)
library(RSiena)
library(data.table)
#setwd("C:/Users/dplewi3/Dropbox")
#setwd("~/Dropbox")

# here is a short script for analysing the co-evolution of the
# friendship network and drinking behavior for the 50 girls in the
# Teenage Friends and Lifestyle Study data

# we'll need 5 data objects, avialable in the friends_and_behavior .zip folder on Canvas

# read in the adjacency matrices, covariates and dependent behavioral variable

# assuming data are in current working directory, load in
friend.data.w1 = as.matrix(read.table("teen-network-wave1.dat")) # network wave 1
friend.data.w2 = as.matrix(read.table("teen-network-wave2.dat")) # network wave 2
friend.data.w3 = as.matrix(read.table("teen-network-wave3.dat")) # network wave 3
drink = as.matrix(read.table("teen-alcohol-usage.dat")) # behavior

# we can use the sna package to plot the networks and the behavioral variable
# we can use sna's nacf() to get descriptive measures of the similarity of
# "friends" with respect to behavior

# moran's I gives us a measure of the degree to which an attribute is correlated in space
# in this case, space is the proximity of the girls in the network
# a value of -1 means perfect dispersion (no effect of proximity)
# a value of +1 means perfectly correlated with space
# 0 would mean randomly disributed
nacf(network(friend.data.w1), drink[, 1], type = "moran")
nacf(network(friend.data.w2), drink[, 2], type = "moran")
nacf(network(friend.data.w3), drink[, 3], type = "moran")

# check out a plot as well
plot(network(friend.data.w1))
plot(network(friend.data.w2))
plot(network(friend.data.w3))

# tell RSiena that the adjacency matrices are network data and in what order
# they should be treated

# this step creates the network objects as a dependent variable
friendship = sienaDependent(array(c(friend.data.w1, friend.data.w2, friend.data.w3), dim = c(50, 50, 3)))

# also telll RSiena that the variable "drink" should be treated as a dependent variable
# here, we're predicting both structure and behavior
drinking = sienaDependent(drink, type = "behavior")

# Define the data set and obtain the basic effects object
CoEvolutionData = sienaDataCreate(friendship, drinking)
CoEvolutionEffects = getEffects(CoEvolutionData)

# run reports to check that data is properly formatted and to get some basic descriptives

print01Report(CoEvolutionData, modelname = 's50_3_CoEvinit')

# can open this up as a text file to check out siena's descriptives

# now let's set up some effects

# first the structural effects that are not yet included by default:
CoEvolutionEffects = includeEffects(CoEvolutionEffects, transTrip, transRecTrip, cycle3)
CoEvolutionEffects
# now sender, receiver and homophily effects for friendship formation:
CoEvolutionEffects = includeEffects(CoEvolutionEffects, egoX, altX, simX, interaction1 = "drinking")
CoEvolutionEffects
# now the assimilation effect for drinking:
CoEvolutionEffects = includeEffects(CoEvolutionEffects, name = "drinking", avSim, interaction1 = "friendship")
CoEvolutionEffects
# note that you need to additionally specify 'name="drinking"' because
# the default for 'name' is the network variable (here "friendship").

# now create an algorithm object:
# diagonalize specifies how much of the surrounding neighborhood/matrix to use in the calculation
# this value ranges from 0 to 1, lower values are more efficient but higher values are more stable
# this is similar to the difference between ubcf and ibcf in the recommender algorithm from last class
CoEvolutionAlgo = sienaAlgorithmCreate(projname = 'CoEvol_results', diagonalize = 0.2)

# estimate the model:
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects)
CoEvolutionResults

# these t-ratios illustrate how well the model has converged
# if the t-ratios for convergence for the non-fixed effects
# are not satisfactorily small (all less than 0.1 in absolute value),
# run the estimation again:
# we use prev ans to start the algorithm from where it stopped last time
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects, prevAns = CoEvolutionResults)

# test the specific parameter of interest:
? Wald.RSiena
Multipar.RSiena(CoEvolutionResults, 15)

# now let's replace average similarity by total similarity:
CoEvolutionEffects = includeEffects(CoEvolutionEffects, include = FALSE, name = "drinking", avSim, interaction1 = "friendship")
CoEvolutionEffects = includeEffects(CoEvolutionEffects, name = "drinking", totSim, interaction1 = "friendship")
CoEvolutionEffects
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects)
CoEvolutionResults
# if the t-ratios for convergence for the non-fixed effects
# are not satisfactorily small (all less than 0.1 in absolute value),
# run again:
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects, prevAns = CoEvolutionResults)
Multipar.RSiena(CoEvolutionResults, 16)

# replace total similarity by average alter:
CoEvolutionEffects = includeEffects(CoEvolutionEffects, include = FALSE, name = "drinking", totSim, interaction1 = "friendship")
CoEvolutionEffects = includeEffects(CoEvolutionEffects, name = "drinking", avAlt, interaction1 = "friendship")
CoEvolutionEffects
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects)
CoEvolutionResults
# if the t-ratios for convergence for the non-fixed effects
# are not satisfactorily small (all less than 0.1 in absolute value),
# run the estimation again:
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects, prevAns = CoEvolutionResults)
Multipar.RSiena(CoEvolutionResults, 15)

# what could be called "total alter" is not implemented directly,
# but can be obtained as the interaction between average alter and outdegree.
# we need to include the main effects in the model, but keep their
# parameters fixed at 0.

? includeInteraction
CoEvolutionEffects = setEffect(CoEvolutionEffects, avAlt, fix = TRUE, test = TRUE, name = "drinking", interaction1 = "friendship")
CoEvolutionEffects = setEffect(CoEvolutionEffects, outdeg, fix = TRUE, test = TRUE, name = "drinking", interaction1 = "friendship")
CoEvolutionEffects = includeInteraction(CoEvolutionEffects, name = "drinking", avAlt, outdeg, interaction1 = c("friendship", "friendship"))
CoEvolutionEffects
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects)
CoEvolutionResults
# if the t-ratios for convergence for the non-fixed effects
# are not satisfactorily small (all less than 0.1 in absolute value),
# run the estimation again:
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects, prevAns = CoEvolutionResults)
CoEvolutionResults
Multipar.RSiena(CoEvolutionResults, 19)

# to see the results of the score-type tests for
# the indegree and outdegree effects on drinking:
CoEvolutionEffects = setEffect(CoEvolutionEffects, indeg, fix = TRUE, test = TRUE, name = "drinking", interaction1 = "friendship")
CoEvolutionEffects
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects, prevAns = CoEvolutionResults)
Multipar.RSiena(CoEvolutionResults, 20)

# The test for the outdegree effect comes close to significance,
# the other two are not significant.

# we can conclude at this point that for all four specifications of
# social influence there appears a positive estimated effect,
# but the significance depends on the specification
# this data set is too small to allow simultaneous estimation
# of all four influence effects at the same time
# the best procedure is to make a choice before seeing the data,
# based on theory or prior experience with similar data sets
# the choice can also be made based on the fit of the model,
# if results from sienaGOF() differentiate between the models
# since the p-values in all four models are between 0.05 and 0.10
# (this will also depend on the random simulations;
# results will be more stable with n3=3000 in sienaAlgorithmCreate)
# we can conclude that there is a tendency toward evidence
# for social influence

# you can get a nicer presentation of the results in a file
# in your working directory in LaTeX with
siena.table(CoEvolutionResults) # .tex file

# and in html (can be imported into MS-Word) with
siena.table(CoEvolutionResults, type = "html") #html

