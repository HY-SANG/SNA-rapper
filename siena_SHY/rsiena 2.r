
library(sna)
library(igraph)
library(RSiena)
library(data.table)

dtTop <- fread('dtTop.csv')[, -1]
dt2015 <- fread('dt2015.csv')[, -1]
dt2016 <- fread('dt2016.csv')[, -1]
dt2017 <- fread('dt2017.csv')[, -1]

mat2015 <- as.matrix(as_adj(graph.edgelist(as.matrix(dt2015[, c('target', 'source')]))))
mat2016 <- as.matrix(as_adj(graph.edgelist(as.matrix(dt2016[, c('target', 'source')]))))
mat2017 <- as.matrix(as_adj(graph.edgelist(as.matrix(dt2017[, c('target', 'source')]))))

mat2015 <- ifelse(mat2015 > 0, 1, 0)
mat2016 <- ifelse(mat2016 > 0, 1, 0)
mat2017 <- ifelse(mat2017 > 0, 1, 0)



# this step creates the network objects as a dependent variable
sdCoop = sienaDependent(array(c(mat2015, mat2016, mat2017), dim = c(71, 71, 3)))

# also telll RSiena that the variable "sdTop" should be treated as a dependent variable
# here, we're predicting both structure and behavior
sdTop = sienaDependent(as.matrix(dtTop[, -1]), type = "behavior")

# Define the data set and obtain the basic effects object
CoEvolutionData = sienaDataCreate(sdCoop, sdTop)
CoEvolutionEffects = getEffects(CoEvolutionData)
#print01Report(CoEvolutionData, modelname = 'rapper_CoEvinit')
# can open this up as a text file to check out siena's descriptives

## now let's set up some effects
## first the structural effects that are not yet included by default:
CoEvolutionEffects = includeEffects(CoEvolutionEffects, transTrip, transRecTrip, cycle3)
CoEvolutionEffects

# There is no effect with short name transTrip, transRecTrip, cycle3, and with interaction1 = < >, interaction2 = < >, and type = < eval >, for dependent variable sdCoop .



# now sender, receiver and homophily effects for sdCoop formation:
#CoEvolutionEffects = includeEffects(CoEvolutionEffects, egoX, altX, simX, interaction1 = "sdTop")
#CoEvolutionEffects

# no effect as this is mutual network

# now the assimilation effect for sdTop:
CoEvolutionEffects = includeEffects(CoEvolutionEffects, name = "sdTop", avSim, interaction1 = "sdCoop")
CoEvolutionEffects
# note that you need to additionally specify 'name="sdTop"' because the default for 'name' is the network variable (here "sdCoop").



# now create an algorithm object:
# diagonalize specifies how much of the surrounding neighborhood/matrix to use in the calculation
# this value ranges from 0 to 1, lower values are more efficient but higher values are more stable
# this is similar to the difference between ubcf and ibcf in the recommender algorithm from last class
CoEvolutionAlgo = sienaAlgorithmCreate(projname = 'CoEvol_results', diagonalize = 1) # we are using 1 for most stable result
# estimate the model:
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects)
CoEvolutionResults

# these t-ratios illustrate how well the model has converged
# if the t-ratios for convergence for the non-fixed effects are not satisfactorily small (all less than 0.1 in absolute value), 
# run the estimation again: we use prevAns to start the algorithm from where it stopped last time
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects, prevAns = CoEvolutionResults)
CoEvolutionResults



# test the specific parameter of interest:
? Wald.RSiena
Multipar.RSiena(CoEvolutionResults, 11)



# now let's replace average similarity by total similarity:
CoEvolutionEffects = includeEffects(CoEvolutionEffects, include = FALSE, name = "sdTop", avSim, interaction1 = "sdCoop")
CoEvolutionEffects = includeEffects(CoEvolutionEffects, name = "sdTop", totSim, interaction1 = "sdCoop")
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects)
CoEvolutionResults
# if the t-ratios for convergence for the non-fixed effects are not satisfactorily small (all less than 0.1 in absolute value), run again:
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects, prevAns = CoEvolutionResults)
Multipar.RSiena(CoEvolutionResults, 11)



# replace total similarity by average alter:
CoEvolutionEffects = includeEffects(CoEvolutionEffects, include = FALSE, name = "sdTop", totSim, interaction1 = "sdCoop")
CoEvolutionEffects = includeEffects(CoEvolutionEffects, name = "sdTop", avAlt, interaction1 = "sdCoop")
CoEvolutionEffects
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects)
CoEvolutionResults
# if the t-ratios for convergence for the non-fixed effects are not satisfactorily small (all less than 0.1 in absolute value), run the estimation again:
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects, prevAns = CoEvolutionResults)
CoEvolutionResults
Multipar.RSiena(CoEvolutionResults, 11)



# what could be called "total alter" is not implemented directly, but can be obtained as the interaction between average alter and outdegree.
# we need to include the main effects in the model, but keep their parameters fixed at 0.
? includeInteraction
CoEvolutionEffects = setEffect(CoEvolutionEffects, avAlt, fix = TRUE, test = TRUE, name = "sdTop", interaction1 = "sdCoop")
CoEvolutionEffects = setEffect(CoEvolutionEffects, outdeg, fix = TRUE, test = TRUE, name = "sdTop", interaction1 = "sdCoop")
CoEvolutionEffects = includeInteraction(CoEvolutionEffects, name = "sdTop", avAlt, outdeg, interaction1 = c("sdCoop", "sdCoop"))
CoEvolutionEffects
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects)
CoEvolutionResults
# if the t-ratios for convergence for the non-fixed effects are not satisfactorily small (all less than 0.1 in absolute value), run the estimation again:
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects, prevAns = CoEvolutionResults)
CoEvolutionResults
#Multipar.RSiena(CoEvolutionResults, 19)



## to see the results of the score-type tests for the indegree and outdegree effects on sdTop:
#CoEvolutionEffects = setEffect(CoEvolutionEffects, indeg, fix = TRUE, test = TRUE, name = "sdTop", interaction1 = "sdCoop")
#CoEvolutionEffects
#CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects, prevAns = CoEvolutionResults)
#CoEvolutionResults
#Multipar.RSiena(CoEvolutionResults,14)
# there is no indegree and outdegree effects on sdTop, because the network is mutual.



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
