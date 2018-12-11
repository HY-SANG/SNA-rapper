
library(sna)
library(RSiena)
library(data.table)

dt2015 <- fread('2015.csv')[, -1]
dtNew <- dt2015
colnames(dtNew)[2] <- 'target'
colnames(dtNew)[3] <- 'source'
dt2015 <- rbind(dt2015, dtNew)

dt2016 <- fread('2016.csv')[, -1]
dtNew <- dt2016
colnames(dtNew)[2] <- 'target'
colnames(dtNew)[3] <- 'source'
dt2016 <- rbind(dt2016, dtNew)

dt2017 <- fread('2017.csv')[, -1]
dtNew <- dt2017
colnames(dtNew)[2] <- 'target'
colnames(dtNew)[3] <- 'source'
dt2017 <- rbind(dt2017, dtNew)

#dt2018 <- fread('2018.csv')[, -1]
#dtNew <- dt2018
#colnames(dtNew)[2] <- 'target'
#colnames(dtNew)[3] <- 'source'
#dt2018 <- rbind(dt2018, dtNew)



v2015 <- sort(unique(unlist(dt2015[, c('source')])))
v2016 <- sort(unique(unlist(dt2016[, c('source')])))
v2017 <- sort(unique(unlist(dt2017[, c('source')])))
#v2018 <- sort(unique(unlist(dt2018[, c('source')])))

vArtist <- intersect(v2015, v2016)
vArtist <- intersect(vArtist, v2017)
vArtist <- intersect(vArtist, v2016)
vArtist <- intersect(vArtist, v2015)
#vArtist <- intersect(vArtist, v2018)

#write.csv(vArtist, 'artist.csv')
#vArtist <- fread('artist.csv')[-1, -1]
#vArtist <- split(vArtist, seq(nrow(vArtist))) # backup, notice there are 71 in common

dt2015 <- dt2015[dt2015[, source %in% vArtist & target %in% vArtist],]
dt2016 <- dt2016[dt2016[, source %in% vArtist & target %in% vArtist],]
dt2017 <- dt2017[dt2017[, source %in% vArtist & target %in% vArtist],]
#dt2018 <- dt2018[dt2018[, source %in% vArtist & target %in% vArtist],]

dtArtist <- data.table(vArtist)

dt <- sort(unique(dt2015[, c('source', 'top.x')]))
dt[, year_2015 := sum(top.x), by = 'source']
y2015 <- sort(unique(dt[, c('source', 'year_2015')]))

dt <- sort(unique(dt2016[, c('source', 'top.x')]))
dt[, year_2016 := sum(top.x), by = 'source']
y2016 <- sort(unique(dt[, c('source', 'year_2016')]))

dt <- sort(unique(dt2017[, c('source', 'top.x')]))
dt[, year_2017 := sum(top.x), by = 'source']
y2017 <- sort(unique(dt[, c('source', 'year_2017')]))

#dt <- sort(unique(dt2018[, c('source', 'top.x')]))
#dt[, year_2018 := sum(top.x), by = 'source']
#v2018 <- sort(unique(dt[, c('source', 'year_2018')]))

dtTop <- merge(y2015, y2016, by = 'source')
dtTop <- merge(dtTop, y2017, by = 'source')
#dtTop <- merge(dtTop, v2018, by = 'source')

#write.csv(dtTop, 'dtTop.csv')
#dtTop <- fread('dtTop.csv')[, -1]

net2015 <- network(dt2015[, c('source', 'target')])
net2016 <- network(dt2016[, c('source', 'target')])
net2017 <- network(dt2017[, c('source', 'target')])
#net2018 <- network(dt2018[, c('source', 'target')])



nacf(net2015, dtTop[, year_2015], type = "moran")
nacf(net2016, dtTop[, year_2016], type = "moran")
nacf(net2017, dtTop[, year_2017], type = "moran")
#nacf(net2018, dtTop[, year_2018], type = "moran")

plot(net2015)
plot(net2016)
plot(net2017)


#write.csv(dt2015, 'dt2015.csv')
#write.csv(dt2016, 'dt2016.csv')
#write.csv(dt2017, 'dt2017.csv')

dt2015 <- fread('dt2015.csv')[, -1]
dt2016 <- fread('dt2016.csv')[, -1]
dt2017 <- fread('dt2017.csv')[, -1]

library(igraph)

g2015 <- graph.edgelist(as.matrix(dt2015[, c('target', 'source')]))
g2016 <- graph.edgelist(as.matrix(dt2016[, c('target', 'source')]))
g2017 <- graph.edgelist(as.matrix(dt2017[, c('target', 'source')]))



mat2015 <- as.matrix(as_adj(g2015))
mat2016 <- as.matrix(as_adj(g2016))
mat2017 <- as.matrix(as_adj(g2017))

mat2015 <- ifelse(mat2015 > 0, 1, 0)
mat2016 <- ifelse(mat2016 > 0, 1, 0)
mat2017 <- ifelse(mat2017 > 0, 1, 0)



# this step creates the network objects as a dependent variable
coop = sienaDependent(array(c(mat2015, mat2016, mat2017), dim = c(71, 71, 3)))

# also telll RSiena that the variable "top" should be treated as a dependent variable
# here, we're predicting both structure and behavior
top = sienaDependent(as.matrix(dtTop[, -1]), type = "behavior")

# Define the data set and obtain the basic effects object
CoEvolutionData = sienaDataCreate(coop, top)
CoEvolutionEffects = getEffects(CoEvolutionData)
print01Report(CoEvolutionData, modelname = 'rapper_CoEvinit')
# can open this up as a text file to check out siena's descriptives

# now let's set up some effects
# first the structural effects that are not yet included by default:
CoEvolutionEffects = includeEffects(CoEvolutionEffects, transTrip, transRecTrip, cycle3)
CoEvolutionEffects
# now sender, receiver and homophily effects for coop formation:
CoEvolutionEffects = includeEffects(CoEvolutionEffects, egoX, altX, simX, interaction1 = "top")
CoEvolutionEffects
# now the assimilation effect for top:
CoEvolutionEffects = includeEffects(CoEvolutionEffects, name = "top", avSim, interaction1 = "coop")
CoEvolutionEffects
# note that you need to additionally specify 'name="top"' because
# the default for 'name' is the network variable (here "coop").



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
CoEvolutionResults



# test the specific parameter of interest:
? Wald.RSiena
Multipar.RSiena(CoEvolutionResults, 11)

# now let's replace average similarity by total similarity:
CoEvolutionEffects = includeEffects(CoEvolutionEffects, include = FALSE, name = "top", avSim, interaction1 = "coop")
CoEvolutionEffects = includeEffects(CoEvolutionEffects, name = "top", totSim, interaction1 = "coop")
CoEvolutionEffects
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects)
CoEvolutionResults
# if the t-ratios for convergence for the non-fixed effects
# are not satisfactorily small (all less than 0.1 in absolute value),
# run again:
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects, prevAns = CoEvolutionResults)
Multipar.RSiena(CoEvolutionResults, 11)

# replace total similarity by average alter:
CoEvolutionEffects = includeEffects(CoEvolutionEffects, include = FALSE, name = "top", totSim, interaction1 = "coop")
CoEvolutionEffects = includeEffects(CoEvolutionEffects, name = "top", avAlt, interaction1 = "coop")
CoEvolutionEffects
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects)
CoEvolutionResults
# if the t-ratios for convergence for the non-fixed effects
# are not satisfactorily small (all less than 0.1 in absolute value),
# run the estimation again:
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects, prevAns = CoEvolutionResults)
Multipar.RSiena(CoEvolutionResults, 11)



# ----
# what could be called "total alter" is not implemented directly,
# but can be obtained as the interaction between average alter and outdegree.
# we need to include the main effects in the model, but keep their
# parameters fixed at 0.

? includeInteraction
CoEvolutionEffects = setEffect(CoEvolutionEffects, avAlt, fix = TRUE, test = TRUE, name = "top", interaction1 = "coop")
CoEvolutionEffects = setEffect(CoEvolutionEffects, outdeg, fix = TRUE, test = TRUE, name = "top", interaction1 = "coop")
CoEvolutionEffects = includeInteraction(CoEvolutionEffects, name = "top", avAlt, outdeg, interaction1 = c("coop", "coop"))
CoEvolutionEffects
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects)
CoEvolutionResults
# if the t-ratios for convergence for the non-fixed effects
# are not satisfactorily small (all less than 0.1 in absolute value),
# run the estimation again:
CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects, prevAns = CoEvolutionResults)
CoEvolutionResults
Multipar.RSiena(CoEvolutionResults, 19)

## to see the results of the score-type tests for
## the indegree and outdegree effects on top:
#CoEvolutionEffects = setEffect(CoEvolutionEffects, indeg, fix = TRUE, test = TRUE, name = "top", interaction1 = "coop")
#CoEvolutionEffects
#CoEvolutionResults = siena07(CoEvolutionAlgo, data = CoEvolutionData, effects = CoEvolutionEffects, prevAns = CoEvolutionResults)
#Multipar.RSiena(CoEvolutionResults,14)

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

