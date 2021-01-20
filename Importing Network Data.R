# IMPORTING NETWORK DATA INTO R
#----------------------------------------

# Often harder than it seems like it should be, don't get discouraged

# The easiest way: use pre-packaged data!
library(network) # Make sure that network package is loaded
data(package="network") # List available datasets in network package
data(flo) # Load a built-in data set; see ?flo for more
flo # Examine the flo adjacency matrix

# Network data comes in a few fundamental forms:
	# Vertex-level Data: Vertex attributes (n rows and k columns)
	# Adjacency Matrix Data for each relation (n by n) matrix
	# Edgelist Data for each edge (e by p) matrix. p typically two—sender & receiver


# Tell R where to look by setting your working directory
# setwd("~/Dropbox/teaching/Networks Class/Basics_2_Descriptives/Lab Files 2/") # path will be different for you
# getwd() # check that it worked

# Read in adjacency matrices
## read.csv creates a data frame object from a CSV file
## Need to indicate that there's no header row in the CSV
advice <- read.csv("Advice.csv", header=F)
reportsto <- read.csv("ReportsTo.csv", header = F)

# Read in vertex attribute data
attributes <- read.csv("KrackhardtVLD.csv")

# Creating Network Objects
#----------------------------------------

# Loading the data is not enough; must create network objects of appropriate class for the package you are using

# Main packages are the statnet suite (network, sna..) and igraph, but note that they require different network objects!

# Read in the library for network analysis
library(network,quietly=T)

## Start with the Advice network

# Use the advice network dataset to create network object
adviceNet <- network(advice)

# Add the vertex attributes into the network
set.vertex.attribute(adviceNet,names(attributes),attributes)

# Add the organizational chart as a network variable
set.edge.attribute(adviceNet,"reportsto",reportsto)

# Simple plot
## Set random number seed so the plot is replicable
set.seed(510)
## Plot the network
plot(adviceNet,displaylabels=T,label=get.vertex.attribute(adviceNet,"Level"),vertex.cex=2,label.cex=1,edge.col=rgb(150,150,150,100,maxColorValue=255),label.pos=5,vertex.col="lightblue")

# We can save this as a network object to make it easier to use in the future
#save(adviceNet, file="adviceNet")

# Now let's do the defense pact network
#--------------------------------------------------


# This dataset comes in edgelist format (advice was in adjacency matrix format)
# Read in vertex dataset
allyV <- read.csv("allyVLD.csv",stringsAsFactors=F)

# Read in edgelist
allyEL <- read.csv("allyEL.csv", stringsAsFactors=F)

# Read in contiguity
contig <- read.csv("contiguity.csv",stringsAsFactors=F,row.names=1)

# (1) Initialize network
# store number of vertices
n <- nrow(allyV)
AllyNet <- network.initialize(n,dir=F)

# (2) Set vertex labels
network.vertex.names(AllyNet)  <- allyV$stateabb

# (3) Add in the edges
# Note, edgelist must match vertex labels
AllyNet[as.matrix(allyEL)]  <- 1

# (4) Store country code attribute
set.vertex.attribute(x=AllyNet,             # Network in which to store
            "ccode",            # What to name the attribute
            allyV$ccode)            # Values to put in

# (5) Store year attribute
set.vertex.attribute(AllyNet,"created",allyV$styear)

# (6) Store network attribute
set.edge.attribute(AllyNet,"contiguous", contig)

# Simple plot
plot(AllyNet,displaylabels=T,label.cex=.5,edge.col=rgb(150,150,150,100,maxColorValue=255))

# Save for future use
#save(AllyNet, file="AllyNet")

# Now the Senate cosponsorship network
#--------------------------------------------------

# These data are semi-pre-packaged (an adjacency matrix and a set of labels bound in one object)
# Read in the cosponsorship data
senlist <- dget("sennet.txt")

# Import vertex attributes; ideology scores on two dimensions
dwnom <- read.csv("dwnom.csv")

# Initialize a Network
SenNet <- network(senlist$net)

# Add the senators' names
network.vertex.names(SenNet) <- senlist$labels

# Add in the dwnominate information
set.vertex.attribute(SenNet,"dwnom",dwnom[,1]) # first dimension
set.vertex.attribute(SenNet,"dwnom2",dwnom[,2]) # second dimension

# Plot the Network
plot(SenNet, label = network.vertex.names(SenNet))
# This looks like a big mess. 
# In some cases, we might want to thin the network (be very cautious when doing this!)

# Thin the Network
SenNet2 <- network(senlist$net > 10)
network.vertex.names(SenNet2) <- senlist$labels
set.vertex.attribute(SenNet2,"dwnom",dwnom[,1]) # first dimension
set.vertex.attribute(SenNet2,"dwnom2",dwnom[,2]) # second dimension

# Look at it Again
plot(SenNet2, label = network.vertex.names(SenNet), edge.col="grey55", label.cex=.75)
plot(SenNet2, label = network.vertex.names(SenNet), edge.col="grey55", label.cex=.75, displayisolates=F)
plot(SenNet2, label = network.vertex.names(SenNet), edge.col="grey55", label.cex=.75, displayisolates=F, vertex.col=get.vertex.attribute(SenNet2, "dwnom"))

# Save for future use
#save(SenNet, file="SenNet.RData")
#save(SenNet2, file="SenNet2.RData")

