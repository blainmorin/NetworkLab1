# DESCRIPTIVE NETWORK ANALYSIS IN R
#----------------------------------------


# load up
library(network)
library(sna) 

# Let's play with the (thinned) Senate data we had been using
load('SenNet2.RData')


# Degree centrality
#---------------------------------------

# easy to get; let's use in-degree centrality
dc <- degree(SenNet2, cmode="indegree")
dc # harder to make sense of alone

# lets start building a table for ourselves
attributes <- as.data.frame(network.vertex.names(SenNet2))
attributes$dwnom <-get.vertex.attribute(SenNet2, "dwnom")
attributes$dc <- dc
attributes

plot(attributes$dwnom, attributes$dc)
# Interesting, seems people to the middle of either side are most degree central

# Plot network with node size proportional to Degree Centrality
## First normalize degree 
ndc <- dc/max(dc)
## Set random number seed so the plot is replicable
set.seed(5)
## Now plot
plot(SenNet2,displaylabels=T,label=network.vertex.names(SenNet2),vertex.cex=3*ndc,label.cex=1,edge.col=rgb(150,150,150,100,maxColorValue=255),label.pos=5,vertex.col="lightblue")


# Get ordered labels
dclab <- senlist$labels[order(dc)]

# Sort degree centrality
dcs <- sort(dc)
plot(1:length(dc),dcs)
text(1:length(dc),dcs,lab=dclab,adj=c(1,.05), col=sample(c("red","blue","black"),length(dc),rep=T), cex=.75)

# create size attribute
dsize <- rank(dc)/60
gplot(SenNet2, label = network.vertex.names(SenNet), edge.col="grey55", label.cex=.75, displayisolates=F, vertex.cex=dsize)


# Betweenness centrality
#---------------------------------------
bc <- betweenness(SenNet2)
bc

attributes$bc <- bc
plot(attributes$dwnom, attributes$bc)

nbc <- bc/max(bc)
set.seed(5)
plot(SenNet2,displaylabels=T,label=network.vertex.names(SenNet2),vertex.cex=3*nbc,label.cex=1,edge.col=rgb(150,150,150,100,maxColorValue=255),label.pos=5,vertex.col="lightblue")

# Get ordered labels
bclab <- senlist$labels[order(bc)]

# Sort betweenness centrality
bcs <- sort(bc)
plot(1:length(bc),bcs)
text(1:length(bc),bcs,lab=bclab,adj=c(1,.05), col=sample(c("red","blue","black"),length(bc),rep=T),cex=.75)


# Eigenvector centrality
#---------------------------------------
ec <- evcent(SenNet2)
ec

attributes$ec <- ec
plot(attributes$dwnom, attributes$ec)

ndc <- ec/max(ec)
set.seed(5)
plot(SenNet2,displaylabels=T,label=network.vertex.names(SenNet2),vertex.cex=3*ndc,label.cex=1,edge.col=rgb(150,150,150,100,maxColorValue=255),label.pos=5,vertex.col="lightblue")


# Comparing centrality measures
#---------------------------------------
plot(dc,ec)
plot(dc,bc)
plot(ec,bc)
cor(cbind(ec,bc,dc))



# Clustering coefficient
#---------------------------------------
library(igraph)

snet <- graph.adjacency(SenNet2[,])
cc <- transitivity(snet,type="local")
cc
# isolates cause problems here again

# Store in data frame
attributes$cc <- cc
attributes

# igraph and statnet offen overlap/conflict, detach igraph before using statnet again
detach(package:igraph)

attributes$cc <- cc
plot(attributes$dwnom, attributes$cc)

ndc <- cc/max(cc)
set.seed(5)
plot(SenNet2,displaylabels=T,label=network.vertex.names(SenNet2),vertex.cex=3*ndc,label.cex=1,edge.col=rgb(150,150,150,100,maxColorValue=255),label.pos=5,vertex.col="lightblue")
#????

# Closeness centrality
#---------------------------------------
cc <- closeness(SenNet2)
cc
# What's the problem here?
library(igraph)
cc <- closeness(snet)
cc
detach(package:igraph)

# Get ordered labels
cclab <- senlist$labels[order(cc)]

# Sort closeness centrality
ccs <- sort(cc)
plot(1:length(cc),ccs)
text(1:length(cc),ccs,lab=cclab,adj=c(1,.05), col=sample(c("red","blue","black"),length(cc),rep=T), cex=.75)



# Measures on the network
#---------------------------------------

# back to the igraph package
library(igraph)

# Density
graph.density(snet)

# Diameter
diameter(snet, directed=T, weights=NA)
mean_distance(snet, directed=T)
distances(snet)

# Reciprocity
reciprocity(snet)
dyad_census(snet) # Mutual, asymmetric, and nyll node pairs
2*dyad_census(snet)$mut/ecount(snet) # Calculating reciprocity

# Clustering coefficient / Transitivity
transitivity(snet, type="global")  # net is treated as an undirected network
transitivity(as.undirected(snet, mode="collapse")) # same as above
transitivity(snet, type="local")
triad_census(snet) # for directed networks 


# degree distribution
deg <- degree(snet, mode="all")
deg.dist <- degree_distribution(snet, cumulative=T, mode="all")
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")
plot(deg.dist, log = "xy", type="l", lwd=3, col="grey", xlab="Degree", ylab="Cumulative Distribution Function", main="")
# not exactly a power law


# Assortativity and Homophily
# Homophily: the tendency of nodes to connect to others who are similar on some variable.

# assortativity_nominal() is for categorical variables (labels)
# assortativity() is for ordinal and above variables
# assortativity_degree() checks assortativity in node degrees

## remember to load dwnom again from previous script
V(snet)$dwnom <- dwnom[,1]

assortativity(snet, V(snet)$dwnom, directed=T)
## [1] 0.4688609
assortativity_degree(snet, directed=T)
## [1] 0.03919813


# Remove igraph to restore network functions
 detach("package:igraph")

