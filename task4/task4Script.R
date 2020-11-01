#loading packages
library(fitdistrplus) #for plot(ecdf)
library(Rfast) #for floyd
library(igraphdata) #for karate club
library(igraph) #for karate club graph
club_mat = as.matrix(club_ad)
#sum of degrees
sumDegreeNode = apply(club_mat, 1, sum)
#min, max, mean
min(sumDegreeNode)
max(sumDegreeNode)
mean(sumDegreeNode)
#frequency
dfsumDegreeNode <- as.data.frame(sumDegreeNode)
#relative frequency
rfreqDfSumDegreeNode <- sumDegreeNode/length(sumDegreeNode)
#change to dataframe
dfrFreqDFSumDegreeNode <- as.data.frame(rfreqDfSumDegreeNode)
#histogram about frequency
hist(sumDegreeNode, breaks = (max(sumDegreeNode)-min(sumDegreeNode))*5, probability = T, col="grey")
#histogram about relative frequency
hist(rfreqDfSumDegreeNode, breaks = (max(rfreqDfSumDegreeNode)-min(rfreqDfSumDegreeNode))*5, probability = T, col="grey")
hist(rfreqDfSumDegreeNode)

#empirical cumulative distribution function
plot(ecdf(sumDegreeNode))

#using the plotdist() function from the fitdistrplus package
plotdist(sumDegreeNode, histo = TRUE, demp = TRUE)

#karate club network
z <- graph.famous("Zachary")

#plot the graph to see what we are talking about.
lay<- layout.fruchterman.reingold(z)
plot(z, layout = lay, vertex.label.cex = 1.5, main="Zachary Karate Club graph")

#define some configuration
nColors = 100
palette <- colorRampPalette(c("yellow", "red"))(nColors)
norm <- function(x) {x / sqrt(sum(x^2))}

#popularity
deg=degree(z)
deg = norm(deg)
dlab = as.integer(deg*100)
degCol = palette[as.numeric(cut(deg, breaks = nColors))]
plot(z, layout=lay, vertex.color=degCol, vertex.size=deg*100, vertex.label.cex=1.5, main="Degree")

#Closeness, average shortest distance between a node
clos = closeness(z)
clos = norm(clos)
clab = as.integer(clos*100)
closCol <- palette[as.numeric(cut(clos,breaks = nColors))]
plot(z, layout = lay, vertex.color = closCol, vertex.size = clos*100, vertex.label.cex=1.5, main="Closeness")

#distance between all pairs of nodes
dis <- closeness(z)

#mean distance
mean(dis)

#diameter 
diameter(z)

#store about factor to create frequency histogram
disfac <- factor(dis)
table(disfac)

#plot the histogram
hist(table(disfac))
hist(dis)

#Betweeness : a measure of how many paths are using a node.
btw <- betweenness(z)
btw <- norm(btw)
blab <- as.integer(btw*100)
btwCol <- palette[as.numeric(cut(btw, breaks= nColors))]
plot.igraph(z, layout = lay, vertex.color=btwCol, vertex.size=btw*100, vertex.label.cex=1.5, main="Betweenness")


