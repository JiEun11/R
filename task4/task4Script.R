#loading packages
library(fitdistrplus) #for plot(ecdf)
library(Rfast) #for floyd
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







