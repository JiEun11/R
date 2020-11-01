load("~/Documents/R/task/R/task4/task4.rda")
club_mat = as.matrix(club_ad)
#sum of degrees
sumDegreeNode = apply(club_mat, 1, sum)
#min, max, mean
min(sumDegreeNode)
max(sumDegreeNode)
mean(sumDegreeNode)
#frequency
dfsumDegreeNode = as.data.frame(sumDegreeNode)
#relative frequency
rfreqDfSumDegreeNode <- sumDegreeNode/length(sumDegreeNode)


