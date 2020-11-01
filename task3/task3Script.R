load("~/Documents/R/task/R/task3/task3.rda")
df<-as.data.frame(read.csv("/Users/jieun/Documents/R/task/weather_nominal.csv", header = T, sep = ";"))
df2 <-read.csv("/Users/jieun/Documents/R/task/iris.csv", header = T, sep = ";", dec = ",", stringsAsFactors=F)
#weather nominal data
#attributes' name (columns' names)
names(df)
#values of Outlook attribute
df$Outlook
df$Temperature
df$Humidity
df$Windy
df$Play
#dimension of data frame
dim(df)
#structure of data frame
str(df)
#attributes
attributes(df)
#first 5 rows of data frames
head(df, 5)
#last two rows of data frames
tail(df, 2)
#returns the frequency of all values of the given attribute (for numeric even median, mean,..)
summary(df)
#to create tabular results of categorical variables
t<- table(df$Outlook)
t
barplot(t)
pie(t)


#Task3-iris data
#Storing to factor 
df2$Sepal.Length <- factor(df2$Species)
str(df)
#table of frequencies
table(df2$Species)
#table of relative frequencies
table(df2$Species)/length(df2$Species)

pie(table(df2$Species))
barplot(table(df2$Species))

#mean
mean(df2$Sepal.Length)
#total variance
var(df2$Sepal.Length)
#standard deviation
sd(df2$Sepal.Length)
#median
median(df2$Sepal.Length)
range(df2$Sepal.Length)
min(df2$Sepal.Length)
max(df2$Sepal.Length)

#table of frequencies
table(df2$Sepal.Length)
#table of rel.frequencies
table(df2$Sepal.Length)/length(df2$Sepal.Length)

#histogram, takes it as a continuous variable
hist(df2$Sepal.Length)
#probability density function
plot(density(df2$Sepal.Length))

#I'd like to join it in one plot
hist(df2$Sepal.Length, freq=F, col="grey")
lines(density(df2$Sepal.Length), col="blue", lwd=2)

#finer binning (finer division of the x-axis interval)
hist(df2$Sepal.Length, breaks = (max(df2$Sepal.Length)-min(df2$Sepal.Length))*5, probability = T, col="grey")
lines(density(df2$Sepal.Length), col="blue", lwd=2)

#add normal distribution
curve(dnorm(x, mean=mean(df$Sepal.Length),sd=sd(df$Sepal.Length)), min(df$Sepal.Length), max(df$Sepal.Length), add=T, col="green")

##Install if missing
if (!require("fitdistrplus"))
  install.packages("fitdistrplus")

library(fitdistrplus)
##empirical cumulative distribution function
plot(ecdf(df2$Sepal.Length))

#using the plotdist() function from the fitdistrplus package
plotdist(df2$Sepal.Length, histo = TRUE, demp = TRUE )

#Can the Sepal.Length frequency be used as a discrete variable?
#use table()
fsl<-table(df2$Sepal.Length)
#relative frequency
prop.table(fsl)

plot(as.numeric(names(fsl)), as.numeric(fsl), xlab="hodnota", ylab = "cetnost", main='hist Sepal.Length')

#the histogram or column chart is better
plot(fsl)
#or
barplot(fsl)

#pmf, probability mass function
plot(prop.table(fsl))

#we will make a matrix from the first two columns
#function cbind()
dm<-cbind(df2$Sepal.Length, df2$Sepal.Width)
#or dm<-dist(df[,c("Sepal.Length", "Sepal.Width")])
dm

#Euclidean distance
dist(dm)

#we do not see much, so we will only count it for the first few instances
dm2 <- head(dm)
dm2
dist(dm2)

min(dist(dm2))
max(dist(dm2))

#cosine measure (similarity)
##Install if missing 
if (!require("lsa"))
  install.packages("lsa")
library(lsa)

#transpose matrix, Why? test the cosine() function on an untranspose matrix
dm2
consine(dm2)
min(cosine(dm2))
max(cosine(dm2))
dm2<-t(dm2)