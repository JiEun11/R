#useful packages
#tidyr ('데이터셋 레이아웃 바꿀 때 유용')
#R markdown ('통계분석에 적합한 유연한 문서 작성')

#load data
wthr <- read.csv("/Users/jieun/Documents/R/task/weather_nominal.csv", header = T, sep = ";", dec = ",", stringsAsFactors = T)
summary(wthr)

