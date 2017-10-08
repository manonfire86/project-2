### import libraries

library(dplyr)
library(tidyr)

### Read the csv file

yogadata = read.csv("20160502_YogaByStateMonth.csv")


### Transform the data set from a wide to long data set

yogadata = yogadata[-1,]
colnames(yogadata)[1] = "Year"

longyogadata = yogadata %>%
  gather(States,Years,Alabama..us.al.:Wyoming..us.wy.) %>%
  spread(key = States,value = Years)

longyogadata$Total = rowSums(longyogadata[,2:ncol(longyogadata)])

yearofpeakinterest = longyogadata[,c("Year","Total")]

### Analysis: Year and month of Peak Interest

peakyear  = subset(yearofpeakinterest,Total == max(yearofpeakinterest[,"Total"]) )

print(head(peakyear))

### Analysis: State with the most interest in yoga

temptable = longyogadata

convertcolumns = function(x){
  as.numeric(unlist(temptable[x]))
}

for(i in 1:ncol(temptable)){
  temptable[i] = convertcolumns(i)
}

statestotal = list()
for(i in 1:ncol(longyogadata)){
  statestotal[i] = sum(temptable[,i])
}

statewithmaxint = data.frame(statestotal)
statewithmaxint = statewithmaxint[,c(-1,-ncol(statewithmaxint))]
colnames(statewithmaxint) = colnames(longyogadata[,2:(ncol(longyogadata)-1)])
statewithmaxint =t(statewithmaxint)
colnames(statewithmaxint) = "Interest"

peakstate = subset(statewithmaxint,statewithmaxint[,'Interest'] == max(statewithmaxint[,"Interest"]))

print(head(peakstate))

### Analysis: Most interest across the last 10 years

years = as.character(longyogadata$Year)
yearlist = list()
for(i in 1:length(years)){
  yearlist[i] = substr(years[i],1,4)
}

temptable2 = longyogadata
temptable2$Year = unlist(yearlist)
temptable2$Year = as.numeric(temptable2$Year)
interestinyears = aggregate(temptable2,by=list(temptable2$Year),FUN=sum)
peakyeardf = interestinyears[,c("Group.1","Total")]

print(peakyeardf)
### Conclusion: The State with the most people interested in Yoga is Vermont with a total of 7529 people interested across the entire twelve year period of 2004-2016. The year and month that generated peak interest across all states was January 2016 with 1803 people interested. The peak year for yoga interest across all states in the last 10 years was 2015 with a total of 18475.

print(paste("The year and month of Peak interest in yoga was",peakyear$Year,"with a total of",peakyear$Total,"people interested",sep=" "))
print(paste("The State with the most interest in yoga is",rownames(peakstate),"with a total of",peakstate[1],"people interested",sep=" "))
print(paste("The peak year for yoga interest in the last 10 years, for all states, was",subset(peakyeardf,Total==max(peakyeardf[(nrow(peakyeardf)-10):nrow(peakyeardf),]))[1],"with a total of",max(peakyeardf[(nrow(peakyeardf)-10):nrow(peakyeardf),]),sep=" "))


