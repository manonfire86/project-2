### load libraries
library(dplyr)
library(tidyr)

### import catfish data
catfishdata = read.csv("CatfishFarm.csv",header = TRUE)
catfishdata = catfishdata[-8:-9,]
colnames(catfishdata) = unlist(catfishdata[1,])
catfishdata = catfishdata[-1,]

### Convert the data from wide to long format

longformatcatfishdata = catfishdata %>%
  gather(Year,Size,`1992`:`2016`) %>%
  spread(key = `Size category`,value = Size)

### Rename the columns

colnames(longformatcatfishdata) = c('Year','Broodfish','Fingerling','Large Size','Medium Size','Small Size','Stockers')

### Convert to dataframe, convert to numeric columns, and calculate row totals for analysis.

catfishdf = data.frame(longformatcatfishdata)
catfishdf$Broodfish = as.numeric(gsub(",","",catfishdf$Broodfish))
catfishdf$Fingerling = as.numeric(gsub(",","",catfishdf$Fingerling))
catfishdf$Large.Size = as.numeric(gsub(",","",catfishdf$Large.Size))
catfishdf$Medium.Size = as.numeric(gsub(",","",catfishdf$Medium.Size))
catfishdf$Small.Size = as.numeric(gsub(",","",catfishdf$Small.Size))
catfishdf$Stockers = as.numeric(gsub(",","",catfishdf$Stockers))
catfishdf$Total = rowSums(catfishdf[,2:7])

### Calculation of Average Annual Growth Rate 

averageannualgrowthrate = (((catfishdf$Total[nrow(catfishdf)]-catfishdf$Total[1])/catfishdf$Total[1])*100)/(as.numeric(catfishdf$Year[nrow(catfishdf)])-as.numeric(catfishdf$Year[1]))

### Calculation of YoY change of Total Population

yoy_change = list()
for(i in 1:nrow(catfishdf)){
  a = ifelse(i == 1,0,(((catfishdf$Total[i]-catfishdf$Total[i-1])/catfishdf$Total[i-1])*100))
  yoy_change[i] = a
  }

### Appending of New columns to the final data frame 

catfishdf$YoY_Change = yoy_change
catfishdf$Cooking_Style = 'Fried'

### Conclusion: The average annual growth rate is -2.52%, indicating that the overall catfish population was on a steady decline year over year. The YoY change indicates there were only small periods of actual growth.

print(paste("The average annual growth rate is ",averageannualgrowthrate,"%",sep = ""))
print(catfishdf)
