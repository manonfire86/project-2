### import libraries 

library(dplyr)
library(tidyr)
library(ggplot2)

### Load CSV

nypd_complaints_data = read.csv("NYPD_Complaint_Data_Historic.csv")


### Remove unneccessary columns for analysis

nypd_complaints_df = nypd_complaints_data[,c(-1,-3,-4,-5,-7,-9,-16,-18,-19,-20)]

nypd_complaints_df = nypd_complaints_df[,-4]


### Sort by Borrough

nypd_complaints_df = nypd_complaints_df[order(nypd_complaints_df$BORO_NM),]

### Duration of days between incident and report for all available dates

duration = as.Date(nypd_complaints_df$RPT_DT,"%m/%d/%Y")-as.Date(nypd_complaints_df$CMPLNT_FR_DT,"%m/%d/%Y")

averagereportingduration = mean(as.numeric(as.character(duration[!is.na(duration)])))

print(paste("The average duration, for all available dates, between the date of a crime's occurence and it's reporting is",round(averagereportingduration,0),"days.",sep=" "))

### Delay in reporting of each crime. This analysis extrapolates the missing duration values, and it does not create a fully unique list of crimes. This is a relatively rough high level overview of the data.

nypd_complaints_df$Duration = as.numeric(as.character(duration))
nypd_complaints_df$KYCD = nypd_complaints_data$KY_CD
nypd_complaints_df$PoliceDesc = nypd_complaints_data$PD_DESC
crimedurationdf = nypd_complaints_df[,c("OFNS_DESC","KYCD","PoliceDesc","Duration")]
crimedurationdf = crimedurationdf[order(crimedurationdf$OFNS_DESC),]
crimedurationdf = crimedurationdf %>% fill(Duration)
missingvals = which(crimedurationdf$OFNS_DESC=="")
crimedurationdf$OFNS_DESC = as.character(crimedurationdf$OFNS_DESC)
crimedurationdf[missingvals,"OFNS_DESC"] = list(as.character(crimedurationdf[missingvals,"PoliceDesc"]))
crimedurationdf[which(crimedurationdf$OFNS_DESC==""),"OFNS_DESC"] = "Other"
options(warn = -1)
averagedurationbycrime = aggregate(crimedurationdf,by = list(crimedurationdf$OFNS_DESC),FUN=mean)
averagedurationbycrime = averagedurationbycrime[,c(-2,-3,-4)]

### Delays greater than 100

print(subset(averagedurationbycrime,Duration>100))

print(paste("The crime that has the longest delay between reporting and occurrence is",subset(averagedurationbycrime,Duration == max(Duration))[1],"with an average delay of",subset(averagedurationbycrime,Duration == max(Duration))[,2],"days"))

### Crime by Burrough: Broad overview. Using the tail function, I saw that most entries with their Jurisdiction containing "N.Y. Police Dept" belonged to Staten Island; thus I filled all NA Burrough entries with "Staten Island" as over 90% of NA fields had a coinciding jurisidiction of "N.Y. Police Dept".

burroughdf = nypd_complaints_df[,c("BORO_NM","LAW_CAT_CD")]
missingburroughval = which(burroughdf$BORO_NM=="")
burroughdf[missingburroughval,"BORO_NM"] = "STATEN ISLAND"
frequencytablebroad = table(burroughdf$LAW_CAT_CD)

print(frequencytablebroad)


par("mar")
par(mar=c(11,11,11,11))
getOption("scipen")
opt <- options("scipen" = 20)
getOption("scipen")
barplot(frequencytablebroad,ylab = "Frequency",main = "Overall Offenses 2006-2016",cex.names = .6,ylim = c(0,5000000),cex.axis = .8)

### Crime by Burrough: Analysis of frequency of occurrence for Felonies, Misdemeanors, and Violations

freqtableburr = table(burroughdf)
freqtableburr = as.data.frame(freqtableburr)
freqtableburr = subset(freqtableburr,BORO_NM != "")

print(freqtableburr)

par("mar")
par(mar=c(7,7,7,7))
ggplot(freqtableburr,aes(x = BORO_NM,Freq,fill=LAW_CAT_CD))+geom_bar(position = position_dodge(),stat = "identity") +theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title = "Violations by Burrough 2006-2016",y="Frequency",x = "Burrough")

### Percentage of Crimes Completed

completiondf = subset(nypd_complaints_data,CRM_ATPT_CPTD_CD =="COMPLETED")
completionrate = (length(completiondf$CRM_ATPT_CPTD_CD)/length(nypd_complaints_data$CRM_ATPT_CPTD_CD))*100

print(paste("Of the total number of attempted crimes,the completion rate is",round(completionrate,2),"%"))

### Granular View: Crimes with the greatest delay between reporting and occurrence by Burrough

granburroughdf = burroughdf
granburroughdf$Offense = nypd_complaints_df$OFNS_DESC
granburroughdf$LAW_CAT_CD = nypd_complaints_df$LAW_CAT_CD
granmissingvals = which(granburroughdf$OFNS_DESC=="")
granburroughdf$Offense = as.character(granburroughdf$Offense)
granburroughdf[granmissingvals,"Offense"] = list(as.character(granburroughdf[granmissingvals,"LAW_CAT_CD"]))
granburroughdf = granburroughdf[,-2]

topitems = unlist(subset(averagedurationbycrime,Duration>100)[1],use.names = FALSE)
granburroughdftopitems = granburroughdf[which(granburroughdf$Offense %in% topitems),]
freqgranburrough = table(granburroughdftopitems)
freqgranburrough = as.data.frame(freqgranburrough)
freqgranburrough = subset(freqgranburrough, BORO_NM != "")
freqgranburrough = subset(freqgranburrough, Offense != "")

ggplot(freqgranburrough,aes(x = BORO_NM,Freq,fill=Offense))+geom_bar(position = position_dodge(),stat = "identity") +theme(axis.text.x = element_text(angle = 90, hjust = 1))+labs(title = "Crimes by Burrough 2006-2016",y="Frequency",x = "Burrough")

