##########
# using stored data put together barplot
##########

#**************
# not working. can't figure how to save xtab and the read ther file into good format
#***************

rm(list=ls())

#setwd='d:/Data/16022015/FROZEN cmpstr_00699 Semester 1 2014-15'
setwd('h:/Analytics data/16022015/FROZEN cmpst_00699 Semester 1 2014-15/')

# read some files and set ome values

users <- read.csv('CMPST_00699 essay data.csv')
totalstu<-nrow(users)
#totalstu
users <- subset(users, users$E.MARK > 40, select=c(EID, E.MARK)) # I dont need all of the vectors
head(users)
passstu<-nrow(users)  # i will use this as an indication of relevant events - I'm thinking 10%
#passstu
getwd()

myxtab <- read.csv('SEM1 event~EID xtab.csv')

str(myxtab)
myxtab

####
# this does stacked bars for each user
#### 

par(mar=c(5,5,1,1)) # more space for bottom margin
barplot(myxtab, 
        xlab="Freq.", 
        #ylab="User", 
        las=2,  # vertical labels at bottom
        legend=T,
        horiz=T,
        col=unique(myxtab)) # events by user las=2 is vertical labels on yaxis