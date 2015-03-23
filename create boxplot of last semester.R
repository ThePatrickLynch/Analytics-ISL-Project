##########
# using stored data put together boxplot
##########

rm(list=ls())

setwd='d:/Data/16022015/FROZEN cmpstr_00699 Semester 1 2014-15'

# read some files and set ome values

users <- read.csv('CMPST_00699 essay data.csv')
totalstu<-nrow(users)
#totalstu
users <- subset(users, users$E.MARK > 40, select=c(EID, E.MARK)) # I dont need all of the vectors
#head(users)
passstu<-nrow(users)  # i will use this as an indication of relevant events - I'm thinking 10%
#passstu

df.table <- read.csv('SEM1 dftable.csv')
mydfft <- read.csv('SEM1 dfft.csv')

####
# here I am going to remove those events where there isnt a very high median
####

#finding median over 10% of sample as dont want to show smaller
lowest=passstu/10
#lowest

z<-which(df.table[3,] > lowest)    #find where 3rd row (median) is greater than 5
#z
df.table<-df.table[z] # take only columns in the vector created above
#df.table

z<-ncol(df.table)

library(RColorBrewer) # nice colour palette stuff
somecols<-brewer.pal(z, "Set2")

par(mar=c(5,5,5,0),bty='n',cex.axis=.7)   # margins, no box, axis text smaller
boxplot(df.table, 
        horizontal=T, 
        las=2,
        axes=T,
        boxwex=0.6, # width of boxes
        col.axis='blue', # axis colour
        col=somecols)


#### 
# I need to think about the size of the students points in case I need to extend x axis.
# also what if the student has other larger activities?
####


### my first function - just plots values onto a boxplot
myfunction <- function(colnum,values) {
  text(x=values, labels=values, y=colnum+0.5,col='blue', cex=0.5, srt=90)   #
}


i<-1
for (i in 1:z) {
  myfunction(i,df.table[,i])
  
}

ymax<-max(df.table)

# stick some sample size data on
text(x=ymax,labels=paste('Passes: (shown)',passstu),y=1,adj=1)
text(x=ymax,labels=paste('Participants:',totalstu),y=1.5, adj=1)


#### and adding back individual students ...

# subset xtab for student EID 492119
mydfft
this.dt<-subset(mydfft,mydfft$EID=='492119')
this.dt

# now plot against each of the boxplot lines the data in this.dt
df.table
interest<-names(df.table)

i<-1
for (i in 1:z) {
  val<-this.dt$Freq[this.dt$EVENT==interest[i]]
  pchval<-16
  points(val,i, pch=pchval, col='orange')
}





