#################
# Purpose:
# to explore the events log for module CMPST_0699 Semester 3 2014/15
# I'm looking to be able to 'plot' the course to provide a background 
#  a) advice for students of current cohort and 
#  b) personal performance of individuals this time against historical
#
#################


rm(list=ls())

setwd='h:/Analytics data/fhsc_skills/SEM1_2014-15/CMPST_00699 events.csv'

####
# to start with I want to get a list of student ids from the cohort - I only want those who passed the essay
# this file has results data for the module
####

users <- read.csv('CMPST_00699 essay data.csv')
totalstu<-nrow(users)
#totalstu
users <- subset(users, users$E.MARK > 40, select=c(EID, E.MARK)) # I dont need all of the vectors
#head(users)
passstu<-nrow(users)  # i will use this as an indication of relevant events - I'm thinking 10%
#passstu
####
# now I have that list I can right join with the events for this course to filter only those users events in the list
####

events <- read.csv('little.csv')  ## events for cmpst_0600 (cut down)
events <- merge(events, users, by='EID', all.y=T)
# head(events)

####
# I'm not interested in lots of the events - there are so few or meaningless. Subsetting can remove
####

events <- subset(events, events$EVENT != 'pres.end') 
events <- subset(events, events$EVENT != 'messages.delete') 
events <- subset(events, events$EVENT != 'messages.movedtodeletefolder') 
events <- subset(events, events$EVENT != 'messages.newfolder') 
events <- subset(events, events$EVENT != 'msnd.email.send') 

events$EVENT <- factor(events$EVENT)    # refactoring gets rid of empty levels 
events$EID <- factor(events$EID)        # and ids not used

####
# I need to rename a number of events for easier reading when plotting later
####

levels(events$EVENT)[levels(events$EVENT)=="asn.read.assignment"] <- "Assign.Read"
levels(events$EVENT)[levels(events$EVENT)=="asn.read.submission"] <- "Assign.Results"
levels(events$EVENT)[levels(events$EVENT)=="melete.section.read"] <- "Pathway.Read"
levels(events$EVENT)[levels(events$EVENT)=="asn.submit.submission"] <- "Assign.Submit"
levels(events$EVENT)[levels(events$EVENT)=="messages.forward"] <- "Mess.Forward"
levels(events$EVENT)[levels(events$EVENT)=="messages.reply"] <- "Mess.Reply"
levels(events$EVENT)[levels(events$EVENT)=="messages.read"] <- "Mess.Read"
levels(events$EVENT)[levels(events$EVENT)=="messages.new"] <- "Mess.New"
levels(events$EVENT)[levels(events$EVENT)=="chat.new"] <- "Chat.New"
levels(events$EVENT)[levels(events$EVENT)=="forums.read"] <- "Forum.Read"
levels(events$EVENT)[levels(events$EVENT)=="forums.response"] <- "Forum.Reply"
levels(events$EVENT)[levels(events$EVENT)=="forums.new"] <- "Forum.Post"
levels(events$EVENT)[levels(events$EVENT)=="content.read"] <- "Resource.Read"

# combine two

levels(events$EVENT)[levels(events$EVENT)=="pres.begin"] <- "Join site"
levels(events$EVENT)[levels(events$EVENT)=="site.upd"] <- "Join site"


####
# some playing around understanding how I can extract information from a table
####

#mytable <- table(events$EVENT, events$EID) # events against users contingency table
#margin.table(mytable,1) # row marginal frequencies
#margin.table(mytable,2) # column marginal frequencies
#prop.table(mytable)  # cell percentage
#round(prop.table(mytable,1),2) # row percentages
#round(prop.table(mytable,2),2) # column percentages
#ftable(mytable)  #pretty(ish) display

####
# crosstabs let me compare events by user
####

myxtab <- xtabs(~EVENT+EID, data=events) # creates a crosstab
#myxtab

####
# jiggery pokery here turns the xtab into a flat table
# then to a data.frame
# which I can use for plots
####

myft <- ftable(myxtab) # makes a flat table, works with contingency or other tables
#myft

####
# myft is a table cross matching user by event individual cells showing the frequency of events
####

mydfft <- as.data.frame(myft) # converting to a data.frame offers  more precision
#mydfft

####
# mydfft is now a list of every event by every user with a Freq count 
####

plot(mydfft$EVENT, 
     mydfft$Freq, 
     las=2) ## plots EVENTS against frequency 

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


####
# printing sorted boxplot for events 
####

# organise by median of each event frequency - it just works!
mydf2 <- with(mydfft,factor(EVENT,levels=levels(EVENT)[order(tapply(Freq,EVENT,median))])) 

par(mar=c(5,9,3,1))   # margins
boxplot(mydfft$Freq~mydf2, 
        mydfft$EVENT, 
        las=2, 
        col='lightgray',
        horizontal=T,
        staplewex=1,
        outline=F,                    # outline=FALSE removes individual marks to make it fit 
        main="Boxplot of student activity") 

# i want to add labels of median, lower, upper
# so i need the values underlying th plot

thisplot <- boxplot(mydfft$Freq~mydf2, 
                    mydfft$EVENT, 
                    horizontal=T,
                    outline=F,                    # outline=FALSE removes individual marks to make it fit 
                    plot=F) 
#thisplot

# i am here just creating a simple table to show boxplot values
mydisttable <- thisplot$stats
colnames(mydisttable) <- thisplot$names
rownames(mydisttable) <- c('min','25%', 'median','75%', 'max')
#mydisttable

df.table<-as.data.frame(mydisttable)
#df.table

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

par(mar=c(5,5,5,0),bty='n',cex.axis=.7)   # margins, no box, axis text smaller
boxplot(df.table, 
        horizontal=T, 
        las=2,
        axes=T,
        boxwex=0.6, # width of boxes
        col.axis='blue', # axis colour
        col='lightgray')




### my first function - just plots values onto a boxplot
myfunction <- function(colnum,values) {
  text(x=values, labels=values, y=colnum+0.5,col='blue', cex=0.5, srt=90)   #
}

z<-ncol(df.table)
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






