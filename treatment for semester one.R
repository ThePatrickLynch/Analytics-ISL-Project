#################
# Purpose:
# to explore the events log for module CMPST_0699 Semester 3 2014/15
# I'm looking to be able to 'plot' the course to provide a background 
#  a) advice for students of current cohort and 
#  b) personal performance of individuals this time against historical
#
#################


Sys.time()

rm(list=ls())

#setwd('d:/Working Analytics')
#setwd('d:/Data/Analytics/Data/16022015/FROZEN cmpst_00699 Semester 1 2014-15/')
setwd('h:/Analytics data/16022015/FROZEN cmpst_00699 Semester 1 2014-15/')

getwd()

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

events <- read.csv('FROZEN - cmpst_00699 as at 26 Feb 2015.csv')  ## events for cmpst_0600 (cut down)
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
events <- subset(events, events$EVENT != 'content.new')
events <- subset(events, events$EVENT != 'content.available')


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

Sys.time()

myxtab <- xtabs(~EVENT+EID, data=events) # creates a crosstab
str(myxtab)

library(RColorBrewer) # nice colour palette stuff
somecols<-brewer.pal(15, "Set3")

barplot(myxtab, 
        xlab="Freq.", 
        #ylab="User", 
        las=2,  # vertical labels at bottom
        legend=T,
        horiz=T,
        col=somecols   
        #col=unique(myxtab)
        )

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

#plot(mydfft$EVENT, 
#     mydfft$Freq, 
#     las=2) ## plots EVENTS against frequency 

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

# i am here just creating a simple table to show boxplot values
mydisttable <- thisplot$stats
colnames(mydisttable) <- thisplot$names
rownames(mydisttable) <- c('min','25%', 'median','75%', 'max')
#mydisttable

df.table<-as.data.frame(mydisttable)
#df.table

# save values for future fastbuild
write.csv(df.table,file='SEM1 dftable.csv')
write.csv(mydfft,file='SEM1 dfft.csv')
write.csv(myxtab,file='SEM1 event~EID xtab.csv')





