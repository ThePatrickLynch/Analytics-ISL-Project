

rm(list=ls())
setwd('d:/Data/Working Analytics')

#install.packages('ggplot2')
library(ggplot2)

readdate <- function()
{ 
  n <- readline(prompt="Enter an date as 'dd/mm/yyyy': ")
  n <- as.POSIXct(n, format="%d/%m/%Y")
  return(n)
}



# The following should come in as a parameter when  figure that out
# it works like this - args <- commandArgs(trailingOnly = TRUE)
# called by (in path) rscript filename.R args'

# for now input

tDate <- readdate()
 
# should probably have a start date, but later slice looks only at student activity
tDate


####
# to start with I want to get a list of student ids from the cohort - I only want those who passed the essay
# this file has results data for the module
####
users <- read.csv('CMPST_00699 essay data.csv')
events <- read.csv('CMPST_00699 events.csv')

####
# first subset data for sdmaller transforms
####


# convert EVENT_DATEs to date format
events$EVENT_DATE <- as.POSIXct(as.character(events$EVENT_DATE), format="%d/%m/%Y %H:%M")
events <- subset(events, events$EVENT_DATE <= tDate) # take only those lt or eq target date

tail(events)
head(events)
####
# get a list of only students who passed
users <- subset(users, users$E.MARK > 40, select=c(EID, E.MARK)) # I dont need all of the vectors
# now I have that list I can right join with the events for this course to filter only those users in the list
# this will also get rid of staff events 
events <- merge(events, users, by='EID', all.y=T) 
events <- events[order(events$EVENT_DATE),] # need to reorder on date (asc) after merge

### 
# select only columns I have an interest in
###

events <- subset(events, select = c(EVENT_DATE, EID, EVENT))


####
# I'm not interested in lots of the events - there are so few or meaningless. Subsetting can remove
####

#events <- subset(events, events$EVENT != 'pres.end') 
events <- subset(events, events$EVENT != 'messages.delete') 
events <- subset(events, events$EVENT != 'messages.movedtodeletefolder') 
events <- subset(events, events$EVENT != 'messages.newfolder') 
#events <- subset(events, events$EVENT != 'msnd.email.send') 
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


# so now I want to plot overall activity to a given date

# histogram of events dist
events$EVENT_DATE <- round(events$EVENT_DATE , "day" ) # round dates to just days

firstDate<-events$EVENT_DATE[1]  # gets the first date in the list
head(events)

hBreaks<-as.numeric(difftime(tDate, firstDate , units="days")) # calculates the number of days in the data
firstDate
head(events)
par(mar=c(10,9,10,10))   # margins
hist(events$EVENT_DATE, hBreaks, las=2)


# then get count for each day
dailyCount<- aggregate(events, by = list(as.character(events$EVENT_DATE)), length)
head(dailyCount)
str(dailyCount)

# dont need these two columns now
dailyCount$EVENT_DATE <- NULL
dailyCount$EID <- NULL

par 

ggplot(aes(x = Group.1, y = EVENT), data = dailyCount) +
  geom_point() +
  geom_point(color='blue') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Total activity', y = 'Date', title='Activity by date')


# dates as factors are not continuous here so I need to define a dummy group for ggplot to
# allow it to join the points

ggplot(aes(x = Group.1, y = EVENT, group=1), data = dailyCount) + 
  geom_line() +
  geom_line(color='blue') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Total activity', y = 'Date', title='Activity by date')


