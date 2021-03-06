###########
# Pulse
# shows an individual student's activity against previous cohorts average
###########

rm(list=ls())
setwd('h:/Analytics Data/fhsc_skills/SEM1_2014-15')

#install.packages('ggplot2')
library(ggplot2)
library(scales) # breaks and formatting functions

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
# tDate <- readdate() 
FromDate <- as.POSIXct("03/10/2014", format="%d/%m/%Y")  # this was the end of the course
tDate <- as.POSIXct("15/01/2015", format="%d/%m/%Y") 

# should probably have a start date, but later slice looks only at student activity
FromDate
tDate


####
# to start with I want to get a list of student ids from the cohort - I only want those who passed the essay
# this file has results data for the module
####
users <- read.csv('CMPST_00699 essay data.csv')
events <- read.csv('CMPST_00699 events.csv')

### 
# select only columns I have an interest in
###
events <- subset(events, select = c(EVENT_DATE, EID, EVENT))

# convert EVENT_DATEs to date format
events$EVENT_DATE <- as.POSIXct(as.character(events$EVENT_DATE), format="%d/%m/%Y %H:%M")

events <- subset(events, events$EVENT_DATE >= FromDate) # only after official start date

events <- subset(events, events$EVENT_DATE <= tDate) # take only those lt or eq target date


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


# histogram of events dist
events$EVENT_DATE <- round(events$EVENT_DATE , "day" ) # round dates to just days

# get a copy of events for our individual student
st.events <- events

tail(events)
head(events)

####
# get a list of only students who passed 'pusers'
pass.users <- subset(users, users$E.MARK > 40, select=c(EID, E.MARK)) # I dont need all of the vectors
studentcount<-nrow(pass.users)

# now I have that list I can right join with the events for this course to filter only those students who passed in the list
# this will also get rid of staff events 
events <- merge(events, pass.users, by='EID', all.y=T) 
events <- events[order(events$EVENT_DATE),] # need to reorder on date (asc) after merge

# so now I want to plot overall activity to a given date

firstDate<-events$EVENT_DATE[1]  # gets the first date in the list
head(events)

hBreaks<-as.numeric(difftime(tDate, firstDate , units="days")) # calculates the number of days in the data
firstDate
head(events)
#par(mar=c(10,9,10,10))   # margins
hist(events$EVENT_DATE, hBreaks, las=2)


# then get count for each day
#dailyCount<- aggregate(events, by = list(as.character(events$EVENT_DATE)), length)
dailyCount<- aggregate(events, by = list(as.Date(events$EVENT_DATE)), length)

# to get the average per student then divide by number of students
dailyCount$EVENT2 <- dailyCount$EVENT / studentcount

studentcount
head(dailyCount)
str(dailyCount)

# dont need these two columns now
dailyCount$EVENT_DATE <- NULL
dailyCount$EID <- NULL


##########################
###
### work these out as averages? Or could use modes or boxplots :)
###
### styudentcount is the count of considered students
##########################
dailyCount$Day <- as.integer(row.names(dailyCount)) # equivalent of a day number field for comparisons

                             
ggplot(aes(x = Day, y = EVENT2), data = dailyCount) +
  geom_point() +
  geom_point(color='blue') + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Day', y = 'Average events', title='Average activity by date')


# dates as factors are not continuous here so I need to define a dummy group for ggplot to
# allow it to join the points

#ggplot(aes(x = Group.1, y = EVENT2, group=1), data = dailyCount) + 
ggplot(aes(x = Day, y = EVENT2, group=1), data = dailyCount) +   
  geom_line() +
  geom_line(color='blue') + 
#  scale_x_date(labels = date_format("%b-%d")) +
#  scale_x_date(labels = date_format("%d")) +
  scale_x_continuous(breaks=1:107) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Day', y = 'Average activity', title='Average activity by day')

#################################################################
# quick experiment to plot an individual against the average


my.Student <- '492119'

head(st.events)

# just get this student's records
my.events <- subset(st.events, users$EID == my.Student, select = c(EVENT_DATE, EVENT, EID))
# sort
my.events <- my.events[order(my.events$EVENT_DATE),] # need to reorder on date (asc) after merge

head(my.events)
tail(my.events)

# need to start plotting at the right start day

# then get count for each day
my.dailyCount<- aggregate(my.events, by = list(as.Date(my.events$EVENT_DATE)), length)
my.dailyCount$Day <- as.integer(row.names(my.dailyCount)) # equivalent of a day number field for comparisons

studentcount
head(my.dailyCount)
str(my.dailyCount)

# dont need these two columns now
my.dailyCount$EVENT_DATE <- NULL
my.dailyCount$EID <- NULL

# first go at two lines on same plot
# good code just might be missing days from indiv student
#ggplot(aes(x = Day, y = EVENT2, group=1), data = dailyCount) +   
#  geom_line() +
#  geom_line(color='blue') + 
#  geom_line(aes(x = Day, y = EVENT, group=1), data=my.dailyCount,color='red') +
#  scale_x_continuous(breaks=1:107) +
#  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#  labs(x = 'Day', y = 'Average activity', title='Average activity by day') 

# has problem I think where dates are not lining up


# using the actual date will line up better than what I have taken as the day count 
ggplot(aes(x = Group.1, y = EVENT2, group=1), data = dailyCount) + 
  geom_line() +
  geom_line(color='blue') + 
  scale_x_date(labels = date_format("%b-%d")) +
  #  scale_x_date(labels = date_format("%d")) +
  geom_line(aes(x = Group.1, y = EVENT, group=1), data=my.dailyCount,color='red') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = 'Day', y = 'Average activity', title='Average activity by day')



# this means to succesfully line up a student from another cohort I will need to convert dates into days from start!



#######
# now to pick an individual student and plot their pulse
#############

# load the current module
# transform data
# get records just for this student
# Day one is FromDate in original and first day in this
# so plot each day against appropriate point as far as we go


# load the current module

