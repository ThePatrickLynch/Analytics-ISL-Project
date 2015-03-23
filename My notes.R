# indicates a comment
# to make command jump to console and execute use CTRL-R

2+2 # product

2 +
2 # hmm can span lines - odd behaviour

# creating vectors

drink=scan() # reads in a list of numbers types?

names<-c('Magnus','Bob','Alex','Lisa','Rosie','John')
    	# <- is the same as =
	# c is collection
	# names is a 'factor'
	# beware case sensitive
drink=c(2,0,1,1,1,2)


# history(n) shows last n commands history(4) 
# CTRL-R can also add only highlighted lines


ls() # tells you everything in memory
rm(drink) # remove from memory
rm(list=ls()) # remove everything

drink[1] # shows the contents of first item in the list
drink[4:6] # 4-6
drink[c(4,1,5)] # selecting multiple items
drink[6]=3 # set a value 

# up arrow skips back though previous commands

# if the response console is '+' it means something is missing in the command

### FILES ###

# will read a range of files
# csv preferred must add NA when data is not available
# spaces can turn numbers into factors because it sees txt

# dataset eye

eye=read.csv(file.choose())  # dont use a name that matches on of your factors
		# could put filename/location instead of choose, but file chooser dialog make life easier

# help for a given command e.g. ? read.csv will show the syntax

names(eye) # tells us the names of all of the columns

str(eye) # data frame structure - nice overview of the data

summary(eye) # provides a summary of an object

# really good normal dist would have median and mean the same

head(eye)    # head and tail show you the top or bottom items in the file
head(eye,10)
tail(eye)


eye[,c(1,5)]  # gives all rows 1 and 5 column
eye[,c("family","water")] # can use names
eye[c(1:10),c(1,5)] # rows 1-10 columns 1 and 5
eye[c(10:50),] # rows 10-50 all columns

eye.nonsense=eye[c(1:10),c(1,5)] # creates a new object from subset
	# eye.nonsense is just a name could be just nonsense for example. naming convention shows derivation

write.csv(eye.nonsense,"test.csv") # writes a csv of the subset to the working directory

# Set the working directory
setwd("
C:/Data")
getwd()

### PLOTS ###

boxplot(eye$ed~eye$depthc) 

boxplot(eye$ed~eye$depthc, data=eye) # alternate

# or attach

attach(eye)  # adds the object eye to the search path
            # attach not preferred as you may use same field name in multiple files

detach(eye)  # removes from search path


# more options

boxplot(eye$ed~eye$depthc,main="Eye diameter by depth",xlab="Depth class", ylab="Eye diameter (mm)", col="cornsilk1") 

# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf



boxplot(eye$ed~eye$depthc,main="Eye diameter by depth",xlab="Depth class", 
        ylab="Eye diameter (mm)", col=c("red","blue","yellow","green")) # list of colours

boxplot(eye$ed~eye$depthc,main="Eye diameter by depth",xlab="Depth class", 
        ylab="Eye diameter (mm)", col=c("red","blue")) # two will alternate



boxplot(eye$ed~eye$depthc,main="Eye diameter by depth",xlab="Depth class", 
        ylab="Eye diameter (mm)", col=c("red","blue","yellow","green"), border="orange") # list of colours


## parametric statistics assume data is normally distributed

qqnorm(eye$ed)  # quantile quantile plot 

qqline(eye$ed) # plots a line

qqnorm(log(eye$ed))  # logs make it easier to see relationships

qqline(log(eye$ed))

shapiro.test(eye$ed)  # a low p value says its not normally distributed less than .05
shapiro.test(log(eye$ed))

hist(eye$ed)  # histogram

par(mfrow=c(1,2)) # separates the graphic window into 2
                  # next plots will be show 1 row two columns = side by side

hist(eye$ed)  # histogram
hist(log(eye$ed))  # histogram

table(eye$habitat) # count of values

eye$habitat # shows contents of habitat column
class(eye$habitat) # what type of object is that item
class(eye) # that object

habitatn=as.numeric(eye$habitat) # new vector of numeric version separate to rest of data
eye$habitatn=as.numeric(eye$habitat) # adds new column to the data

tab.habitat=table(eye$habitat) # puts data into a table labelled chi.habitat shows count
tab.habitat # if randomly distributed then each area should be same 28 from those not NA
summary(eye$habitat) # will also show NA

chisq.test(tab.habitat) # low value for chi means good random distribution 
# p-value is the probability of random distribution

shrimp.chi=chisq.test(tab.habitat)
shrimp.chi$exp # expected outcome for random
summary(shrimp.chi)
shrimp.chi$obs # observed
shrimp.chi$sdev 

ed.cl=eye$ed/eye$cl #ratio of ed to cl

boxplot(ed.cl~eye$depthc)


plot(eye) # scatter plot everything
eye.numeric=eye[c(3,6,8,13,14,15,16)] # lets just get numeric values
plot(eye.numeric)

plot(eye$cl~eye$ed) # bivariate scatterplot y,x axis

     
# quick test for normal distribution
qqnorm(eye$ed)
eye$log.ed=log(eye$ed)
qqnorm(eye$log.ed)

plot(log(eye$ed), log(eye$cl), pch=17, col=c("red","green")[eye$ds]) # more parameters pch is point characters

# can vary pch and col based on other parameters

plot(log(eye$ed), log(eye$cl), pch=c(17,15,19)[eye$habitat], col=c("red","green")[eye$ds])

# vary size by depth

plot(log(eye$ed), log(eye$cl), pch=c(17,15,19)[eye$habitat], col=c("red","green")[eye$ds], cex=log(eye$depth))

# Useful site www.statmethods.net

mean(eye$ed)
median(eye$ed)
var(eye$ed)
quantile(eye$ed)
sd(eye$ed)
stem(eye$ed) #like a histgram, but shows info

hist(eye$ed)


#summarising data by factor

agg.ed.by.ds=aggregate(eye$ed, list(eye$ds), mean) # aggregates the mean of values for each factor
colnames(agg.ed.by.ds)=c("DS","eye_diameter") # change column names
barplot(agg.ed.by.ds$eye_diameter,names.arg=c("N","Y"), col=c("Red","Blue"))

#or

barplot(agg.ed.by.ds$eye_diameter,names.arg=agg.ed.by.ds$DS, col=c("Red","Blue"))

barplot(agg.ed.by.ds$eye_diameter,names.arg=c("N","Y"), col=c("Red","Blue"), ylab="Mean eye diameter", xlab="Dorsal spot", ylim=c(0,2))

agg.edbyds.sd=aggregate(eye$ed, list(eye$ds), sd) # aggregate stdev


# using arrows to draw error lines????

### HOMEWORK look at text editor sublime
plot a barplot with error bars


locator(3) # lets you find coordinates on a plot


#################################################################
# Day 2
#################################################################

eye=read.csv(file.choose())
str(eye)
names(eye)
eye[c(1:5,7)]
summary(eye)
eye[c(1:3,7,1),c(1:5)] # first 5 columns, rows 1-3 and 7 and 1

qqnorm(eye$ed)  # graph for normal dist
log.ed=log(eye$ed)
qqnorm(log.ed) # better view for correlation?
shapiro.test(log.ed) # test for normal distribution
ks.test(log.ed,rnorm(98))   # tests against generated value Kolmogorov-Smirnov test
                            # 98 is size of sample

set.seed(42)                # random number seed

# rnorm(n,mean=n2,sd=n3) 
my.normdist=rnorm(n=1000,mean=25,sd=5)   # syntax for rnorm which generates a random normal distribution
mean(my.normdist)
sd(my.normdist)
summary(my.normdist)

plot(my.normdist)

par(mfrow=c(1,2))  # mfrows is multiple figures in a row
ndist1=rnorm(n=10,mean=25,sd=5)
qqnorm(ndist1)
qqline(ndist1)
ndist2=rnorm(n=1000,mean=25,sd=5)
qqnorm(ndist2)
qqline(ndist2)

locator(1)     # find one location on plot
text(1.104,23.4,"b")   # puts text 'b' at x,y coordinates on current plot

place=locator(1)
text(place,"here")

place=locator(3)
text(place,"here")


# play with loops here
for (n in 1:5)
{
place=locator(1)
text(place, n)
#  sqr.squared[n] = sqr[n]^2 # try locate and numbered label
}



# stack overflow good site

#### histogram of a normal dist - generated

norm1=rnorm(n=100,mean=50,sd=10) # gives a random normal distribution
norm2=rnorm(100,80,10)

norm1a=hist(norm1) # creates histogram of rnorm
norm2a=hist(norm2)
plot(norm1a,col=rgb(0,0,1,.25),xlim=c(0,120))      # plots, last rgb part is transparency
plot(norm2a,col=rgb(0,0,1,.25),xlim=c(0,120)) 


#on same chart
plot(norm1a,col=rgb(0,0,1,.25),xlim=c(0,120))
plot(norm2a,col=rgb(1,0,1,.25),xlim=c(0,120),add=T)   # add=t allows adding to existing plot

# test for match

t.test(norm1,norm2)  # higher the magnitude of t the higher the difference in means, the p-value i how identicle they might be
                    # uses a non standard t test - a welsh test?

###Bar plots with error bars in 5 minutes

## need to look at markdown stuff

means=tapply(eye$ed,eye$ds,mean) # gives the means for with a dorsal spot and those without
sds=tapply(eye$ed,eye$ds,sd)

b=barplot(means,ylim=c(min(0),max(pretty(means+sds))))   #does it need pretty, no?

#simpler, plus colours and labels
b=barplot(means,ylim=c(min(0),max(means+sds)),col=c("grey","yellow"),ylab="Mean eye diameter (mm)", xlab="Dorsal spot")

#add error bars
arrows(b,means+sds,b,means-sds,angle=90,code=3)  # b means plotting on this barplot

# or if you only want positive error bars then take out the -sds
arrows(b,means+sds,b,means-0,angle=90,code=3,col="red")

box(bty="l")  # 'l' not 1  # puts a line along the bottom

####### Histograms

rm(list=ls()) # just clear everything

# lobster database uses 'N/A' as a value this is not the same as 'NA'

lobs=read.csv(file.choose())   
str(lobsters)
names(lobs)
head(lobs,10)
boxplot(lobs$size~lobs$species) # non normal the boxplot shows nothing useful

# just want lobsters
attach(lobs)
lobster=lobs[which(species=="lobster"),]  
# get a bit of info
class(lobster) # its a data.frame
str(lobster)
head(lobster)

# do a histogram
lh=hist(lobster$size,col="grey",xlab="Size (mm)") # hist of lobsters
# get info on the histogram
lh$breaks    # shows breaks = bin size
lh$counts   # counts in each bin

# sometimes best to switch axes
lbp=barplot(lh$counts,horiz=TRUE, col="grey", xlim=c(0,1600))
axis(2,at=lbp,labels=lh$breaks[-1],las=2) # label y axis

# add minimum landing size to plot
lh=hist(lobster$size,col="grey",xlab="Size (mm)",prob=TRUE)   # prob=TRUE needed as its probability or density (adds up to one) not frequency histogram
abline(v=87,col="red",lty=1,lwd=2)   # plot a line at 87 vertically on x axis

curve(dnorm(x,mean=mean(lobster$size),sd=sd(lobster$size)), add=TRUE, lwd=2, lty=1, col="blue")


### ***** aside over lunch k means clustering - apparently built in data for flowers
### ***** lookup rcharts

par(mfrow=c(1,2))


### fitted line - will work on any plot
lh2=hist(lobster$size,col="grey",xlab="Size (mm)") # hist of lobsters
my.multiplier=lh2$counts/lh2$density   # this is the height per unit of the chart its a vector because there are lots
my.density=density(lobster$size) # this returns a table with x and y values for each bin
my.density$y=my.density$y*my.multiplier[1] # multiplies every item in the y vector by the multiplier (we use the first as they are all the same)

lines(my.density,lwd=2,lty=1,col="blue")   ## generates 

# changing bin (or breaks) size

lh2$breaks

lh2=hist(lobster$size,breaks=32, col="grey") # sets number of bins = 32 equal sized

my.multiplier=lh2$counts/lh2$density   # this is the height per unit of the chart its a vector because there are lots
my.density=density(lobster$size) # this returns a table with x and y values for each bin
my.density$y=my.density$y*my.multiplier[1] #multiplies every item in the y vector by the multiplier (we use the first as they are all the same)

lines(my.density,lwd=2,lty=1,col="blue")   

##### next thing
cpue=read.csv(file.choose())   # catch per unit of effort (per pot)
str(cpue)
head(cpue)
str(cpue)

barplot(cpue$lobcpue,names.arg=sort(cpue$site),las=2, ylim=c(0.0,5.0)) # names.arg addslabels. LAS makes the labels vertical

barplot(cpue$loblpue,ylim=c(0.0,5.0), col="black", yaxt="n") #y axis title is blank

# plot them together to show cpue versus landing pue
barplot(cpue$lobcpue,names.arg=sort(cpue$site),las=2, ylim=c(0.0,5.0)) 

par(new=TRUE)
barplot(cpue$loblpue,ylim=c(0.0,5.0), col="black", yaxt="n", add=TRUE)
 # could use locator(1)
legend("topright",legend=c("CPUE","LPUE"),fill=c("grey","black"))
abline(h=0)  # same as 
              # box(bty="l")  # 'l' not 1  # puts a line along the bottom

### can i write an lti connector to a page for students?

## magnus advises exploring par more for setting up plotting

#################################### next ########################
### t-tests and stuff? ###

eye=read.csv(file.choose())

boxplot(eye$ed~eye$ds)   # eye diameter of animals that has a ds and those that don't
boxplot(ed~ds, data=eye) # is same as above

eye$loged=log(eye$ed)
boxplot(eye$loged~eye$ds)

# t.test will tell us if these are significantly different
t.test(eye$loged~eye$ds)
# t.tests good for small upto say 30
# paired t-test is good for before and after experiment test

# for parametric statistics normally dist data and variances equal - so should test variancies and normality
var.test(eye$loged~eye$ds)

## read Field - pschologist? Books on R



##### installing packages
# menu Packages, install package
# using needs
library(lawstat)


# remove.packages("car","C:/Users/pdspl/Documents/R/win-library/3.1") # to unistall


? levene.test # came in from lawstat so now can get help

levene.test(loged$ds, data=eye)

## murray logan book good for teaching
## Andy Fields 

library(ggplot2) ### just seeing what else

### linear models
eye$loged=log(eye$ed)
eye$logcl=log(eye$cl)

par(mfrow=c(1,2))
boxplot(loged~ds,data=eye,ylab="Eye diameter", xlab="presence of DS")
plot(loged~logcl,data=eye,xlab="Carapace length", ylab="Eye diameter") 


### from here not right ,,,,,

## is there a difference in eye size based on carapace length and wether they has a ds or not
plot(loged~logcl,data=eye,xlab="Carapace length", ylab="Eye diameter",pch=as.numeric(eye$ds), col=as.numeric(unique(eye$ds)) 
    # pch makes different shape point for ds and not ds
    # needs to be numeric

legend("topleft", inset=0.05,legend=c("No","Yes"),pch=c(1,2), col=c(1,2))  # inset tidies a bit

## need to look again at the above it is wrong ,,,,,,


###linear model - bivariate model

edcl.lm=lm(loged~logcl, data=eye)  # lm=linear model
summary(edcl.lm)   # the closer th r squared value is to 1 the stronger the relationship. 
                    # 1 is perfect, 0 is random.  
                    # F-statistic needs to be high for a good model. df high means a better model
                    # p-value has to be less than 0.05

abline(edcl.lm)  # adds a parametric line of best fit

## which points biggest influence of the line?
par(mfrow=c(2,2))
plot(edcl.lm)
## need to understand what these mean

edclds.lm1=aov(loged~ds+logcl, data=eye)   # two way anova
summary(edclds.lm1)  # so we see a relationship between ds and ed and cl and ed, but not looking at the three together

edclds.lm2=aov(loged~ds*logcl, data=eye)   # two way anova with interaction
summary(edclds.lm2)  # ds:logc1>0.05 so not significant

anova(edclds.lm1,edclds.lm2)  # so pr 0.06267 (>0.05) means no significant difference between models

# dont understand above

# three pages on eBridge needs reading tonight


###########################################################
#            Radial plots                                 #
###########################################################

### this is the fun bit apparently !!! Radial plots
rm(list=ls())
neph=read.csv(file.choose())

str(neph)
## using radial which is in package called plottrix
library(plotrix)
radial.plot(neph$distance, neph$rads,rp.type="s", point.symbol=20, start=0, point.col="grey")
## can tidy up and add features
radial.plot(neph$distance, neph$rads,rp.type="s", point.symbol=20,
            start=0, point.col="grey", show.radial.grid=TRUE, 
            radial.lim=c(0,30), radial.labels="", labels="")

fem=neph[which(neph$sex=="female"),]
male=neph[which(neph$sex=="male"),]
non1=neph[which(neph$remarks=="empty burrow"),]
non2=neph[which(neph$sex=="untagged"),]

radial.plot(neph$distance, neph$rads,rp.type="s", point.symbol=3, 
            cex=2, start=0, point.col="black", add=TRUE, show.grid=TRUE, radial.lim=c(0,30))

radial.plot(fem$distance, fem$rads,rp.type="s", point.symbol=16, 
            cex=2, start=0, point.col="pink", add=TRUE, show.grid=TRUE, radial.lim=c(0,30))

radial.plot(male$distance, male$rads,rp.type="s", point.symbol=16, 
            cex=2, start=0, point.col="blue", add=TRUE, show.grid=TRUE, radial.lim=c(0,30))

radial.plot(non2$distance, non2$rads,rp.type="s", point.symbol=20, 
            cex=2, start=0, point.col="green", add=TRUE, show.grid=TRUE, radial.lim=c(0,30))

radial.plot(non1$distance, non1$rads,rp.type="s", point.symbol=20, 
            cex=2, start=0, point.col="grey", add=TRUE, show.grid=TRUE, radial.lim=c(0,30))

########################################################
#                       Day 3                          #
########################################################

rm(list=ls())
fish=read.csv(file.choose())
str(fish)
head(fish)
names(fish)

# what factors control the number of species of fish in a country?

summary(fish$fishno)

#need car package

library(car)

#  [1] "country"       "landarea"      "fwarea"  freashwater      "coastline"     "mpaarea"  marine protected area     "mpano"   mpa number      "lat"           "long"         
# [9] "fishno"    total fish    "fishfw"    fish fresh water    "fishsw"        "fishingtro"    "fishend"       "fishthreat"    "fishreef"   fidh on reefs   "percend"     endemic 
# [17] "percthr"  threatened     "percint"    introduced   "comfish"       "fishemp"       "ssfish"        "greywatfoot"   "hdi"           "popdens"      
# [25] "rain"          "percmpa"       "freshwithdraw" "envscipapers" 


# what things might account for fish numbers?
fish.num=fish[c(9,2,3,4,5,6,7,8,22,23,24)]  # picks out sensible fields to look at
plot(fish.num) # quick look at all - doesnt show anything obvious

scatterplotMatrix(fish.num)

scatterplotMatrix(fish.num,diag="boxplot")

# lets just plot two bigger
par(mfrow=c(2,2))
boxplot(fish.num$fishno,main="Fish no", col="blue")
boxplot(fish.num$lat,main="Lat", col="blue")
boxplot(fish.num$coastline,main="Coastline", col="blue")
boxplot(fish.num$hdi,main="Hdi", col="blue")


# boxplot - median line middle, box either side equal ish, tails same sort of length then is normal
# non normal need log'ing to normalise

fish.num$landarea=log(fish.num$landarea)
fish.num$fishno=log(fish.num$fishno)
fish.num$fwarea=log(fish.num$fwarea+0.2)
fish.num$coastline=log(fish.num$coastline+0.1)
fish.num$mpaarea=log(fish.num$mpaarea)
fish.num$greywatfoot=log(fish.num$greywatfoot+0.01)  # has some zeroes so needs shifting as log cant use 0 - could be problematic if predicting
fish.num$popdens=log(fish.num$popdens)
fish.num$mpano=log(fish.num$mpano)

scatterplotMatrix(fish.num,diag="boxplot")

## helps us see where two factors have a relationship which shouldn't be used coastline is related to mpano. no point using both
## y is fishno and we dont want any of the others to be correlated
# testing for correlation

# pearson non parametric
? cor

cor(fish.num,use="complete.obs")

# looking for a high coreelation coeffient value in the resultany near 1 or -1 we dont want
# magnus suggest anything over .5


cor(fish.num[-1],use="complete.obs") # takes out first column
# OR
cor(fish.num[-c(1,3,5,6)],use="complete.obs") # get rid of x plus the 

## now we know how the data needs cleaning data is cleaner we can recreate from scratch

fish.num2=fish[c(9,4,7,8,22,23,24)]

# could do this in a loop

fish.num2$coastline=log(fish.num2$coastline+0.1)
fish.num2$greywatfoot=log(fish.num$greywatfoot+0.01)  # has some zeroes so needs shifting as log cant use 0 - could be problematic if predicting
fish.num2$popdens=log(fish.num$popdens)
fish.num2$fishno=log(fish.num$fishno)

scatterplotMatrix(fish.num2,diag="boxplot")

fish.num3=fish.num2[complete.cases(fish.num2*0),,drop=FALSE] # get rid of incomplete records  
#     - THIS LINE IS WRONG CHECK MAGNUSSES - concerned if we need to redo logs or not

summary(fish.num3)

fish.num.lm1=lm(fishno~coastline+lat+long+greywatfoot+hdi+popdens,data=fish.num3) # linear model
summary(fish.num.lm1) # results show very small values for lat and coastline therefore they are important as hdi is picked out
## 3 stars less than .001

# wondering if absolute latitude as opposed to north/south +/- might make a difference
fish.num3$lata=abs(fish.num3$lat)
fish.num.lm2=lm2(fishno~coastline+long+greywatfoot+hdi+popdens+lata,data=fish.num3)
summary(fish.num.lm2)

#what if we do same with long
#fish.num3$longa=abs(fish.num3$long)
#fish.num.lm2=lm(fishno~coastline+long+greywatfoot+hdi+popdens+lata+longa,data=fish.num3)
#summary(fish.num.lm2)

## latitude and coastline best describe the number of fish species in an area
# changes affect other results so losing least intersting

fish.num.lm3=lm(fishno~coastline+greywatfoot+hdi+popdens+lata,data=fish.num3)
summary(fish.num.lm3)

step(fish.num.lm3,direction = "backward")
# lowest aic number is best model fishno ~ coastline + lata  --  this may change when line further up works properly


#compare models
anova(fish.num.lm1,fish.num.lm2,fish.num.lm3)  # lowest p-value is best
plot(fish.num.lm2)

influence.measures(fish.num.lm2) # identifies values that are influencing the model - go look

#####################################
# Maps
#####################################

rm(list=ls())

# load packages ggmap, mapproj, rworldmap, rworldxtra
library(ggmap)
library(mapproj)
library(rworldmap)
library(rworldxtra)

map1=get_map(location="YO113AZ", zoom=17, maptype="satellite") # can add ,city, country or postcode or lat,long
ggmap(map1)

fleet=read.csv(file.choose())   #cpue file

newmap=getMap(resolution="high") # world map
plot(newmap)

UK.limits=geocode(c("Shetland","Isles of Scilly","Rockall","Lowestoft"))  # calculating map limits, can use usual google refs
UK.limits  # shows limits
plot(newmap, xlim=range(UK.limits$lon),ylim=range(UK.limits$lat))

holderness=geocode(c("53.614902N,0.212588W",
                     "54.095777N,0.508468W",
                     "53.614902N,0.23586E", 
                     "54.095777N,0.23586W"))

plot(newmap, xlim=range(holderness$lon),ylim=range(holderness$lat))

points(fleet$lon,fleet$lat,col="red",cex=as.numeric(fleet$lobcpue), pch=20)
points(fleet$lon,fleet$lat,col=as.numeric(fleet$soak),cex=as.numeric(fleet$lobcpue), pch=20)

# add a text
brid=locator(1)  # pick a point
text(brid,"Bridlington")

## european health authority have a good site for dos and donts for charts

## neural network in r?


####################
# confidence levels - clustering
####################

#fishcoast=subset(fish$coastline>0, select=c("fishno","coastline"))
#log.fishno=log(fishcoast$fishno)
#log.coastline=log(fishcoast$coastline)

# magnus will do a couple of videos covering this bit # out of time due to crash
# euclian distance between samples




