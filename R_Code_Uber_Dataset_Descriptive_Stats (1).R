# Lets Install the required libraries first

#install.packages("readxl")
#install.packages("readcsv")
#install.packages("lubridate")

# Lets load the required libraries

library(readxl)
library(readcsv)
# You can set working directory if you want or can provide the full folder details

uberdata <- read.csv("C:/Users/hp/Desktop/02_Next Steps/BABI Mentoring/01_May19 Batch/02_SMDM/Session 3_Week1/Dataset_Uber.csv")

# Let's view the data now

head(uberdata, 10)
tail(uberdata, 10)

dim(uberdata)
str(uberdata)
summary(uberdata)

# What are you observations from the data summary?
# What can you say about the overall distribution of data
# Do you notice any 'NA'?
# Do you notice any outliers?
# What else can you observe about the data?



# Let's look at the number of NAs

sum(is.na(uberdata))
nrow(uberdata)*ncol(uberdata)

sapply(uberdata, function(x) sum(is.na(x)))


# Let's do the "missing data treatment"

# We'll mark NA as a seperate category

uberdata$borough <- as.factor(replace(as.character(uberdata$borough), is.na(uberdata$borough),"Unknown"))
table(uberdata$borough)

# Let us break date into hour, day, month
uberdata$start_date = strptime(uberdata$pickup_dt,'%Y-%m-%d %H:%M')

library(lubridate)
uberdata$start_month <- month(uberdata$start_date)
uberdata$start_day <- day(uberdata$start_date)
uberdata$start_hour <- hour(uberdata$start_date)
uberdata$wday <- weekdays(uberdata$start_date)
uberdata <- uberdata[,-14]
head(uberdata,15)



by(uberdata, INDICES = uberdata$borough, FUN = summary)
by(uberdata, INDICES = uberdata$hday, FUN = summary)
   
#Let's try to get the no. of holidays in each month

library(htmlwidgets)
library(rpivotTable)
?rpivotTable

rpivotTable(uberdata)
rpivotTable(uberdata, rows = "start_month", cols = "hday", aggregatorName = "Count Unique Values")

# Let's validate if there are any holdays in Months 3 & 4
table(uberdata$hday,uberdata$start_month)

library(data.table)
?uniqueN

#If we need to find the number of unique days 

length(unique(uberdata$start_day))

# But we need to know the total no. of unique days for which data is captured

uniqueN(uberdata, by=c('start_month', 'start_day'))

#Another way using the rpivotTable - Just add the no. of days per month
rpivotTable(uberdata, rows = "start_month", aggregatorName = "Count Unique Values", vals = "start_day")

#Let's do some exploratory data analysis(EDA) using some charts

#Histograms
hist(uberdata$spd)
hist(uberdata$vsb)
hist(uberdata$pickups, col = "lightgreen")
hist(uberdata$vsb, main= "Visibility") # Almost clear weather 
hist(uberdata$temp, main="Temperature") # What do we see in the data - we can see a bimodal distribution



#Box Plots

boxplot(uberdata$spd, uberdata$hday, main = "Box plot of Wind speed by working day")

#Box Plot by factor
boxplot(uberdata$spd~uberdata$hday)
summary(uberdata$spd)
# Low speed for duration, except few outliers, avg is around 5

boxplot(uberdata$pcp01~uberdata$hday)
boxplot(uberdata$pcp24~uberdata$hday)

#Histograms in the same panel by factor

library(lattice)
histogram(~pickups|factor(hday), data = uberdata)
histogram(~pickups|factor(start_month), data = uberdata)
histogram(~sd|factor(start_month), data = uberdata)
histogram(~temp|factor(start_month), data = uberdata) # - See the shift in the temperature peaks by month


#Some additional ways to plot the histograms by factors

library(ggplot2)

ggplot(uberdata, aes(pickups)) +
  geom_histogram() +
  facet_wrap(~ borough, ncol = 3)


#Lets test some understanding

boxplot(uberdata[,c(4:7)])
#What is wrong with the box plots?

boxplot(uberdata[,c(9:12)])
#Does this make sense at all?


plot(density(uberdata$dewp), main="Dew point variations")
# Distribution looks similar to that of temperature (bi-modal)

plot(density(uberdata$sd), main="Snow depth")
# No snow for majority of times


#Lets look at some correlations - qualitatively
library(corrplot)
corrplot(cor(uberdata[,c(4,5,6,7,9,10,11,12)]))
corrplot(cor(uberdata[,c(4,5,6,7,9,10,11,12)]), method = "ellipse", title = "Correlation plat of some of the variables", diag = FALSE)

# Some additional plots


# Pickup vs wind speed
plot(uberdata$spd, uberdata$pickups, xlab= "speed", ylab="pickup", main ="pickup vs speed")
abline(lm(uberdata$pickups~uberdata$spd))
# Doesn't seems to be strong predictor for number of rides

plot(uberdata$vsb, uberdata$pickups, xlab= "visibility", ylab="pickup", main ="pickup vs visibility")
abline(lm(uberdata$pickups~uberdata$vsb))
# again not a srtong predictor

plot(aggregate(pickups~start_month,data=uberdata, sum), type="b")
plot(aggregate(pickups~start_day,data=uberdata, sum), type="b", main ="Sum of bookings for each day")


plot(aggregate(pickups~start_hour,data=uberdata, sum), type="b")

uberdata$wday = as.factor(uberdata$wday)
plot(aggregate(pickups~wday,data=uberdata, sum), type="b")


ggplot(aes(x = reorder(wday, pickups), y = pickups), data = uberdata) +
  geom_bar(aes(fill=pickups), width=0.5, stat = "identity") + coord_flip()

# low bookings on monday which rises till saturday and then drops off slightly on sunday

#Another way to plot histograms usine quick plots (qplot)

qplot(uberdata$temp, geom = "histogram", main = "Histogram of Temperature", binwidth = 15)
qplot(uberdata$dewp, geom = "histogram", main = "Histogram of Dew Point", binwidth = 15)

# Ways to plot combined histograms of multiple columns
library(psych)
data1 <- uberdata[,c(6,7)]
multi.hist(data1)

data2 <- uberdata[,c(9,10,11)]
multi.hist(data2)

data3 <- uberdata[,c(6,7,9,10)]
multi.hist(data3)

## ________________________________________________________________________________________