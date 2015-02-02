#Load needed packages
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)

#Input Headache Data
df = read.csv("headache.log.csv")

#Make dataframe for number of days exercising per month
dfexercise = dcast (headache.log, month ~ exercise)

#Make dataframe for number of days with end pain above 6 per month
dfhighpain = filter(headache.log, midEnd == "end") %>% filter(pain > 6) %>% count(month)

#Put new dataframes together
dfactivepain = full_join(dfexercise, dfhighpain, by = "month")

#Add variable (# of days above 6)/(# of days exercising)
dfactivepain$painVexercise = dfactivepain[,5]/dfactivepain[,3]

#Convert Time to Play Nice
headache.log$plot.date = mdy_hms(headache.log$date)

#Plot Pain Over Time
qplot(headache.log$plot.date, 
      headache.log$pain, 
      colour = headache.log$midEnd,
      shape = headache.log$midEnd,
      geom = c("point", 
               "smooth"),
      ylim = c(0,10),
      xlab = "Time",
      ylab = "Pain Level")

#Plot Relative Pain Frequencies
barplot(table(head.log$pain)/length(head.log$pain))

#Independent 2-group t Tests
t.test(pain~bump, data=headache.log)
t.test(pain~exercise, data=headache.log) 
t.test(pain~sleep, data=headache.log) 
t.test(pain~stress, data=headache.log) 
t.test(pain~vidgames, data=headache.log) 
t.test(pain~driving, data=headache.log) 
t.test(pain~bike, data=headache.log) 
t.test(pain~hotcold, data=headache.log) 
t.test(pain~screen, data=headache.log)
summary(aov(pain ~ caffeine, data=headache.log))
summary(aov(pain ~ alcohol))