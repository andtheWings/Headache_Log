#Load needed packages
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(ggvis)

#Input Headache Data
df = read.csv("headache.log.csv")

#Make dataframe for number of days exercising per month
dfexercise = dcast (df, month ~ exercise)

#Make dataframe for number of days with end pain above 6 per month
dfhighpain = filter(df, midEnd == "end") %>% filter(pain > 6) %>% count(month)

#Put new dataframes together
dfactivepain = full_join(dfexercise, dfhighpain, by = "month")

#Add variable (# of days above 6)/(# of days exercising)
dfactivepain$painVexercise = dfactivepain[,5]/dfactivepain[,3]

#Convert Time to Play Nice
df$plot.date = mdy_hms(df$date)

#Plot Pain Over Time
qplot(df$plot.date, df$pain, 
      colour = df$midEnd,
      shape = df$midEnd,
      geom = c("point", 
               "smooth"),
      ylim = c(0,10),
      xlab = "Time",
      ylab = "Pain Level")

#Trying to replicate with ggvis
df %>% 
  ggvis(~plot.date, ~pain, fill = ~factor(midEnd)) %>% 
  layer_points() %>% 
  group_by(midEnd) %>% 
  layer_smooths(
    stroke = ~factor(midEnd), 
    span = input_slider(0.2, 1),
    se = TRUE)

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