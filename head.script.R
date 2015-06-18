#Load needed libraries
library(ggplot2)
library(lubridate)
library(dplyr)
library(reshape2)
library(ggvis)

#Input Headache Data
df = read.csv("headache.log.csv")
df$month = as.character(df$month)

#Make dataframe for number of days exercising per month
dfexercise = dcast (df, month ~ exercise)

#Make dataframe for number of days with end pain above 6 per month
dfhighpain = filter(df, midEnd == "end") %>% filter(pain > 6) %>% count(month)

#Put new dataframes together
dfactivepain = full_join(dfexercise, dfhighpain, by = "month")

#Add variable (# of days above 6)/(# of days exercising)
dfactivepain$painVexercise = dfactivepain[,5]/dfactivepain[,3]

#Convert Time to Play Nice
df$plot_date = mdy_hms(df$date)

#Plot Pain Over Time
Time_of_Day <- df$midEnd 
qplot(df$plot.date, df$pain, 
      colour = Time_of_Day,
      shape = Time_of_Day,
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
barplot(table(df$pain)/length(df$pain), legend=df$month)

#Independent 2-group t Tests
t.test(pain~bump, data=df)
t.test(pain~exercise, data=df) 
t.test(pain~lessthanfive, data=df) 
t.test(pain~stress, data=df) 
t.test(pain~run, data=df) 
t.test(pain~strength, data=df) 
summary(aov(pain ~ caffeine, data=df))
summary(aov(pain ~ alcohol, data=df))