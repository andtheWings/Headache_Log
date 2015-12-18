#Load needed libraries
library(dplyr)
library(ggvis)
library(lubridate)

library(reshape2)

#Input Headache Data

df = read.csv("headache.log.csv")
df$month = as.character(df$month)

#Convert Time to Play Nice
df$plot_date = mdy_hms(df$date)

#Trying to replicate with ggvis
df %>% 
  ggvis(~plot_date, ~pain, fill = ~factor(midEnd)) %>%  
  group_by(midEnd) %>% 
  layer_smooths(
    stroke = ~factor(midEnd), 
    span = input_slider(0.2, 1, label="Span of the fitted lines"),
    se = TRUE) %>%
  add_axis("x", title = "Date") %>%
  add_axis("y", title = "Pain Level") %>%
  add_legend(c("fill","stroke"), title="Time of Day") %>%
  scale_numeric("y", domain = c(1, 10), nice = FALSE)
  layer_points()

#Make dataframe for number of days exercising by month
dfexercise = dcast(df, month ~ exercise)

#Make dataframe for number of days with end pain above 6 by month
dfhighpain = filter(df, midEnd == "end") %>% filter(pain > 6) %>% count(month)

#Put new dataframes together
dfactivepain = full_join(dfexercise, dfhighpain, by = "month")

#Add variable (# of days above 6)/(# of days exercising)
dfactivepain$painVexercise = dfactivepain[,5]/dfactivepain[,3]


  
 
  

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