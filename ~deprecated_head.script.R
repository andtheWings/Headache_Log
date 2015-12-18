#Load needed packages
library(reshape)
library(ggplot2)

#Input Headache Data
head.log = read.csv("headache.log.new.csv")

#Convert Data Types
head.log$plot.date = mdy_hms(head.log$date)

#Plot Pain Over Time
qplot(head.log$plot.date, 
      head.log$pain, 
      colour = head.log$midEnd,
      shape = head.log$midEnd,
      geom = c("point", 
               "smooth"),
      ylim = c(0,10),
      xlab = "Time",
      ylab = "Pain Level")

#Plot Relative Pain Frequencies
barplot(table(head.log$pain)/length(head.log$pain))

#Independent 2-group t Tests
t.test(pain~bump, data=head.log)
t.test(pain~exercise, data=head.log) 
t.test(pain~sleep, data=head.log) 
t.test(pain~stress, data=head.log) 
t.test(pain~vidgames, data=head.log) 
t.test(pain~driving, data=head.log) 
t.test(pain~bike, data=head.log) 
t.test(pain~hotcold, data=head.log) 
t.test(pain~screen, data=head.log)
summary(aov(pain ~ caffeine))
summary(aov(pain ~ alcohol))