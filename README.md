# Assignment2_Orokoh


#Clean the workspace
rm(list = ls())

#Load Libraries
library(dplyr)
library(ggplot2)
library(haven)

#Set working directory
setwd("/Users/yaaangieb/Downloads/R Codes /FFCWS") 

#Import data
pr <- read_dta("/Users/yaaangieb/Downloads/R Codes /FFCWS/FF_wave5_2020v2.dta")

#Keeping variables of interest only
selected_vars <- pr %>% select(idnum, m5f23d, m5f23i, m5f1, m5f23h, m5f23c, m5f23g, 
                                m5i10, m5i17, m5i14a5, m5i14a2, m5i14a3, m5i14a4, 
                                m5i16a, cm5hhinc, m5i17, m5j1, cm5age, cm5edu, 
                                cm5povca, m5e1d, m5i14c, m5f7_105, m5f7_107, 
                                m5f7_106, m5f8a3, m5i12a_code, f5d2b, cm5marp, 
                                cm5marf, m5f5, m5j9i, f5f8a1, m5i13, m5i13p, m5h3, 
                                m5d2d, m5f2, m5f1a, m5f3b, m5f4, m5f6, 
                                cm5marp, cm5marf, cm5cohp, cm5relf, cm5marp, 
                                cm5marf, cm5cohf, cm5cohp)
#Inspecting the selected vars
View(selected_vars)

#droping all participants not in this wave & Replacing -6 with NA
sv <- selected_vars[!apply( selected_vars== -9, 1, any), ]
sv[sv == -6] <- NA
sv[sv == -1] <- NA
sv[sv == -2] <- NA
sv[sv == -3] <- NA

#Changing Varaible types
sv$homeless <- as.factor(sv$m5f23i)
sv$number_Hours_Worked <- as.factor(sv$m5i10)
sv$evenings <- as.factor(sv$m5i14a2)
sv$nights <- as.factor(sv$m5i14a3)
sv$weekends <- as.factor(sv$m5i14a4)
sv$irregular_Schedule<- as.factor(sv$m5i14a5)
sv$schedule_stress <- as.factor(sv$m5i16a)
sv$multiple_jobs <- as.factor(sv$m5i16a)
sv$constructed_income <- as.factor(sv$cm5hhinc)
sv$edu_constructed <- as.factor(sv$cm5edu)
sv$age_constructed <- as.factor(sv$cm5age)
sv$occupation <- as.factor(sv$m5i12a_code)
sv$multiple_moves <- as.factor(sv$m5f1)
sv$no_times_moved <- as.factor(sv$m5f1a)
sv$rent_or_own <- as.factor(sv$m5f2)
sv$eviction <- as.factor(sv$m5f23d)
sv$doubled_up <- as.factor(sv$m5f23h)
sv$missed_payment <- as.factor(sv$m5f23c)


#Data Visualization
#Investigating variable
t <- table(sv$Homeless)
print(t)
round(100*prop.table(t),digits = 1)

#Visualizing it
ggplot(data = sv,mapping = aes(doubled_up)) + geom_bar() + 
  labs(title = "Distribution of Doubled Up",
       x="Doubled Up (1=yes,2=No)",
       y= "Distribution"
  )
theme_minimal()
