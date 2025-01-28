# Assignment2_Orokoh

While the chaos of housing insecurity, forced moves, eviction, financial stress, living with family members, or frequent moves may impede how one acts or preforms in the workplace (Desmond and Gershenson, 2016; Marcal et. al, 2023), less is known about how the workplace, may impede or even onset, housing instability. In investigating beyond mere measures of income, the present study aims to investigate the association between precarious employment conditions and housing instability. Specifically, the following research question is tested. 

            A.)	Does employment precarity predict housing instability subtypes? 
            B.)	Is there an association between employment precarity and housing instability subtypes? 

Measures and Methods: 

Data 

The sample population comes from the Future of Families and Wellbeing Study (FFCW). The FFCW is a longitudinal study which follows families with children who were born between 1998-2000 in 20 large U.S. cities. Cities included were cities with over 200,000 residents and hospitals were randomly selected, with an intentional over sampling of births to unmarried parents. Mothers and fathers were interviewed shortly after giving birth in the hospital and again at 1-, 3-, 5-, 9-, 15, and 20 year follow ups. Due to the oversampling of families from socio-demographically disadvantaged backgrounds, the sample has a high concentration of African Americans and Latino populations. The present study uses data from wave five, year nine which were collected during the years 2007-2010. Data on mothers and fathers were collected separately and the analytic sample in this study was limited to the mother’s responses only. 

Measures

Dependent Variable: Housing Instability: 

I defined housing instability as any mother who self-reported issues related to eviction, missed housing payments, homelessness, doubling up, and recent moves. The FFCWS ask mother’s if they “Did not pay full amount of rent/mortgage payments in past 12 months, ” “Were evicted from home/apartment for not paying rent in past 12 months”, “Moved in with other people because of financial problems in past 12 months”, “Stayed at shelter/other place not meant for regular housing in past 12 months”, and if “Mother has moved in the last four years.” Housing instability was measured dichotomously (0,1). We coded mothers as unstably housed if respondents reported yes to each housing instability indicator. 

Independent Variable: Employment Precarity

Scholars have referred to precarious work arrangements in several ways–as nonstandard work, alternative work arrangements, flexible staffing, nontraditional, peripheral employment, contingent work, and even disposable work (Kalleberg, 2000). Despite this variation in nomenclature, precarity has dominated recent labor literature as a term that best describes work arrangements that 1) deviate from what is considered a standard employment arrangement, “in which it was generally expected that work was done full-time, would continue indefinitely, and was performed at the employer’s place of business under the employer’s direction” (Kalleberg, 2000, p. 341) and; 2) where employment “is uncertain, unpredictable, and risky from the point of view of the worker”, often resulting in “distress” (Kalleberg, 2009, p. 2)




Graphs: 


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

Graph 1: 

#Visualizing it
ggplot(data = sv,mapping = aes(doubled_up)) + geom_bar() + 
  labs(title = "Distribution of Doubled Up",
       x="Doubled Up (1=yes,2=No)",
       y= "Distribution"
  )
theme_minimal()


#Rationale 
Graph one is a simple distribution of people within the Future of Families and Child WellBeing Dataset. I use a bar graph to explore the distribution and the 
amount of people who are classified as doubled-up. Doubled up is a term which refers to those who are not quite homeless, but are living with others. This is a 
population of interest because it is frwuently a population that is overlooked by homelessness data. Those who are considered 'doubled-up' are not classified as 
homeless. This is also a subset of interest because those who are doubled up are often in precarious working conditions. Because precarious employment is of interest in my 
study, I want to know if those who are in precarious households are more likely to be doubled_up. 


Graph 2: 

ggplot(data = sv,mapping = aes(total)) +geom_histogram(color='grey') +
  labs(title = "Distribution of income",
       x= "Income",
       y= "Amount ($)"
  )
This graph will explore the income distribution of the sample. 

Graph 3: 

  #Comparing the two plots
ggplot(data = sv,mapping = aes(x=missed_payment,y=multiple_jobs)) +geom_boxplot() + labs(
  title = "Missed mortgage/rent payment by Multiple Jobs",
  x= "missed mortgage/rent payment(1-yes, 2=No)",
  y= "Multiple Jobs"
)
theme_minimal()

