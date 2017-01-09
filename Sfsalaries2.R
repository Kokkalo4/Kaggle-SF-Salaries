#all analysis here was based on the analysis of SF Salaries data set from users msjgriffiths and JustinDomingue


#loading of packages and libraries
library(plyr)
library(dplyr)
library(GGally)
library(gender)
library(readr)
library(ggplot2)
library(genderdata)
library(tidyr)

# read csv on SF salaries data with readr package set not provided to NA from start
salary_data1 <- read_csv("..../input/salaries.csv",na=c("Not Provided"))

#check basic outline of data

str(salary_data1)
summary(salary_data1)
describe(salary_data1)
head(salary_data1)
tail(salary_data1)
glimpse(salary_data1)

#cleaning data

#remove id , Agency and Notes column

table(duplicated(salary_data1$Id))
salary_data1 <- salary_data1 %>%
  select(-Id, -Agency, -Notes)

#split data by year

salary2011 <- salary_data1[which(salary_data1$Year==2011), ]
salary2012 <- salary_data1[which(salary_data1$Year==2012), ]
salary2013 <- salary_data1[which(salary_data1$Year==2013), ]
salary2014 <- salary_data1[which(salary_data1$Year==2014), ]

#get insight on basic numeric figures of data per year
summary(salary2011)
summary(salary2012)
summary(salary2013)
summary(salary2014)

#Base pay Mean plot per year

plotBPmean <- c(mean(salary2011$BasePay, na.rm = TRUE), mean(salary2012$BasePay, na.rm = TRUE), mean(salary2013$BasePay, na.rm = TRUE), mean(salary2014$BasePay, na.rm = TRUE))
barplot(plotBPmean, xlab = "Basepay Mean per year", ylab = "$" , main = "BarPlot of Basepay Mean", 
        ylim = c(0, 67000), names.arg = c("2011" , "2012" , "2013", "2014"))

#Overtime pay Mean plot per year

plotOTPmean <- c(mean(salary2011$OvertimePay, na.rm = TRUE), mean(salary2012$OvertimePay, na.rm = TRUE) , mean(salary2013$OvertimePay, na.rm = TRUE) , mean(salary2014$OvertimePay, na.rm = TRUE))
barplot(plotOTPmean, xlab = "Overtime Mean per year", ylab = "$" , main = "BarPlot of Overtime Mean",
        ylim = c(0, 8000), names.arg = c("2011" , "2012" , "2013", "2014"))

#Other pay Mean plot per year

plotOPmean <- c(mean(salary2011$OtherPay, na.rm = TRUE), mean(salary2012$OtherPay, na.rm = TRUE) , mean(salary2013$OtherPay, na.rm = TRUE) , mean(salary2014$OtherPay, na.rm = TRUE))
barplot(plotOPmean, xlab = "Other Pay Mean per year", ylab = "$" , main = "BarPlot of Other Pay Mean",
        ylim = c(0, 8000), names.arg = c("2011" , "2012" , "2013", "2014"))

#Benefits Mean plot per year

#plotBenmean <- c(mean(salary2011$Benefits, na.rm = TRUE), mean(salary2012$Benefits, na.rm = TRUE) , mean(salary2013$Benefits, na.rm = TRUE) , mean(salary2014$Benefits, na.rm = TRUE))
#barplot(plotBenmean, xlab = "Benefits Mean per year", ylab = "$" , main = "BarPlot of Benefits Mean",
       #ylim = c(0, 8000), names.arg = c("2011" , "2012" , "2013", "2014"))

#Total Pay Mean plot per year

plotTPmean <- c(mean(salary2011$TotalPay, na.rm = TRUE), mean(salary2012$TotalPay, na.rm = TRUE), mean(salary2013$TotalPay, na.rm = TRUE), mean(salary2014$TotalPay, na.rm = TRUE))
barplot(plotBPmean, xlab = "Total Pay Mean per year", ylab = "$" , main = "BarPlot of Total Pay Mean", 
        ylim = c(0, 67000), names.arg = c("2011" , "2012" , "2013", "2014"))

#Total Pay Benefits Mean plot per year

plotTPBmean <- c(mean(salary2011$TotalPayBenefits, na.rm = TRUE), mean(salary2012$TotalPayBenefits, na.rm = TRUE), mean(salary2013$TotalPayBenefits, na.rm = TRUE), mean(salary2014$TotalPayBenefits, na.rm = TRUE))
barplot(plotBPmean, xlab = "Total Pay Benefits Mean per year", ylab = "$" , main = "BarPlot of Total Pay Benefits Mean", 
        ylim = c(0, 67000), names.arg = c("2011" , "2012" , "2013", "2014"))

# what are the most frequent titles? -- FULL set
salary_data1 %>%
  group_by(JobTitle) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  head( salary_data1 , n = 10)

#Most frequent job titles per year
salary2011 %>%
  group_by(JobTitle) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  head( salary_data1 , n = 10)

salary2012 %>%
  group_by(JobTitle) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  head( salary_data1 , n = 10)

salary2013 %>%
  group_by(JobTitle) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  head( salary_data1 , n = 10)

salary2014 %>%
  group_by(JobTitle) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)) %>%
  head( salary_data1 , n = 10)

# How many jobs occur only once?
salary2011 %>%
  group_by(JobTitle) %>%
  summarise(Frequency = n()) %>%
  mutate(OccursOnce = Frequency == 1) %>%
  group_by(OccursOnce) %>%
  summarise(Total = n())

salary2012 %>%
  group_by(JobTitle) %>%
  summarise(Frequency = n()) %>%
  mutate(OccursOnce = Frequency == 1) %>%
  group_by(OccursOnce) %>%
  summarise(Total = n())

salary2013 %>%
  group_by(JobTitle) %>%
  summarise(Frequency = n()) %>%
  mutate(OccursOnce = Frequency == 1) %>%
  group_by(OccursOnce) %>%
  summarise(Total = n())

salary2014 %>%
  group_by(JobTitle) %>%
  summarise(Frequency = n()) %>%
  mutate(OccursOnce = Frequency == 1) %>%
  group_by(OccursOnce) %>%
  summarise(Total = n())

# what jobs occur only once?

salary_data1 %>%
  group_by(JobTitle) %>%
  summarise(Frequency = n()) %>%
  mutate(OccursOnce = Frequency == 1) %>%
  group_by(OccursOnce) %>%
  summarise(Total = n())

salary_data1 %>%
  filter(!duplicated(JobTitle)) %>%
  head( salary_data1, n = 10)

salary_data1 %>%
  mutate(SharesTitle = duplicated(JobTitle)) %>%
  ggplot(aes(x=TotalPay)) +
  geom_density(aes(fill = SharesTitle), alpha = 0.6)

salary_data1 %>%
  mutate(SharesTitle = duplicated(JobTitle)) %>%
  ggplot(aes(x=Year)) +
  geom_density(aes(fill = SharesTitle), alpha = 0.6)

#who has a pay of 0 over the course of 4 years

salary2011  %>%
  filter(TotalPay <= 0) #3 in 2011

salary2012  %>%
  filter(TotalPay <= 0) #25 in 2012

salary2013  %>%
  filter(TotalPay <=0) #288 in 2013

salary2014  %>%
  filter(TotalPay <= 0) #53 in 2014

#how many have 0 totalpay but receive benefits per year

salary2011  %>%
  filter(TotalPay <= 0 & TotalPayBenefits > 0) #0 in 2011

salary2012  %>%
  filter(TotalPay <= 0 & TotalPayBenefits > 0) #25 in 2012

salary2013  %>%
  filter(TotalPay <= 0 & TotalPayBenefits > 0) #285 in 2013

salary2014  %>%
  filter(TotalPay <= 0 & TotalPayBenefits > 0) #25 in 2014

#how many have no pay and no benefits per year

salary2011  %>%
  filter(TotalPay <= 0 & TotalPayBenefits == 0) #3 in 2011

salary2012  %>%
  filter(TotalPay <= 0 & TotalPayBenefits == 0) #0 in 2012

salary2013  %>%
  filter(TotalPay <= 0 & TotalPayBenefits == 0) #0 in 2013

salary2014  %>%
  filter(TotalPay <= 0 & TotalPayBenefits == 0) #27 in 2014


# who has a pay between 0 and 100.00
salary_data1 %>%
  filter(TotalPay > 0, TotalPay < 100)

#who has a pay between 100.00 and 5000
salary_data1 %>%
  filter(TotalPay > 100, TotalPay < 5000)

#who has a pay between 5000 and 20000
salary_data1 %>%
  filter(TotalPay > 5000, TotalPay < 20000)

#who has a pay between 20000 and 100000
salary_data1 %>%
  filter(TotalPay > 20000, TotalPay < 100000)

#who has a pay above 100000
salary_data1 %>%
  filter(TotalPay > 100000) 

#make the number of observations percentage and then form a piechart

piechart20111 <- count(salary2011[which(salary2011$TotalPay > 0 & salary2011$TotalPay < 100), ])/36159 * 100
piechart20112 <- count(salary2011[which(salary2011$TotalPay > 100 & salary2011$TotalPay < 5000), ])/36159 * 100
piechart20113 <- count(salary2011[which(salary2011$TotalPay > 5000 & salary2011$TotalPay < 20000), ])/36159 * 100
piechart20114 <- count(salary2011[which(salary2011$TotalPay > 20000 & salary2011$TotalPay < 100000), ])/36159 * 100
piechart20115 <- count(salary2011[which(salary2011$TotalPay > 100000), ])/36159 * 100
piechart2011 <- as.numeric(c(piechart20111 , piechart20112 , piechart20113 , piechart20114 , piechart20115))
pie(piechart2011, labels = c("0.23% has salary between 0 and 100 $ per year", "7.61% has salary between 100 and 500 $ per year", 
                             "10.92% has salary between 5000 and 20000 $ per year" , 
                             "54.87% has salary between 20000 and 10000 $ per year"
                             , "26.34% has salary 100000 or above $ per year"),  edges = 500, radius = 1 , 
    col = c("purple", "violetred1", "green3","cornsilk", "cyan"),
    main = "Pie Chart of Salary distribution for the Year 2011")

salary2011  %>%
  filter(TotalPay > 0, TotalPay < 100)
salary2011  %>%
  filter(TotalPay > 100, TotalPay < 5000)
salary2011  %>%
  filter(TotalPay > 5000, TotalPay < 20000)
salary2011  %>%
  filter(TotalPay > 20000, TotalPay < 100000)
salary2011  %>%
  filter(TotalPay > 100000) 

piechart20121 <- count(salary2012[which(salary2012$TotalPay > 0 & salary2012$TotalPay < 100), ])/36766 * 100
piechart20122 <- count(salary2012[which(salary2012$TotalPay > 100 & salary2012$TotalPay < 5000), ])/36766 * 100
piechart20123 <- count(salary2012[which(salary2012$TotalPay > 5000 & salary2012$TotalPay < 20000), ])/36766 * 100
piechart20124 <- count(salary2012[which(salary2012$TotalPay > 20000 & salary2012$TotalPay < 100000), ])/36766 * 100
piechart20125 <- count(salary2012[which(salary2012$TotalPay > 100000), ])/36766 * 100
piechart2012 <- as.numeric(c(piechart20121 , piechart20122 , piechart20123 , piechart20124 , piechart20125))
pie(piechart2012, labels = c("0.33% has salary between 0 and 100 $ per year", "7.12% has salary between 100 and 500 $ per year", 
                             "10.79% has salary between 5000 and 20000 $ per year" , 
                             "54.48% has salary between 20000 and 10000 $ per year"
                             , "27.20% has salary 100000 or above $ per year"),  edges = 500, radius = 1 , 
    col = c("purple", "violetred1", "green3","cornsilk", "cyan"),
    main = "Pie Chart of Salary distribution for the Year 2012")

salary2012  %>%
  filter(TotalPay > 0, TotalPay < 100)
salary2012  %>%
  filter(TotalPay > 100, TotalPay < 5000)
salary2012  %>%
  filter(TotalPay > 5000, TotalPay < 20000)
salary2012  %>%
  filter(TotalPay > 20000, TotalPay < 100000)
salary2012  %>%
  filter(TotalPay > 100000) 


piechart20131 <- count(salary2013[which(salary2013$TotalPay > 0 & salary2013$TotalPay < 100), ])/37606 * 100
piechart20132 <- count(salary2013[which(salary2013$TotalPay > 100 & salary2013$TotalPay < 5000), ])/37606 * 100
piechart20133 <- count(salary2013[which(salary2013$TotalPay > 5000 & salary2013$TotalPay < 20000), ])/37606 * 100
piechart20134 <- count(salary2013[which(salary2013$TotalPay > 20000 & salary2013$TotalPay < 100000), ])/37606 * 100
piechart20135 <- count(salary2013[which(salary2013$TotalPay > 100000), ])/37606 * 100
piechart2013 <- as.numeric(c(piechart20131 , piechart20132 , piechart20133 , piechart20134 , piechart20135))
pie(piechart2013, labels = c("0.22% has salary between 0 and 100 $ per year", "7.15% has salary between 100 and 500 $ per year", 
                             "10.61% has salary between 5000 and 20000 $ per year" , 
                             "51.02% has salary between 20000 and 10000 $ per year"
                             , "30.20% has salary 100000 or above $ per year"),  edges = 500, radius = 1 , 
    col = c("purple", "violetred1", "green3","cornsilk", "cyan"),
    main = "Pie Chart of Salary distribution for the Year 2013")

salary2013  %>%
  filter(TotalPay > 0, TotalPay < 100)
salary2013  %>%
  filter(TotalPay > 100, TotalPay < 5000)
salary2013  %>%
  filter(TotalPay > 5000, TotalPay < 20000)
salary2013  %>%
  filter(TotalPay > 20000, TotalPay < 100000)
salary2013  %>%
  filter(TotalPay > 100000) 

piechart20141 <- count(salary2013[which(salary2014$TotalPay > 0 & salary2014$TotalPay < 100), ])/38123 * 100
piechart20142 <- count(salary2013[which(salary2014$TotalPay > 100 & salary2014$TotalPay < 5000), ])/38123 * 100
piechart20143 <- count(salary2013[which(salary2014$TotalPay > 5000 & salary2014$TotalPay < 20000), ])/38123 * 100
piechart20144 <- count(salary2013[which(salary2014$TotalPay > 20000 & salary2014$TotalPay < 100000), ])/38123 * 100
piechart20145 <- count(salary2013[which(salary2014$TotalPay > 100000), ])/38123 * 100
piechart2014 <- as.numeric(c(piechart20141 , piechart20142 , piechart20143 , piechart20144 , piechart20145))
pie(piechart2014, labels = c("0.27% has salary between 0 and 100 $ per year", "7.61% has salary between 100 and 500 $ per year", 
                             "11.51% has salary between 5000 and 20000 $ per year" , 
                             "51.38% has salary between 20000 and 10000 $ per year"
                             , "29.07% has salary 100000 or above $ per year"),  edges = 500, radius = 1 , 
    col = c("purple", "violetred1", "green3","cornsilk", "cyan"),
    main = "Pie Chart of Salary distribution for the Year 2014")

salary2014  %>%
  filter(TotalPay > 0, TotalPay < 100)
salary2014  %>%
  filter(TotalPay > 100, TotalPay < 5000)
salary2014  %>%
  filter(TotalPay > 5000, TotalPay < 20000)
salary2014  %>%
  filter(TotalPay > 20000, TotalPay < 100000)
salary2014  %>%
  filter(TotalPay > 100000) 


# Status

salary_data1 %>%
  ggplot(aes(x = TotalPay)) + 
  geom_density(aes(fill=Status), alpha=0.6)

salary2011 %>%
  ggplot(aes(x = TotalPay)) + 
  geom_density(aes(fill=Status), alpha=0.6)

salary2012 %>%
  ggplot(aes(x = TotalPay)) + 
  geom_density(aes(fill=Status), alpha=0.6)

salary2013 %>%
  ggplot(aes(x = TotalPay)) + 
  geom_density(aes(fill=Status), alpha=0.6)

salary2014 %>%
  ggplot(aes(x = TotalPay)) + 
  geom_density(aes(fill=Status), alpha=0.6)

# status ocunts
salary_data1 %>%
  group_by(Status) %>%
  summarise(Frequency = n())

salary2011 %>%
  group_by(Status) %>%
  summarise(Frequency = n())

salary2012 %>%
  group_by(Status) %>%
  summarise(Frequency = n())

salary2013 %>%
  group_by(Status) %>%
  summarise(Frequency = n())

salary2014 %>%
  group_by(Status) %>%                    #status only for 2014. FT 22334 , PT 15785
  summarise(Frequency = n())   

#jobs with most overtime pay per year

salary2011 %>%
  group_by(OvertimePay) %>%
  arrange(desc(OvertimePay)) %>%  
  head( salary2011 , n = 10)

salary2012 %>%
  group_by(OvertimePay) %>%
  arrange(desc(OvertimePay)) %>%  
  head( salary2012 , n = 10)

salary2013 %>%
  group_by(OvertimePay) %>%
  arrange(desc(OvertimePay)) %>%  
  head( salary2013 , n = 10)

salary2014 %>%
  group_by(OvertimePay) %>%
  arrange(desc(OvertimePay)) %>%  
  head( salary2014 , n = 10)

# jobs with most other pay per year

salary2011 %>%
  group_by(OtherPay) %>%
  arrange(desc(OtherPay)) %>%  
  head( salary2011 , n = 10)

salary2012 %>%
  group_by(OtherPay) %>%
  arrange(desc(OtherPay)) %>%  
  head( salary2012 , n = 10)

salary2013 %>%
  group_by(OtherPay) %>%
  arrange(desc(OtherPay)) %>%  
  head( salary2013 , n = 10)

salary2014 %>%
  group_by(OtherPay) %>%
  arrange(desc(OtherPay)) %>%  
  head( salary2014 , n = 10)


# jobs with most benefit pay per year

salary2011 %>%
  group_by(Benefits) %>%
  arrange(desc(Benefits)) %>%  
  head( salary2011 , n = 10)

salary2012 %>%
  group_by(Benefits) %>%
  arrange(desc(Benefits)) %>%  
  head( salary2012 , n = 10)

salary2013 %>%
  group_by(Benefits) %>%
  arrange(desc(Benefits)) %>%  
  head( salary2013 , n = 10)

salary2014 %>%
  group_by(Benefits) %>%
  arrange(desc(Benefits)) %>%  
  head( salary2014 , n = 10)


#check unique job titles
(length(unique(salary2011$JobTitle))) #1045 unique job titles in 2011
head(unique(salary2011$JobTitle))

(length(unique(salary2012$JobTitle))) #1044 unique job titles in 2012
head(unique(salary2012$JobTitle))

(length(unique(salary2013$JobTitle))) #1051 unique job titles in 2013
head(unique(salary2013$JobTitle))

(length(unique(salary2014$JobTitle))) #997 unique job titles in 2014
head(unique(salary2014$JobTitle))

# 1 tropos
year=c(2011,2012,2013,2014)
sapply(1:length(year), function(i) length(unique(salary_data1[salary_data1$Year==year[i],]$JobTitle)))

# 2 tropos
sapply(year, function(i) length(unique(salary_data1[salary_data1$Year==i,]$JobTitle)))

#max apo oles tis stiles
apply(salary_data1, 2, max, na.rm=TRUE)

#split EmployeeName in two columns FirstName and Last Name

salary_data2 <- extract(salary_data1, EmployeeName , c("FirstName", "LastName"), "([^ ]+) (.*)")
