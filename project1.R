library('ggplot2')
setwd('~/desktop/sta141a')

#Read the data
college = readRDS('college_scorecard.rds')

#get the number of rows and columns.
dim(college)
View(college)

#range of year and the number of colleges for each year
which(is.na(college$academic_year))#to check whether there is NA
typeof(college$academic_year)
year = as.double(college$academic_year)
range = range(year);range
table(college$academic_year)


#the 5 states with the most colleges and with the least colleges
which(is.na(college$state))
table(college$state)
sort(table(college$state),decreasing = TRUE)

#for each year,the 5 states with the most colleges and with the least colleges
college2012 <- subset(college, academic_year == '2012')
college2013 <- subset(college, academic_year == '2013')
college2014 <- subset(college, academic_year == '2014')
college2015 <- subset(college, academic_year == '2015')
college2016 <- subset(college, academic_year == '2016')
sort(table(college2012$state),decreasing = TRUE)
sort(table(college2013$state),decreasing = TRUE)
sort(table(college2014$state),decreasing = TRUE)
sort(table(college2015$state),decreasing = TRUE)
sort(table(college2016$state),decreasing = TRUE)
ggplot(college,aes(x = academic_year,color = state)) + 
  geom_point(stat = 'count')




#a scatter plot of public schools in the 2014 academic year
ggplot(college2014,aes(x = avg_net_price.public,
                       y = earn_10_yrs_after_entry.median))+
  geom_point() + ylim(c(0,150000)) + xlim(c(-1000,75000)) +
  labs(title = 'Situation for public schools in 2014', 
       x = 'average net price',
       y = 'median student earnings after 10 years')



# scatter plot of private for_profit schools in the 2014 academic year
for_profit_college2014 <-subset(college2014, ownership == 'Private for-profit')
ggplot(for_profit_college2014,aes(x = avg_net_price.private,
                       y = earn_10_yrs_after_entry.median))+
  geom_point()  + ylim(c(0,150000)) + xlim(c(-1000,75000)) +
  labs(title = 'Situation for private for_profit schools in 2014', 
       x = 'average net price',
       y = 'median student earnings after 10 years')

specialcase <- subset(for_profit_college2014, avg_net_price.private > 50000)
View(specialcase)


# scatter plot of private non_profit schools in the 2014 academic year
non_profit_college2014 <-subset(college2014, ownership == 'Private nonprofit')
ggplot(non_profit_college2014,aes(x = avg_net_price.private,
                                  y = earn_10_yrs_after_entry.median))+
  geom_point()  + ylim(c(0,150000)) + xlim(c(-1000,75000)) +
  labs(title = 'Situation for private nonprofit schools in 2014', 
       x = 'average net price',
       y = 'median student earnings after 10 years')


# a bar plot that shows the number of schools recorded for each year
ggplot(college,aes(x = ownership,fill = academic_year)) + 
  geom_bar(position = 'dodge')




#a line plot for the admission rates for UC Davis for each year
UCDavis <-subset(college,id == '110644')
ggplot(UCDavis,aes(x = academic_year,
                   y = admission_rate.overall,group = 1))+
  geom_point()+geom_line() +
  labs(title = 'the admission rates for UC Davis for each year', 
       x = 'academic year',
       y = 'admission rate')











