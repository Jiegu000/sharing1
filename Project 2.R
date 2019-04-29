colleges = readRDS('college_scorecard.rds')
library(ggplot2)

#classify colleges by years
colleges2012 <- subset(colleges, academic_year == '2012')
colleges2013 <- subset(colleges, academic_year == '2013')
colleges2014 <- subset(colleges, academic_year == '2014')
colleges2015 <- subset(colleges, academic_year == '2015')
colleges2016 <- subset(colleges, academic_year == '2016')

#classify colleges by ownership
public_colleges <- subset(colleges,ownership == 'Public')
private_forprofit_colleges <- subset(colleges,ownership == 'Private for-profit')
private_nonprofit_colleges <- subset(colleges,ownership == 'Private nonprofit')



#Find features with no missing values and most missing values
#----------------------
colleges_isna <- sapply(colleges,is.na)
colleges_isna_count <- apply(colleges_isna,2,sum)
colleges_nomissing = colleges[,colleges_isna_count == 0]
most_misssing = max(colleges_isna_count)
colleges_mostmissing = colleges[,colleges_isna_count == most_misssing]

names(colleges_nomissing)
names(colleges_mostmissing)

#by years
sum(is.na(colleges2012))
sum(is.na(colleges2013))
sum(is.na(colleges2014))
sum(is.na(colleges2015))
sum(is.na(colleges2016))

#by ownership
sum(is.na(public_colleges))
sum(is.na(private_forprofit_colleges))
sum(is.na(private_nonprofit_colleges))


#Explore student population------------------

#population for three types of school
aggregate(size ~ ownership, colleges,summary,na.rm = TRUE)
aggregate(grad_students ~ ownership, colleges,summary,na.rm = TRUE)
aggregate(size ~ ownership, colleges,sd,na.rm = TRUE)
aggregate(grad_students ~ ownership,colleges,sd,na.rm = TRUE)


#poulation for school of different years
aggregate(size ~ academic_year, colleges,summary,na.rm = TRUE)
aggregate(grad_students ~ academic_year, colleges,summary,na.rm = TRUE)
aggregate(size ~ academic_year, colleges,sd,na.rm = TRUE)
aggregate(grad_students ~ academic_year, colleges,sd,na.rm = TRUE)

#unusuall population colleges

small_undergraduate = subset(colleges,size == 0)
small_graduate = subset(colleges,grad_students == 0)
unusual_small = subset(colleges,grad_students == 0 & size == 0)

large_exception = subset(colleges,size > 100000 | grad_students > 40000)



#relation between undergraduate and graduate population
ggplot(colleges,aes(x = size,y = grad_students)) + 
  geom_point() +
  labs(title = 'relation between undergraduate students 
       and graduate students', x = 'undergraduate students',
       y = 'graduate students')

ggplot(colleges,aes(x = size,y = grad_students)) + 
  geom_point() + geom_density2d() + xlim(-200,2500) + ylim(-200,1000) +
  labs(title = 'relation between undergraduate students 
       and graduate students', x = 'undergraduate students',
       y = 'graduate students')
one_exception <- subset(colleges, grad_students > 40000 & size < 25000)



#Explore the program percentage-----------------


names(colleges)
program = colleges[47:84] 
program2013 = colleges2013[47:84]
program2014 = colleges2014[47:84]
program2015 = colleges2015[47:84]
program2016 = colleges2016[47:84]





program2012_mean = sapply(program2012,mean,na.rm = TRUE)
sort(program2012_mean,decreasing = TRUE)
program2013_mean = sapply(program2013,mean,na.rm = TRUE)
sort(program2013_mean,decreasing = TRUE)
program2014_mean = sapply(program2014,mean,na.rm = TRUE)
sort(program2014_mean,decreasing = TRUE)
program2015_mean = sapply(program2015,mean,na.rm = TRUE)
sort(program2015_mean,decreasing = TRUE)
program2016_mean = sapply(program2016,mean,na.rm = TRUE)
sort(program2016_mean,decreasing = TRUE)

sapply(program,summary,na.rm = TRUE)

#mean equal to zero, but median is large
ggplot(colleges, aes(x = program_percentage.personal_culinary)) + geom_histogram()
#mean not equal to 0
ggplot(colleges, aes(x = program_percentage.health)) + geom_histogram()
ggplot(colleges, aes(x = program_percentage.business_marketing)) + geom_histogram()



#Analysis tuition---------------
#How does tuition vary across different states?
tuition_list_instate<-lapply(year_split,function(x)aggregate(tuition.in_state~state,colleges,mean))
tuition_list_instate


sd(tuition_list_instate$`2012`[[2]])
sd(tuition_list_instate$`2013`[[2]])
sd(tuition_list_instate$`2014`[[2]])
sd(tuition_list_instate$`2015`[[2]])
sd(tuition_list_instate$`2016`[[2]])

tuition_list_outstate<-lapply(year_split,function(x)aggregate(tuition.out_of_state~state,colleges,mean))
tuition_list_outstate


sd(tuition_list_outstate$`2012`[[2]])
sd(tuition_list_outstate$`2013`[[2]])
sd(tuition_list_outstate$`2014`[[2]])
sd(tuition_list_outstate$`2015`[[2]])
sd(tuition_list_outstate$`2016`[[2]])

tution_mean_instate <- aggregate(tuition.in_state~state,colleges,mean)
ggplot(tution_mean_instate,aes(x = state,y = tuition.in_state)) + geom_point()

tution_mean_outstate <- aggregate(tuition.out_of_state~state,colleges,mean)
ggplot(tution_mean_outstate,aes(x = state,y = tuition.out_of_state)) + geom_point()

#relationship between the number of universities in a state and tuition
school2016_number = as.double(table(colleges2016$state))
ggplot(tution_mean_instate,aes(x = school2016_number,y = tuition.in_state,color = state)) +
  geom_point() +labs(title = ' the amount of colleges v.s. tuition in state',
                    x = 'the amount of colleges',y = 'tuition in state') +
  guides(color = guide_legend('State'))

ggplot(tution_mean_outstate,aes(x = school2016_number,y = tuition.out_of_state,color = state)) +
  geom_point() +labs(title = ' the amount of colleges v.s. tuition out of state',
                     x = 'the amount of colleges',y = 'tuition out of state') +
  guides(color = guide_legend('State'))



#Demographics-------------------


all(colleges$demographics.age_entry != 0,na.rm = TRUE)
colleges_firststep <-subset(colleges,demographics.race_ethnicity.white != 0 &
                               demographics.race_ethnicity.black != 0 &
                                 demographics.race_ethnicity.hispanic != 0 &
                                 demographics.race_ethnicity.asian != 0 &
                                 demographics.race_ethnicity.aian != 0 &
                                 demographics.race_ethnicity.nhpi != 0 &
                                 demographics.race_ethnicity.two_or_more != 0 &
                                 demographics.race_ethnicity.non_resident_alien != 0 &
                                 demographics.race_ethnicity.unknown != 0 &
                                 demographics.veteran != 0 &
                                 demographics.first_generation != 0 &
                                 demographics.men >= 0.49 &
                                 demographics.women >= 0.49)

names(colleges)
colleges_secondstep = colleges_firststep[87:95]
SD = apply(colleges_secondstep,1,sd,na.rm = TRUE)
colleges_thirdstep = cbind(colleges_firststep[4],SD,colleges_fisrtstep[138:142])
colleges_most_diverse <- subset(colleges_thirdstep,SD < 0.15)

View(colleges_most_diverse)




#The average median family income for different types of schools-------
mean(public_colleges$demographics.median_family_income,na.rm = TRUE)
mean(private_forprofit_colleges$demographics.median_family_income,na.rm = TRUE)
mean(private_nonprofit_colleges$demographics.median_family_income,na.rm = TRUE)

ggplot(colleges,aes(x = ownership,y = demographics.median_family_income)) + geom_boxplot()
                           



#For colleges in 2014, what is the relation between
#admission rate and the median student earnings after 10 years
ggplot(colleges2014,aes(x = admission_rate.overall, 
                        y = earn_10_yrs_after_entry.median)) +
  geom_point() + labs(title = 'admission rate v.s. median
                      student earnings after 10 years',
  x = 'admission rate',y = 'median student earnings after 10 years')

unusual_value <-subset(colleges2014,admission_rate.overall > 0.50 &
                         earn_10_yrs_after_entry.median >100000)                               
                       


#how do tuition vary between three types of school
ggplot(colleges,aes(x = ownership,y = tuition.out_of_state)) + geom_boxplot()
ggplot(colleges,aes(x = ownership,y = tuition.in_state)) + geom_boxplot()

high_tuition <- subset(private_forprofit_colleges,tuition.out_of_state > 60000 | 
                         tuition.in_state > 60000)

#why some colleges have both 0 graduate students and 0 undergraduate students
View(unusual_small)
subset(colleges,name == 'Lyme Academy College of Fine Arts')
subset(colleges,name == 'School of the Museum of Fine Arts at Tufts University')
#schools about fine art, only have program_percentage.visual_performing.














