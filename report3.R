library(ggplot2)
library(ggridges)
library(gridExtra)
library(ggmap)
cl = readRDS('cl_apartments.rds')

#Q1:Big picture of data--------------------------


names(cl)
dim(cl)

#check the data it spans
sort(cl$date_posted)
sort(cl$date_posted,decreasing = TRUE)
sort(cl$date_updated,decreasing = TRUE)

#check the place it spans
min(cl$latitude,na.rm = TRUE)
max(cl$latitude,na.rm = TRUE)

min(cl$longitude,na.rm = TRUE)
max(cl$longitude,na.rm = TRUE)


#remove non-CA date
cl_ca <- subset(cl,state == 'CA')
#check the place it spans
cl_ca[cl_ca$latitude == min(cl_ca$latitude,na.rm = TRUE),]
cl_ca[cl_ca$latitude == max(cl_ca$latitude,na.rm = TRUE),]

cl_ca[cl_ca$longitude == min(cl_ca$longitude,na.rm = TRUE),]
cl_ca[cl_ca$longitude == max(cl_ca$longitude,na.rm = TRUE),]


#remove the duplicated data.
cl_no_duplicate = cl_ca[!duplicated(cl$title),]
cl_no_duplicate = cl_no_duplicate[!duplicated(cl_no_duplicate$text),]
#see cities with the most posts and the least post
sort(table(cl_no_duplicate$city),decreasing = TRUE)





#Q2(1)Are apartments in suburbs more likely to be family-friendly
# (many bedrooms, pets allowed, etc) than apartments in major cities?
#----------------------------------

#suburbs and major city

major_city <- subset(cl_no_duplicate,city == 'Los Angeles' | city == 'San Diego' |
                    city == 'San Jose' | city == 'Sacramento ' | city == 'Oakland'|
                    city == 'San Francisco' | city == 'Long Beach' | city == 'South San Francisco' |
                    city == 'West Sacramento'| city == 'Fresno')

suburbs <- subset(cl_no_duplicate,city != 'Los Angeles' & city != 'San Diego' &
                    city != 'San Jose' & city != 'Sacramento ' & city != 'Oakland'&
                    city != 'San Francisco' & city != 'Long Beach' & city != 'South San Francisco'&
                    city != 'West Sacramento' & city != 'Fresno')



#bedroom
summary(major_city$bedrooms)
summary(suburbs$bedrooms)

props3 = prop.table(table(major_city$bedrooms))
props3 = as.data.frame(props3)
g7 = ggplot(props3,aes(x = Var1,y = Freq)) + geom_bar(stat = 'identity') + 
  labs(x='bedrooms',y = 'proportion',title = 'major city') + 
  ylim(c(0,0.6))

props4 = prop.table(table(suburbs$bedrooms))
props4 = as.data.frame(props4)
g8 = ggplot(props4,aes(x = Var1,y = Freq)) + geom_bar(stat = 'identity') + 
  labs(x='bedroom',y ='proportion',title = 'suburbs') + 
  ylim(c(0,0.6))

grid.arrange(g7,g8)


#pets
props1 = prop.table(table(major_city$pets))
props1 = as.data.frame(props1)
g9 = ggplot(props1,aes(x = Var1,y = Freq)) + geom_bar(stat = 'identity') + 
  labs(x='Pets',y = 'proportion',title = 'major city') + 
  ylim(c(0,0.7))

props2 = prop.table(table(suburbs$pets))
props2 = as.data.frame(props2)
g10 = ggplot(props2,aes(x = Var1,y = Freq)) + geom_bar(stat = 'identity') + 
  labs(x='Pets',y = 'proportion',title = 'suburbs') + 
  ylim(c(0,0.7))

grid.arrange(g9,g10)



#Q2(2)Which adds more to rent---------------------
summary(cl_no_duplicate$price)

hist(cl_no_duplicate$price)
tail(sort(cl_no_duplicate$price),10)
#(From discussion)no one will rent an apartment for $9M/month
#we can potentially ignore them in the context of our problems.

#sometimes the values are not acctualy that large.
#There was just an issue in parsing the data
#If we read the posting for the highest priced apartment
#Then we see the price range is $3408-3742
#the price in this data is 34083742
cl_no_duplicate$price[cl_no_duplicate$price>=30000000] <- 3408
cl_no_duplicate$price[cl_no_duplicate$price==9951095] <- 995

#smaller than 100 price delete
cl_small_price <- subset(cl_no_duplicate,price <= 100)
cl_no_duplicate$price[cl_no_duplicate$price <= 100] <- NA

mean_bedrooms = aggregate(price~bedrooms,cl_no_duplicate,mean)
mean_bathrooms = aggregate(price~bathrooms,cl_no_duplicate,mean)
g5 = ggplot(mean_bedrooms,aes(x =bedrooms,y = price,group = 1)) + geom_point() + geom_line() + 
  ylim(c(0,15000)) + labs(x = 'number of bedrooms',y = 'mean price')
g6 = ggplot(mean_bathrooms,aes(x =bathrooms,y = price,group = 1)) + geom_point() + geom_line() + 
  ylim(c(0,15000)) + labs(x = 'number of bathrooms',y = 'mean price')

grid.arrange(g5,g6)




#Q2(3)Do apartments in similar geographical areas tend to be similar?
#-------------------------------------
cl_sample <- subset(cl_no_duplicate,city == 'San Francisco' | city == 'Oakland' |
                      city == 'San Jose')
#look at bedroom
ggplot(cl_sample,aes(x = bedrooms,..prop..))+ 
  geom_bar() + facet_wrap(~city) + 
  labs(x = 'number of bedrooms',y = 'proportion',
       title = 'The proportion of apartments with different number of  bedrooms')

#look at price and sqft
#sqft
hist(cl_no_duplicate$sqft)
plot(sort(cl_no_duplicate$sqft))
tail(sort(cl_no_duplicate$sqft))
#perhaps 200,000 sqft was an error in the data
#so we remove it 
cl_no_duplicate$sqft[cl_no_duplicate$sqft >= 10000] <- NA


head(sort(cl_no_duplicate$sqft))
#perhaps 1 and 3 sqft are erros in data, so we choose to remove it
cl_no_duplicate$sqft[cl_no_duplicate$sqft <= 3] <- NA


#dot plot about price and sqft
ggplot(cl_sample,aes(x = sqft,y =price)) + geom_point() +
  facet_wrap(~ city) + labs(title = ' Relation between sqft and price in three cities')



#Q3&4: Own Questions---------------------------
#1.Will the parking influence the price of apartments? How does it influence that?
mean_price_parking = aggregate(price~parking,cl_no_duplicate,mean)
ggplot(mean_price_parking,aes(x = parking,y = price)) + geom_bar(stat = 'identity') +
  labs(title = 'Mean price for different conditions of parking')
cl_none <-subset(cl_no_duplicate,parking == 'none')


#2.Will the laundary influence the price of apartment?

#3.Will the apartments with 0,1, 2, or 3 bedrooms be different on the
#laundary? and how?
cl_bedrooms0123 <- subset(cl_no_duplicate,bedrooms == 0 |bedrooms == 1 | bedrooms == 2 |
                           bedrooms == 3 )
ggplot(cl_bedrooms0123,aes(x = laundry))+ 
  geom_bar() + facet_wrap(~bedrooms) +labs(title = 'The situation of laundry for different number of bedrooms')
cl_hookup <- subset(cl_no_duplicate,laundry == 'hookup')

#4.Which area(by latitude and longitude ) in Los Angeles has more
# relatively low price apartments?


#5.How the price of apartment varies in north california and south california?
cl_N <- subset(cl_no_duplicate,latitude >= 36)
cl_S <- subset(cl_no_duplicate,latitude < 36)

summary(cl_N$price)
summary(cl_S$price)

g1 = ggplot(cl_N,aes(price)) + geom_histogram(aes(y = stat(count / sum(count)))) + 
  xlim(c(0,7500)) + ylim(c(0,0.25)) + 
  labs(title = 'Situation for North Calfornia',
                                            y = 'proption')
g2 = ggplot(cl_S,aes(price)) + geom_histogram(aes(y = stat(count / sum(count)))) + 
  xlim(c(0,7500)) + ylim(c(0,0.25)) + 
  labs(title = 'Situation for South Calfornia',
                                            y = 'proption')

grid.arrange(g1,g2)


#6. Are apartments in San Francisco and Los Angeles different in rent market(from price and sqft)?
cl_sf <- subset(cl_no_duplicate, city == 'San Francisco')
g3 = ggplot(cl_sf,aes(x = sqft,y = price)) + geom_point() + 
  ylim(c(0,15000)) + xlim(c(0,4000)) + labs(title = 'San Francisco')
g4 = ggplot(cl_los,aes(x = sqft,y = price)) + geom_point() +
  ylim(c(0,15000)) + xlim(c(0,4000)) + labs(title = 'Los Angeles')

grid.arrange(g3,g4)

#7.If a person want to rent a studio apartment(0 bedrooms) or apartment
#with one bedroom, which area(by longtitude and latitude) should he go?
cl_bedrooms01 <- subset(cl_no_duplicate,bedrooms == 0 | bedrooms == 1)
ggplot(cl_bedrooms01,aes(x = latitude,y = longitude)) + geom_point() + 
  geom_density2d() + 
  labs(title = 'spread of apartments with 0 or 1 bedroom')
cl_placetogo <- subset(cl_no_duplicate, latitude > 37 & latitude < 38)
sort(table(cl_placetogo$county))

#8.Will the apartments with more bedrooms allow pets more easily?



#9.Do apartments have a stricter policy on dogs or cats?


#10.For counties with more famous colleges(rank top 100),will the price be
#higher?


#Q5:limitations of the data set

cl_isna <- sapply(cl,is.na)
cl_isna_count <- apply(cl_isna,2,sum)
sum(cl_isna_count)
sort(cl_isna_count)
#I have mentioend and solved other limitations in the front questions.


