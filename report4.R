library(ggplot2)
library(ggmap)
library(Lahman)
library(stringr)

#Before solving questions, read data and adjust data
#==============================================
cl = readRDS('cl_apartments.rds')


#adjust the data like I did in assignment3
cl_ca <- subset(cl,state == 'CA')
cl_no_duplicate = cl_ca[!duplicated(cl$title),]
cl_no_duplicate = cl_no_duplicate[!duplicated(cl_no_duplicate$text),]
cl_no_duplicate$price[cl_no_duplicate$price>=30000000] <- 3408
cl_no_duplicate$price[cl_no_duplicate$price==9951095] <- 995
cl_small_price <- subset(cl_no_duplicate,price <= 100)
cl_no_duplicate$price[cl_no_duplicate$price <= 100] <- NA
cl_no_duplicate$sqft[cl_no_duplicate$sqft >= 10000] <- NA
head(sort(cl_no_duplicate$sqft))
cl_no_duplicate$sqft[cl_no_duplicate$sqft <= 3] <- NA
cl = cl_no_duplicate

#Read Census data
ann = read.csv('DEC_10_SF1_SF1DP1_with_ann.csv',header = TRUE)

#see the feature of data
names(ann)
head(ann$GEO.display.label)

#remove extra information in GEO
ann$GEO.display.label = str_remove_all(ann[,3],' (city|CDP),| California')

#Check difference in two variables
setdiff(cl$city,ann$GEO.display.label)

#merge the data
cl_ann = merge(cl,ann,by.x = 'city',by.y = 'GEO.display.label')



#Q1:Use maps to explore at least 3 features of the Craigslist posts in Davis.
#=============================================================

#get the data for davis
Davis <- subset(cl_ann,city == 'Davis')


#get the map for daivs
bbox = c(
  -121.792517,38.514802,
  -121.689546,38.573602
)#From piazza
m = get_stamenmap(bbox, zoom = 13, maptype = "toner-lite") 


#Q2:question1: How does the price of aparments vary in Davis
ggmap(m) + geom_point(aes(x = longitude,y = latitude,color = price),Davis,size = 5) +
  labs(title = 'The distribution of price in Davis')

#Q2:question2: if the pets??? policy is related to location, 
#where should we go to rent an apartment assuming that we have pets. 

table(Davis$pets)
#add new variables to Davis to record whether a pet is allowed.
pets_policy = (Davis$pets == 'both' | Davis$pets == 'cats' |
                  Davis$pets == 'dogs' | Davis$pets == 'negotiable')
Davis$pets_policy= pets_policy

#remove na in pets
Davis_no_na_pet <- subset(Davis,is.na(pets_policy) != TRUE)

#a dotplot of pets_policy in Davis
ggmap(m) + geom_point(aes(x = longitude,y = latitude,color = pets_policy),Davis_no_na_pet,size = 5) + 
  labs(color = 'Whether pets are allowed',title = 'Pets policy in Davis')


#Q2:question3: which part of Davis should we go 
#if we want to rent an apartment with more bedrooms and relatively large size
ggmap(m) + geom_point(aes(x = longitude,y = latitude,size = as.character(bedrooms),color = sqft),Davis) +
  labs(size = 'number of bedrooms',title = 'Situation about size and bedrooms of apartments')





#Q2:Repeat question 1 for the southern half of the San Francisco Bay Area 
#(counties San Francisco, San Mateo, Santa Clara, Alameda, and Contra Costa).
#=============================================================

#get the data for San Francisco Bay Area
county_name = c('San Francisco','San Mateo','Santa Clara','Alameda','Contra Costa')
SF_BayArea = cl_ann[cl_ann$county%in%county_name,]

#get the map for San Francisco Bay Area
bbox2 = c(-122.80,36.8,
          -121.3,38.5)
m2 = get_stamenmap(bbox2, zoom = 10, maptype = "toner-lite") 


#Q2:question1:how the price of renting an apartment varies 
#in southern half of the San Francisco Bay Area.

#cut the price into four interval
SF_BayArea$price_interval = cut_number(SF_BayArea$price, 4)
#get the dotplot of price in map
SF_BayArea_noNA_price <- subset(SF_BayArea,price_interval != is.na(price_interval))
ggmap(m2) + geom_point(aes(x = longitude,y = latitude,color = price_interval),SF_BayArea_noNA_price) +
  labs(title = 'The distribution of price in sourthern half
       of the San Francisco Bay Area',color = 'price interval')


#Q2:question2:parking situation in southern half of the San Francisco Bay Area. 

#Define different parking type
SF_BayArea$Parking_situation[SF_BayArea$parking%in%c('street','off-street')] = 'street'
SF_BayArea$Parking_situation[SF_BayArea$parking%in%c('garage')] = 'garage'
SF_BayArea$Parking_situation[SF_BayArea$parking%in%c('valet')] = 'valet'
#remove NA in parking_situation
SF_BayArea_parking_noNA <- subset(SF_BayArea,Parking_situation != is.na(Parking_situation))
#get a dot plot of parking in the map
ggmap(m2) + geom_point(aes(x = longitude,y = latitude,color = Parking_situation),SF_BayArea_parking_noNA) +
  labs(title = 'Parking Situation in southern half
       of the San Francisco Bay Area', 
       color = 'Pakring situation')


#Q2:question3:check pets??? policy to solve that if the pets??? policy is related to location, 
#where should we go to rent an apartment assuming that we have pets
#add new varibles to record hwther pets are allowed
SF_BayArea$pets_policy[SF_BayArea$pets%in%c('both','cats','dogs','negotiable')] = 'allowed'
SF_BayArea$pets_policy[SF_BayArea$pets%in%c('none')] = 'not allowed'

#get a dot plot of pet policy in map
ggmap(m2) + geom_point(aes(x = longitude,y = latitude,color = pets_policy),SF_BayArea) +
scale_color_manual(values = c('allowed' = 'black','not allowed' = 'green')) +
  labs(title = 'Pets policy in southern half
       of the San Francisco Bay Area',
       color = 'Whether pets are allowed')



#Q3:Which places in the southern San Francisco Bay Area have the oldest populations? 
#How does this relate to the rental market, if at all?

#I have merged the data at the begining of the code.
#tansfer discrete data to continuous data
SF_BayArea$HD02_S025 = as.character(SF_BayArea$HD02_S025)
SF_BayArea$HD02_S025 = as.double(SF_BayArea$HD02_S025)

#To check which place have the oldest population
max(SF_BayArea$HD02_S025)
SF_BayArea[SF_BayArea$HD02_S025 == max(SF_BayArea$HD02_S025),]$place


#To get the mean price in each palce
mean_price = aggregate(price~place,SF_BayArea,mean)
names(mean_price) = c('place','mean_price')

#to get a dot plot of the percent over 65 years vs mean price
SF_BayArea_meanprice = merge(SF_BayArea,mean_price,by = 'place')
ggplot(SF_BayArea_meanprice,aes(x = HD02_S025,y = mean_price)) + geom_point() +
  geom_density2d() +
  labs(x = 'percent of 65 years or older(%)',
  y = 'mean price',
  title = 'The relation between percent of 
65 years or older and mean price')


  




