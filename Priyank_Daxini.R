library(dplyr)
library(tidyr)
library(ggplot2)
library(e1071)

setwd("C:/Users/HP/Downloads")
getwd()

host <- read.csv("host.csv")
price <- read.csv("pricing.csv")
property <- read.csv("property.csv")
review <- read.csv("reviews.csv")

#EDA and pre-processing
head(host)
host$host_since <- as.Date(host$host_since)
host$id <- as.factor(host$id)
str(host)

head(price)

str(price)
price$id <- as.factor(price$id)

head(review)
str(review)
review$first_review <- as.Date(review$first_review)
review$last_review <- as.Date(review$last_review)
review$id <- as.factor(review$id)

head(property)
str(property)
levels(property$city)

#merging
host2 <- host %>% 
  left_join(price, by = "id") #merged price and host

head(host2)

head(property)

property2 <- property %>% 
  mutate(id = property_id) %>% 
  select(-property_id) #changed column name to match with host2

str(property2)
property2$id <- as.factor(property2$id)

host3 <- host2 %>% 
  left_join(property2, by = "id") %>% 
  select(-latitude.longitude) #merged host2 and property2
head(host3)

#cleaning
sum(is.na(host3$host_response_rate))
host3$host_response_rate <- replace_na(host3$host_response_rate, replace = median(host3$host_response_rate, na.rm = TRUE))
tail(host3)
levels(host3$room_type)
head(host3)

#Analysis

#1
cor(host3$log_price, host3$accommodates) #0.5675

#2
host3 %>% 
  filter(room_type == "Private room") %>% 
  summarise(mean_prices = mean(exp(log_price)))
  #Median price for private room is $88.48799

#3
host3 %>% 
  filter(cancellation_policy %in% c("flexible", "moderate") & city == "DC") %>% 
  count()
  #3640 properties in DC have a flexible or moderate cancellation policy

#4
host4 <-host3 %>% 
  mutate(prices = exp(log_price))  
head(host4) #Created original price column

#5
host4 %>% 
  filter(room_type=="Entire home/apt" & city %in% c("SF", "LA")) %>% 
  arrange(prices) %>% 
  select(city, prices, room_type, accommodates) %>% 
  head() #Entire homes available on the West coast cities from lowest to highest price


#Visualisation
# 1. Number of properties with various cancellation policy trends
host3 %>% 
  group_by(cancellation_policy) %>%  
  summarise(no_of_hosts = n_distinct(id)) %>% 
  ggplot(aes(x=cancellation_policy, y=no_of_hosts))+
  geom_point()+
  ggtitle("Cancellation Policy Trends")+
  geom_text(aes(label=no_of_hosts), hjust=0.2, vjust=-0.5)+
  xlab("Cancellation policy")+
  ylab("Number of hosts")+
  theme(plot.title = element_text(hjust = .5))

# 2. Median prices of various city properties
host4 %>% 
  group_by(city) %>% 
  summarise(median_price = median(prices)) %>% 
  ggplot(aes(x=city, y=median_price, labels = median_price)) + 
  geom_point()+
  geom_text(aes(label=median_price), hjust=0.2, vjust=-0.5)+
  ggtitle("Citywise Pricing Trends")+
  xlab("City")+
  ylab("Median Price")+
  theme(plot.title = element_text(hjust = .5))

# 3. Division according to room type
host3 %>% 
  ggplot(aes(accommodates, fill = room_type))+
  geom_histogram()+
  geom_histogram(bins = sqrt(nrow(host3)))+
  xlab("Accomodates")+
  ylab("Number")+
  ggtitle("Number of guests accommodated by different room types")+
  theme(plot.title = element_text(hjust = .5))
  
#4 Room types in NYC
host4 %>% 
  filter(city == "NYC") %>% 
  ggplot(aes(x=room_type))+
  geom_bar(aes(fill = factor(cleaning_fee)), stat = "count", position = "dodge")+
  geom_text(stat = "count", aes(label=..count..), vjust =0)+
  xlab("Room Type")+ 
  ylab("Count")+
  ggtitle("Room types in NYC")+
  theme(plot.title = element_text(hjust = .5))

#5
host4 %>% 
  mutate(Per_head_cost = prices/accommodates) %>% 
  ggplot(aes(x=accommodates, y=Per_head_cost))+
  geom_smooth()+
  ggtitle("Per head cost vs No. of guests")+
  xlab("No. of Guests")+
  ylab("Per Head Cost")+
  theme(plot.title = element_text(hjust=.5))
  
  


