install.packages('skimr')

library(skimr)
library(tidyverse)
f <- file.choose() #brings up GUI finder to select files to be read
hotels <- read.csv(f)
skim(hotels)
hotels %>% filter(country != "USA") %>% filter(lead_time < 1)
resort_baby_hotels  <- hotels %>% filter(babies > 1 | children > 1 & (hotel == "Resort Hotel"))
skim(resort_baby_hotels)
unique(hotels$hotel)
city_baby_hotels  <- hotels %>% filter(babies > 1 | children > 1 & (hotel == "City Hotel"))
city_hotels <- hotels %>% filter(hotel == "City Hotel")
resort_hotels <- hotels %>%  filter(hotel == "Resort Hotel")
(count(city_baby_hotels) / count(city_hotels) > count(resort_baby_hotels) / count(resort_hotels))
#the code above returns false, meaning that a larger portion of resort hotels have a 
#child or a baby in the booking
#both resort and city hotels have booking with babies or kids
count(resort_baby_hotels) 
count(city_baby_hotels) 
nrow(city_baby_hotels) == count(city_baby_hotels) #returns true
hotels_adults_freq <- hotels %>% count(adults) %>% arrange(desc(n))
glimpse(hotels_adults_freq)
hotels_adults_freq
#the most common amount of adults per booking is two. Its surprising that
#bookings with 26 adults are just as common as bookings with 5 adults
hotels_canceled_freq <- hotels %>% count(is_canceled) %>% arrange(desc(n))
glimpse(hotels_canceled_freq)
#the high cancellation rate suggests that the bookings agent or service may be 
#combining bookings because of cancellations, or canceling bookings to account for 
#changes in guests 
hotel_adr <- hotels %>% group_by(hotel) %>% summarise(min(adr), mean(adr), median(adr), max(adr))
hotel_adr
#resort max adr
resort_max_adr <- resort_hotels %>% group_by(arrival_date_month) %>% filter(adr == 508)
resort_max_adr
num_resort_guest_max_adr <- resort_max_adr$adults + resort_max_adr$children + resort_max_adr$babies
num_resort_guest_max_adr
resort_max_adr$arrival_date_year
resort_max_adr$arrival_date_month

#city max adr
city_max_adr <- city_hotels %>% group_by(arrival_date_month) %>% filter(adr == 5400)
num_city_guest_max_adr <- city_max_adr$adults + city_max_adr$children + city_max_adr$babies
num_city_guest_max_adr
city_max_adr$arrival_date_year
city_max_adr$arrival_date_month


#resort min adr
resort_min_adr <- resort_hotels %>% group_by(arrival_date_month) %>% filter(adr == -6.38)
resort_min_adr
num_resort_guest_min_adr <- resort_min_adr$adults + resort_min_adr$children + resort_min_adr$babies
num_resort_guest_min_adr
resort_min_adr$arrival_date_year
resort_min_adr$arrival_date_month

#city min adr
city_min_adr <- city_hotels %>% group_by(arrival_date_month) %>% filter(adr == 0)
city_min_adr
num_city_guest_min_adr <- city_min_adr$adults + city_min_adr$children + city_min_adr$babies
num_city_guest_min_adr
city_min_adr$arrival_date_year
city_min_adr$arrival_date_month

#resort avg adr
resort_avg_adr <- resort_hotels %>% group_by(arrival_date_month) %>% filter(adr == 95)
resort_avg_adr
num_resort_guest_avg_adr <- resort_avg_adr$adults + resort_avg_adr$children + resort_avg_adr$babies
num_resort_guest_avg_adr
resort_avg_adr$arrival_date_year
resort_avg_adr$arrival_date_month

#city avg adr
city_avg_adr <- city_hotels %>% group_by(arrival_date_month) %>% filter(adr == 105)
city_avg_adr
num_city_guest_avg_adr <- city_avg_adr$adults + city_avg_adr$children + city_avg_adr$babies
num_city_guest_avg_adr
city_avg_adr$arrival_date_year
city_avg_adr$arrival_date_month


#resort med adr

resort_med_adr <- resort_hotels %>% group_by(arrival_date_month) %>% filter(adr == 75)
resort_med_adr
num_resort_guest_med_adr <- resort_med_adr$adults + resort_med_adr$children + resort_med_adr$babies
num_resort_guest_med_adr
resort_med_adr$arrival_date_year
resort_med_adr$arrival_date_month

#city med adr

city_med_adr <- city_hotels %>% group_by(arrival_date_month) %>% filter(adr == 99.9)
city_med_adr
num_city_guest_med_adr <- city_med_adr$adults + city_med_adr$children + city_med_adr$babies
num_city_guest_med_adr
city_med_adr$arrival_date_year
city_med_adr$arrival_date_month