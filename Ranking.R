library(tidyverse)


setwd("c:/")
setwd('Users/Mr. Bablu/Desktop/datascience/')



#---------------house rank------
Town = read_csv("CleanedData/Town.csv")%>% 
  select(shortPostcode, Town, District, County)

House_price = read_csv("CleanedData/housePricesclean.csv")

#house rank
Houseprice= House_price %>%
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()
housePrices=Houseprice  %>% 
  filter(Year=="2020") %>% 
  group_by(Town) %>% 
  summarise(Price=mean(Price)) %>% 
  arrange(Price) %>% 
  mutate(HouseScore=10-(Price/120000)) %>% 
  select(Town, HouseScore)
housePrices


#download rank

speed_downloads = read_csv("CleanedData/cleanBroadBandspeeds.csv")

Speed_Download = speed_downloads %>%
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()

download_speed=Speed_Download%>% 
  group_by(Town) %>% 
  summarise(downloadSpeed=AverageDownload) %>% 
  arrange(downloadSpeed) %>% 
  mutate(DownloadScore=10-(downloadSpeed/120000)) %>% 
  select(Town,DownloadScore)
download_speed



#crime score rank
crime_score=read_csv("CleanedData/cleanCrimes.csv")
crime_rank = crime_score %>%
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()


crime_rank=crime_rank%>% 
  group_by(Town) %>% 
  summarise(score=mean(n)) %>% 
  arrange(score) %>% 
  mutate(score=10-(score/1200)) %>% 
  select(Town,score)
crime_rank


#school score
school_score=read_csv("CleanedData/School.csv")
school_rank = school_score %>%
  rename(shortPostcode=shortPostCode) %>% 
  left_join(Towns,by="shortPostcode") %>% 
  na.omit()

school_rank=school_rank%>% 
  group_by(District) %>% 
  summarise(score=mean(Attainment8Score)) %>% 
  arrange(score) %>% 
  mutate(score=10-(score/1800)) %>% 
  select(District,score)
school_rank

