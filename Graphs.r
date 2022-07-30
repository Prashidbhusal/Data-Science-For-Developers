library(tidyverse)
library(dplyr)
library(scales)
library(fmsb)

library(ggrepel)




setwd("c:/")

setwd('Users/ASUS/Desktop/datascience')

euro <- dollar_format(prefix = "\u20ac", big.mark = ",")


#-------------------------graoh of broadband speed---------------------
Towns = read_csv("CleanedData/Towns.csv")%>% 
  select(shortPostcode, Town, District, County)
BroadbandSpeedsclean=read_csv("CleanedData/cleanBroadbandSpeeds.csv")

broadband=Towns %>% 
  left_join(BroadbandSpeedsclean,by="shortPostcode")

ggplot(broadband,aes(y=Town)) +
  labs(x="Speeds (Mbits/s)",y="Town",title="GREATER MANCHESTER Broadband Speeds")+
  geom_bar(data=filter(broadband,County=="GREATER MANCHESTER"),aes(x=MaxDownload,fill="Maximum"),stat="Identity")+
  geom_bar(data=filter(broadband,County=="GREATER MANCHESTER"),aes(x=AverageDownload,fill="Average"),stat="Identity")+
  guides(fill=guide_legend("Download Speeds"))

#---------------MERSEYSIDE Broadband Speeds---------------
ggplot(broadband,aes(y=Town)) +
  labs(x="Speeds (Mbits/s)",y="Town",title="MERSEYSIDE Broadband Speeds")+
  geom_bar(data=filter(broadband,County=="MERSEYSIDE"),aes(x=MaxDownload,fill="Maximum"),stat="Identity")+
  geom_bar(data=filter(broadband,County=="MERSEYSIDE"),aes(x=AverageDownload,fill="Average"),stat="Identity")+
  guides(fill=guide_legend("Download Speeds"))

#---------------Average download speed---------------
broadband %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = `AverageDownload`, fill=District)) +
  scale_y_continuous(breaks = seq(0,200,10)) +
  geom_boxplot() +
  labs(title = "Average download speed (Mbit/s) by district", x = "District",
       y = "Average Download Speed (Mbit/s)")+
  coord_flip()

#--------------broadband end-----------------------------



#---------------graph of Average house price ------------------
Towns = read_csv("CleanedData/Town.csv")
HousePrices=read_csv("CleanedData/HousePricesclean.csv")

HousePricesclean <- HousePrices %>% 
  left_join(Towns, by ="shortPostcode")

House_town = HousePricesclean %>% 
  filter(County=="GREATER MANCHESTER"|County=="MERSEYSIDE") %>% 
  group_by(Town,District,County) %>% 
  summarise(AveragePrice= mean(Price)) %>% 
  ungroup(Town,District,County) %>%
  na.omit()

# BOXPLOT Average house prices by district (2019-2021)
House_town %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = AveragePrice, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2019-2021 house prices by district")



# BARGRAPH houseprices by district (2021)
HousePricesclean %>% 
  filter(Year == 2021) %>% 
  group_by(District) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  ggplot(aes(x = District, y = AveragePrice)) +
  geom_bar(position = "stack",stat = "identity", fill = "cornflowerblue") +
  scale_y_continuous(limits=c(0,5000000),breaks = seq(0, 5000000, 30000), 
                     label = euro) +
  geom_text(aes(label = euro(AveragePrice)), 
            vjust = -0.25) +
  labs(title = "2021 Average house prices by district") +
  coord_flip()


#LINEGRAPH Average house prices by year (2019-2021)
HousePricesclean %>% 
  group_by(Year) %>% 
  summarise(AveragePrice = mean(Price)) %>% 
  ggplot(aes(x = Year, y = AveragePrice)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_text(aes(label = euro(AveragePrice)), 
            vjust = -0.85) +
  scale_y_continuous(breaks = seq(0, 300000, 5000), 
                     label = euro) +
  scale_x_continuous(breaks = 2019:2021) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "2019-2021 Average house prices by year")


# BOXPLOT Average house prices by district (2019-2021)
HousePricesclean %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = Price, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2019-2021 house prices by district")



# BOXPLOT Average house prices by district (2021)
HousePricesclean %>% 
  filter(Year == 2021) %>% 
  group_by(District) %>% 
  ggplot(aes(x = District, y = Price, fill=District)) +
  scale_y_continuous(limits=c(0,2000000), breaks = seq(0,2000000,200000), 
                     label = euro) +
  geom_boxplot() +
  coord_flip() +
  labs(title="2021 house prices by district")

#-------------end------------------------------------------

#-----------crime graph-------------------

Town  = read_csv("CleanedData/Town.csv")
crime_Data = read_csv("Cleaneddata/cleanCrimes.csv")



crimeData = crime_Data %>% 
  left_join(Towns, by = "shortPostcode") %>% 
  na.omit()


# Boxplot for 2019-2021 Drugs count by District
crimeData %>% 
  filter(CrimeType == "Drugs") %>% 
  ggplot(aes(x=District, y=n, fill=CrimeType)) + 
  geom_boxplot() +
  labs(title=" 2019-2021 Drugs count by District")+
  coord_flip()



# Piechart for 2021 Robbery by District
RobberyData <- crimeData %>% 
  filter(CrimeType=="Robbery", Year == 2021) %>%
  group_by(Town) %>%
  mutate(sumCount = sum(n)) %>% 
  ungroup() %>%
  mutate(perc =sumCount / sum(n)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>% 
  distinct(Town, sumCount, perc, labels) %>% 
  select(Town, sumCount, perc, labels)

RobberyData %>% 
  ggplot(aes(x = "", y = perc, fill = Town)) +
  geom_col(color = "white") +
  geom_label(aes(label = labels),color="black",
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y") +
  theme_void()+
  labs(title="2021 Robbery by District")



#-----------------school graph---------------

schoolData = read_csv('CleanedData/School.csv', show_col_types = FALSE)

liverpoolSchool = read_csv("CleanedData/liverpoolSchoolData.csv")
manchesterSchoolData = read_csv('CleanedData/manchesterSchoolData.csv')


# Linegraph Average Attainment8Score by year
schoolData %>% 
  group_by(Year) %>% 
  summarise(AverageAttainment = mean(Attainment8Score)) %>% 
  ggplot(aes(x = Year, y = AverageAttainment)) +
  geom_line(size = 1.5, 
            color = "lightgrey") +
  geom_text(aes(label = AverageAttainment), 
            vjust = -0.85) +
  scale_x_continuous(breaks = 2016:2019) +
  geom_point(size = 2, 
             color = "steelblue")+
  labs(title = "Average Attainment8Score by year")


# Boxplot of year 2016-2019 where Attainment8Score is greater than 30
schoolData %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2016-2019 Attainment8Score of Schools")
schoolData

# Boxplot of year 2016-2019 where Attainment8Score is greater than 30 (LIVERPOOL SCHOOL ONLY)
liverpoolSchool %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2016-2019 Average Attainment8Score of Liverpool Schools")



# Boxplot of year 2016-2019 where Attainment8Score is greater than 30 (MANCHESTER SCHOOL ONLY)
manchesterSchoolData %>% 
  filter(Attainment8Score>30) %>% 
  ggplot(aes(x = SchoolName, y = Attainment8Score)) +
  scale_y_continuous(breaks = seq(0, 80, 5))+
  geom_boxplot() +
  coord_flip() +
  labs(title="2016-2019 Average Attainment8Score of Manchester Schools")

