#Clear Workspace
dev.off()
cat("\014")
rm(list=ls())
set.seed(18552)

library(tidyr)
library(zoo)
library(readxl)
library(lubridate)
library(ggplot2)
library(scales)
library(sp)
library(rgdal)
library(cowplot)
library(ggpubr)
library(maps)
library(sf)
library(GISTools)
library(readxl)
library(dplyr)
library(gganimate)
library(png)
library(gapminder)
library(gifski)
library(reshape2)
library(ggthemes)

# CASES
# 1) country wise [line graph animation]
# https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases
country.data<-read.csv("/Users/bhaveshshah/Desktop/School/DataViz/worldometer_coronavirus_daily_data.csv")

india.data<-subset(country.data, country.data$country==("India"))
india.data<-na.omit(india.data)
india.data<-subset(india.data, india.data$date>as.Date("2021-03-01"))

usa.data<-subset(country.data, country.data$country==("USA"))
usa.data<-na.omit(usa.data)
usa.data<-subset(usa.data, usa.data$date>as.Date("2021-03-01"))

brazil.data<-subset(country.data, country.data$country==("Brazil"))
brazil.data<-na.omit(brazil.data)
brazil.data<-subset(brazil.data, brazil.data$date>as.Date("2021-03-01"))

france.data<-subset(country.data, country.data$country==("France"))
france.data<-na.omit(france.data)
france.data<-subset(france.data, france.data$date>as.Date("2021-03-01"))

country_tracker<-ggplot()+
  geom_line(india.data, mapping = aes(x=date, y=daily_new_cases, group = 1, color = "India"), lwd=1.5)+
  geom_line(usa.data, mapping = aes(x=date, y=daily_new_cases, group = 1, color = "USA"))+
  geom_line(brazil.data, mapping = aes(x=date, y=daily_new_cases, group = 1, color = "Brazil"))+ 
  geom_line(france.data, mapping = aes(x=date, y=daily_new_cases, group = 1, color = "France"))+
  ggtitle("Daily New Covid-19 Cases for Last 3 Months in \nSignificantly Affected Countries")+
  labs(y= "Daily New Cases", x = "Dates (March 1st, 2021 - Current)", color= "Country")+
  theme(axis.text.x=element_blank(), plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(color = 'black'))
  scale_colour_manual("", breaks = c("India", "USA", "Brazil", "France"), values = c("darkorange", "dodgerblue4", "forestgreen", "firebrick3"))

country_tracker
country_tracker_anim<-country_tracker+transition_reveal(along = as.Date(date))
animate(country_tracker_anim, renderer=gifski_renderer())

anim_save("Country_Tracker.gif",animation=country_tracker_anim, renderer=gifski_renderer())

----------------------------------------------------------------------
# MEDICAL FACILITIES/ EQUIPMENT
# 2) health expenditure -- percent GDP [inverted doubly bar graph]
# https://data.worldbank.org/indicator/SH.XPD.CHEX.GD.ZS
# also https://www.worldometers.info/gdp/gdp-by-country/
healthGDP.data<-data.frame(
  name=c("United States", "China", "Japan", "Germany", "India", "United Kingdom", "France", "Brazil", "Italy", "Canada", "Russia", "South Korea", "Australia", "Spain", "Mexico", "Indonesia", "Turkey", "Netherlands", "Saudi Arabia", "Switzerland"),
  percentage=c(16.89, 5.35, 10.95, 11.43, 3.54, 10.00, 11.26, 9.51, 8.67, 10.79, 5.32, 7.56, 9.28, 8.98, 5.37, 2.87, 4.12, 9.97, 6.36, 11.88)
)

healthGDP.data$gdp_adj <- (healthGDP.data$percentage-mean(healthGDP.data$percentage))/sd(healthGDP.data$percentage) # calculate mpg_z

# create binary variable out of gdp_adj
healthGDP.data$gdp_adj_simplified[healthGDP.data$gdp_adj<0]<-"Below" 
healthGDP.data$gdp_adj_simplified[healthGDP.data$gdp_adj>=0]<-"Above"

healthGDP.data

Healthcare_GDP<-ggplot(data=healthGDP.data, mapping=aes(x=reorder(name, -gdp_adj), y=gdp_adj, fill=gdp_adj_simplified))+
  geom_bar(stat="identity")+
  coord_flip()+
  ggtitle("Average Percentage GDP Spent on Healthcare for\nTop 15 Countries by GDP")+
  labs(y= "Standard Devisions Away from Average Percentage Spent", 
       x = "Country", 
       fill="Percentage")+
  scale_fill_manual(values=c("mediumaquamarine", "sienna1"))

ggsave(path = "./", filename = "Healthcare_GDP.png", plot = Healthcare_GDP)

# 3) # of hospitals/ beds [histogram]
# https://www.kaggle.com/praveengovi/stopcoronatn-wave-2
hospital.data<-read_excel("/Users/bhaveshshah/Desktop/School/DataViz/Tamil Nadu COVID Bed status-2021-04-19.xlsx", sheet="data") 

summary(hospital.data)

Hospital_Beds<-ggplot(data=hospital.data, aes(x=`COVID BEDS Vacant`))+
  geom_histogram(stat="count", color='coral1', fill='coral1')+
  ggtitle("Number of Vacant Beds per Hospital")+ 
  labs(y= "Frequency", x = "Number of Beds")+
  theme(plot.title=element_text(hjust=0.5, size = 20))+
  xlim(c(0, 85))+
  ylim(c(0, 22))

# 4) oxygen tanks shortage [histogram]
Oxygen_Beds<-ggplot(data=hospital.data, aes(x=`OXYGEN SUPPORTED BEDS Vacant`))+
  geom_histogram(stat="count", color='purple4', fill='purple4')+
  ggtitle("Number of Vacant Oxygen-Supported Beds per Hospital")+ 
  labs(y= "Frequency", x = "Number of Beds")+
  theme(plot.title=element_text(hjust=0.5, size = 20))+
  xlim(c(0, 85))+
  ylim(c(0, 30))

# 5) ventilators [histogram]
Ventilators<-ggplot(data=hospital.data, aes(x=`VENTILATOR Vacant`))+
  geom_histogram(stat="count", color='royalblue1', fill='royalblue1')+
  ggtitle("Number of Vacant Ventilators per Hospital")+ 
  labs(y= "Frequency", x = "Number of Ventilators")+
  theme(plot.title=element_text(hjust=0.5, size = 20))

ggsave(path = "./", filename = "Hospital_Beds.png", plot = Hospital_Beds)
ggsave(path = "./", filename = "Oxygen_Beds.png", plot = Oxygen_Beds)
ggsave(path = "./", filename = "Ventilators.png", plot = Ventilators)

----------------------------------------------------------------------
# VACCINES
# 6) number of people vaccinated -- vaccinated to entire population [stacked bar]
# downloaded dataset - https://github.com/covid19india/api
# Read in India map.
india.map<-st_read("/Users/bhaveshshah/Desktop/School/DataViz/India_States_ADM1_GADM-shp/3e563fd0-8ea1-43eb-8db7-3cf3e23174512020330-1-layr7d.ivha.shp")

# Read in dataset and clean it up.
dta<-read.csv("/Users/bhaveshshah/Desktop/School/DataViz/covid_vaccine_statewise.csv")
dta<-subset(dta, dta$State!=("India"))
View(dta)

# Create hashmap corresponding states with populations.
states<-c("Uttar Pradesh", "Maharashtra", "Bihar", "West Bengal", "Madhya Pradesh", "Tamil Nadu", "Rajasthan",
          "Karnataka", "Gujarat", "Andhra Pradesh", "Odisha", "Telangana", "Kerala", "Jharkhand",
          "Assam", "Punjab", "Chhattisgarh", "Haryana", "Delhi", "Jammu and Kashmir", "Uttarakhand", "Himachal Pradesh",
          "Tripura", "Meghalaya", "Manipur", "Nagaland", "Goa", "Arunachal Pradesh", "Puducherry", "Mizoram", "Chandigarh",
          "Sikkim", "Dadra and Nagar Haveli and Daman and Diu", "Andaman and Nicobar Islands", "Ladakh", "Lakshadweep")
populations<-c(199812341, 112374333, 104099452, 91276115, 72626809, 72147030, 68548437, 61095297, 60439692, 49577103,
               41974219, 35003674, 33406061, 32988134, 31205576, 27743338, 25545198, 25351462, 16787941, 12267032,
               10086292, 6864602, 3673917, 2966889, 2570390, 1978502, 1458545, 1383727, 1247953, 1097206, 1055450,
               610577, 585764, 380581, 274000, 64473)
hashmap<-setNames(as.list(populations), states)

# Calculate vaccinated percentage by dividing # of people vaccinated by total population, and then multiplying by 100.
dta$vaccinated_percentage<-100 * (as.numeric(dta$Total.Individuals.Vaccinated) / as.numeric(unlist(hashmap[dta$State])))
View(dta)

# Create a dummy variable to plot.
dta$vaccinated_percentage_dummy[as.numeric(dta$vaccinated_percentage)<=5]<-0
dta$vaccinated_percentage_dummy[5<as.numeric(dta$vaccinated_percentage) & 
                                  as.numeric(dta$vaccinated_percentage)<=10]<-1
dta$vaccinated_percentage_dummy[10<as.numeric(dta$vaccinated_percentage) & 
                                  as.numeric(dta$vaccinated_percentage)<=15]<-2
dta$vaccinated_percentage_dummy[15<as.numeric(dta$vaccinated_percentage) & 
                                  as.numeric(dta$vaccinated_percentage)<=20]<-3
dta$vaccinated_percentage_dummy[20<as.numeric(dta$vaccinated_percentage)]<-4
table(dta$vaccinated_percentage_dummy)
View(dta)

# Merge the data.
india.map$State<-india.map$NAME_1
india.vaccine.map<-merge(india.map, dta, by='State')

map.theme<-theme(axis.line=element_blank(),axis.text.x=element_blank(),
                 axis.text.y=element_blank(),axis.ticks=element_blank(),
                 axis.title.x=element_blank(),
                 axis.title.y=element_blank(),
                 panel.background=element_blank(),panel.border=element_blank(),
                 panel.grid.major=element_blank(),
                 panel.grid.minor=element_blank(),plot.background=element_blank())

# Subset date
April24<-subset(india.vaccine.map, india.vaccine.map$Updated.On==("24/04/2021"))

table(April24$vaccinated_percentage_dummy)

April24.plot<-ggplot(data=April24)+
  geom_sf(aes(fill=as.factor(vaccinated_percentage_dummy)))+
  scale_fill_manual(values=c("white", "cadetblue1", "cadetblue2", "cadetblue3", "cadetblue4"), name="Percent Vaccinated in State", 
                    limits=c("0", "1", "2", "3", "4"),
                    labels=c("Less than 5%", "Between 5% and 10%", "Between 10% and 15%", "Between 15% and 20%", "Greater than 20%"), 
                    drop=FALSE)+
  map.theme+
  ggtitle("Percent of People Vaccinated Against Covid-19 (April 24, 2021)")+
  theme(plot.title = element_text(hjust = .5, size = 14)) 
  
ggsave(path = "./", filename = "India_Map.png", plot = April24.plot)

----------------------------------------------------------------------
# 7) age [donut chart] 
# https://censusindia.gov.in/Census_And_You/age_structure_and_marital_status.aspx
age.data<-data.frame(
  ranges=c("0-4", "5-24", "25-44", "45-64", "65+"),
  percentages=c(10.7, 43, 27.6, 13.5, 4.8)
)

age.data$lab = paste(age.data$percentage, "%")

age.data$ranges

theme_void()+  
  
Age.Breakdown<-ggplot(data=age.data, aes(x=2, y=percentages, fill=ranges))+
  geom_bar(stat="identity", size=1, width=1, color = "white")+
  labs(title="Breakdown of Indian Population by Age", fill="Age Groups")+
  coord_polar(theta="y", start=0)+
  geom_text(aes(label=lab), position=position_stack(vjust=0.5))+
  xlim(0.5, 2.5)+
  theme_economist()+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.title.x=element_blank(),
        axis.text.x=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_blank())

ggsave(path = "./", filename = "Age_Breakdown.png", plot = Age.Breakdown)