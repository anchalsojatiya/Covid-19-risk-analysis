#########################################
### Author:Anchal Sojatiya
### Date 04/10/2020
#########################################

#########################################
### Cleaning Workspace
#########################################

rm(list=ls())

#########################################
### Insatalling Packages and Loading Libraies
#########################################

library(ggplot2)
library(dplyr)
require(maps)
require(viridis)
library(varhandle)
library(plotly)
library(rvest)
library(reshape2)
library(hrbrthemes)
library(RColorBrewer)

#creating map
world_map <- map_data("world")
mapplot<-ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")

#Loading worldometer data
url<-"https://www.worldometers.info/coronavirus/"
worldometer <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="main_table_countries_today"]') %>%
  html_table()
worldometer <- worldometer[[1]]
for (i in names(worldometer[,-2])){
  worldometer[,i]<- gsub(',', '', worldometer[,i])
  worldometer[,i]<-as.numeric(worldometer[,i])
}

head(worldometer)
names(worldometer)[names(worldometer) == "Country,Other"] <- "region"
names(worldometer)[names(worldometer) == "Tests/1M pop"] <- "TestsPerMillion"
names(worldometer)[names(worldometer) == "Deaths/1M pop"] <- "DeathsPerMillion"
names(worldometer)[names(worldometer) == "Tot Cases/1M pop"] <- "TotalPerMillion"
#worldometer$Country<-as.factor(worldometer$Country)
world_total<-worldometer[1:8,]
worldometer<-worldometer[-231:-224,]
worldometer<-worldometer[-8:-1,]
worldometer <- within(worldometer, region[region == 'UAE'] <- 'United Arab Emirates')
worldometer <- within(worldometer, region[region == 'Czechia'] <- 'Czech Republic')
worldometer <- within(worldometer, region[region == 'North Macedonia'] <- 'Macedonia')
worldometer <- within(worldometer, region[region == 'DRC'] <- 'Democratic Republic of the Congo')
worldometer <- within(worldometer, region[region == 'Congo'] <- 'Republic of Congo')
worldometer <- within(worldometer, region[region == 'CAR'] <- 'Central African Republic')
worldometer <- within(worldometer, region[region == 'S. Korea'] <- 'South Korea')


worldometer$DeathRate<-(worldometer$TotalDeaths/worldometer$TotalCases)*100
worldometer$RecoveryRate<-(worldometer$TotalRecovered/worldometer$TotalCases)*100
worldometer<-worldometer[with(worldometer, order(-TotalCases)),]



#Total Cases

worldometer_map <- full_join(worldometer, world_map, by = "region")

ggplotly(ggplot(worldometer_map, aes(long, lat, group = group))+
           geom_polygon(fill="lightgray")+
  geom_polygon(aes(fill = TotalCases ,text = paste("Country:",region)), color = "lightgray",size = 0.1)+
    scale_fill_distiller(palette ="Reds", direction = 1,values = c(0,0.15,1))+
    ggtitle("Total Cases Worldwide"))



#Number of Cases
ggplotly(ggplot(worldometer_map, aes(long, lat, group = group))+
           geom_polygon(fill="lightgray")+
           geom_polygon(aes(fill = ActiveCases ,text = paste("Country:",region)), color = "lightgray",size = 0.1)+
           scale_fill_distiller(palette ="RdPu", direction = 1,values = c(0,0.2,1))+
           ggtitle("Currently Active Cases"))

#Number of Tests
ggplotly(ggplot(worldometer_map, aes(long, lat, group = group))+
           geom_polygon(fill="lightgray")+
           geom_polygon(aes(fill = TotalTests,text = paste("Country:",region,"Test Per Million:",TestsPerMillion)), color = "lightgray",size = 0.1)+
           scale_fill_distiller(palette ="YlOrRd", direction = 1,values = c(0,0.2,1))+
           ggtitle("Total Tests Per Million"))


##################################################
#Loading population data
##################################################


population_data <-read.table("covid19-country-data/covid19_data - population.csv",sep=",",header=TRUE)
names(population_data)[names(population_data) == "Country"] <- "region"
population_data$region<-as.character(population_data$region)

population_data <- within(population_data, region[region == 'United States'] <- 'USA')
population_data <- within(population_data, region[region == 'Congo'] <- 'Republic of Congo')
population_data <- within(population_data, region[region == 'United Kingdom'] <- 'UK')
population_data <- within(population_data, region[region == 'Czech Republic (Czechia)'] <- 'Czech Republic')
population_data <- within(population_data, region[region == 'North Macedonia'] <- 'Macedonia')
population_data <- within(population_data, region[region == 'Dominican Republic'] <- 'Democratic Republic of the Congo')


population_data$Density_KM2m<-as.numeric(gsub(",", "", as.character(population_data$Density_KM2m)))
population_data$Population_2020<-as.numeric(gsub(",", "", as.character(population_data$Population_2020)))
population_data$Median_age<-as.numeric(population_data$Median_age)

population_map <- full_join(population_data, world_map, by = "region")
ggplotly(ggplot(population_map, aes(long, lat, group = group))+
           geom_polygon(aes(fill = Population_2020 ,text = paste("Country:",region,"<br>Density/km2:",Density_KM2m,"<br>Rank:",Rank)), color = "lightgray",size = 0.1)+
           scale_fill_distiller(palette ="BuPu", direction = 1,values = c(0,0.15,1))+ggtitle("Population "))

##################################################
#Loading age data
##################################################



age_data <-read.table("covid19-country-data/covid19_data - age.csv",sep=",",header=TRUE,fileEncoding = "UTF-8")
names(age_data)[names(age_data) == "Country"] <- "region"

age_data$region<-trimws(age_data$region, which = c("both"), whitespace = "[ \\s]")
age_data <- within(age_data, region[region == 'Aruba (Netherlands)'] <- 'Aruba')
age_data <- within(age_data, region[region == 'Antigua and Barbuda'] <- 'Antigua')
age_data <- within(age_data, region[region == 'United States'] <- 'USA')
age_data <- within(age_data, region[region == 'Republic of the Congo'] <- 'Republic of Congo')
age_data <- within(age_data, region[region == 'United Kingdom'] <- 'UK')



for (x in colnames(age_data[2:4]))
{
  age_data[x] <- sapply(age_data[x], function(z) as.numeric(gsub("%", "", as.character(z))))[1:dim(age_data)[1]]
  
}

age_data$region<-as.character(age_data$region)
age_map <- full_join(age_data, world_map, by = "region")
ggplotly(ggplot(age_map, aes(long, lat, group = group))+
           geom_polygon(aes(fill = age_over_65_years ,text = paste("Country:",region)), color = "lightgray",size = 0.1)+
           scale_fill_distiller(palette ="PuBuGn", direction = 1)+ggtitle("Percentage of population above 65"))


##################################################
#Loading Health data
##################################################
health_data <-read.table("covid19-country-data/covid19_data - health.csv",sep=",",header=TRUE,fileEncoding = "UTF-8")
names(health_data)[names(health_data) == "Country"] <- "region"
health_data$region<-as.character(health_data$region)
health_data <- within(health_data, region[region == 'United States'] <- 'USA')
health_data <- within(health_data, region[region == 'United Kingdom'] <- 'UK')



health_map <- full_join(health_data, world_map, by = "region")
ggplotly(ggplot(health_map, aes(long, lat, group = group))+
           geom_polygon(aes(fill = Health_Care_Index ,text = paste("Country:",region,"Rank:",Rank)), color = "lightgray",size = 0.1)+
           scale_fill_distiller(palette ="PuBuGn", direction = 1)+ggtitle("Health Care Index"))


##################################################
#Loading Air Traffic data data
##################################################
air_data_all <-read.table("covid19-country-data/covid19_data - airport_traffic_world -clean.csv",sep=",",header=TRUE)
names(air_data_all)[names(air_data_all) == "Country.Name"] <- "region"
air_data_all<-air_data_all[with(air_data_all, order(-airport_traffic_2018_thousands)),]
air_data_all<-air_data_all[1:30,]
ggplotly(ggplot(data=air_data_all, aes(x=reorder(region, -airport_traffic_2018_thousands), y=airport_traffic_2018_thousands,
                                       text=paste0("Region: ",region,"<br>Air Traffic:",airport_traffic_2018_thousands))) +
           geom_bar(stat="identity")+ theme(axis.text.x=element_text(angle=45, hjust=1))+
           geom_col(aes(fill = reorder(Country.Code, -airport_traffic_2018_thousands)))+
           theme(legend.position = "none")+xlab("Country")+ylab("Air Taffic")+
           ggtitle("Air Taffic Across different regions"),tooltip = "text")



air_data <-read.table("covid19-country-data/covid19_data - airport_traffic_world.csv",sep=",",header=TRUE)
names(air_data)[names(air_data) == "Country.Name"] <- "Country"
air_data$Country<-as.character(air_data$Country)


lat_lon <-read.table("covid19-country-data/covid19_data - lat_long.csv",sep=",",header=TRUE)
names(lat_lon)[names(lat_lon) == "country_name"] <- "Country"
lat_lon$Country<-as.character(lat_lon$Country)


air_data <- within(air_data, Country[Country == 'Russian'] <- 'Russia')
air_data <- within(air_data, Country[Country == 'Iran, Islamic Rep.'] <- 'Iran')



air_map <- full_join(air_data, lat_lon, by = "Country")

ggplotly(ggplot(data=air_map,aes(x=longitude, y=latitude,
                                 text=paste0("Country: ",Country,"<br>Air Traffic:",airport_traffic_2018_thousands)))+
           geom_jitter(aes(col=airport_traffic_2018_thousands, size=airport_traffic_2018_thousands),alpha=0.7) +
           scale_size(range = c(1, 15))+scale_color_gradientn(colours = rainbow(15))+
           labs(title="Air Traffic Across World",
                x="Longitude", y = "Latitude"),tooltip = "text")


worldometer_g <- subset(worldometer, ActiveCases > 100000 | TotalDeaths >1000)
ggplotly(ggplot(data=worldometer_g)+
           geom_jitter(aes(x=DeathRate, y=RecoveryRate,col=region, size=TotalCases),alpha=0.7) +
           scale_size(range = c(1, 15))+
           labs(title="Death Rate vs Recovery Rate of Top Affected Countries",
                x="Death Rate", y = "Recovery Rate"))

##################################################
#Loading Hospital data
##################################################
bed_data <-read.table("covid19-country-data/covid19_data - hospital_beds.csv",sep=",",header=TRUE,fileEncoding = "UTF-8")
names(bed_data)[names(bed_data) == "Country.territory"] <- "Country"

ggplotly(ggplot(data=bed_data, aes(x=reorder(Country, -hosp_beds_per_1000_2016), y=hosp_beds_per_1000_2016,
                                   text=paste0("Country: ",Country,"<br>Beds:",hosp_beds_per_1000_2016))) +
           geom_bar(stat="identity",fill="steelblue", na.rm = TRUE)+ theme(axis.text.x=element_text(angle=45, hjust=1))+
           theme(legend.position = "none")+xlab("Country")+ylab("Beds per 1000")+
           ggtitle("Beds per 1000 in 2016"),tooltip = "text")


##################################################
#Loading GDP data
##################################################
gdp_data <-read.table("covid19-country-data/covid19_data - gdp.csv",sep=",",header=TRUE,fileEncoding = "UTF-8")
names(gdp_data)[names(gdp_data) == "Country.Territory"] <- "region"

gdp_data$region<-as.character(gdp_data$region)
gdp_data$GDP_USD_Million<-as.numeric(gsub(",", "", as.character(gdp_data$GDP_USD_Million)))

gdp_data$region<-trimws(gdp_data$region, which = c("both"), whitespace = "[ \\s]")
gdp_data <- within(gdp_data, region[region == 'Aruba (Netherlands)'] <- 'Aruba')
gdp_data <- within(gdp_data, region[region == 'Antigua and Barbuda'] <- 'Antigua')
gdp_data <- within(gdp_data, region[region == 'United States'] <- 'USA')
gdp_data <- within(gdp_data, region[region == 'Congo, Republic of the'] <- 'Republic of Congo')
gdp_data <- within(gdp_data, region[region == 'United Kingdom'] <- 'UK')
gdp_data <- within(gdp_data, region[region == 'Congo, Democratic Republic of the'] <- 'Democratic Republic of the Congo')



gdp_map <- full_join(gdp_data, world_map, by = "region")
ggplotly(ggplot(gdp_map, aes(long, lat, group = group))+
           geom_polygon(aes(fill = GDP_USD_Million ,text = paste("Country:",region,"Rank:",Rank)), color = "lightgray",size = 0.1)+
           scale_fill_distiller(palette ="PuBuGn", direction = 1,values = c(0,0.04,1))+ggtitle("GDP"))


