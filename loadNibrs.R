
##load requisite libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(usmap)
library(rgdal)
library(leaflet)
library(stringr)

##set working directory
setwd("~/R/OhioJustice")

##download NIBRS dataset into local working environment

##unzip NIBRS dataset

##load county/agency merge file

agencyRaw <- read.csv("./NIBRS_Data_Prep/OH/agencies.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

##load incident file
incidentRaw <- read.csv("./NIBRS_Data_Prep/OH/NIBRS_incident.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

##load county population file
populationRaw <- read.csv("./co-est2019-alldata.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

##load arrestee file
arresteeRaw <- read.csv("./NIBRS_Data_Prep/OH/NIBRS_ARRESTEE.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

##load American Community Survey attributes
acSurveyRaw <- read.csv("./American_Community_Survey.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

##load offenses
offenseRaw <- read.csv("./NIBRS_Data_Prep/OH/NIBRS_OFFENSE.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)

##load offense translation
offenseDecodeRaw <- read.csv("./NIBRS_Data_Prep/OH/NIBRS_OFFENSE_TYPE.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)




##transform county population file

countyPopulation <- populationRaw %>%
  select(STNAME, CTYNAME, CENSUS2010POP) %>%
  filter(STNAME == "Ohio" & CTYNAME != "Ohio") 

countyPopulation$CTYNAME <- gsub(" County", "", countyPopulation$CTYNAME)
countyPopulation$CTYNAME <- toupper(countyPopulation$CTYNAME)

##transform acSurvey file

acSurvey <- acSurveyRaw
acSurvey$County <-gsub(" County", "", acSurvey$County)
acSurvey$County <- toupper(acSurvey$County)

##create incidents by agency summary

incidentByAgency <- incidentRaw %>%
  group_by(AGENCY_ID) %>%
  summarize(incidentCount=n())

##create incidents by county via agency

incidentByCounty <- left_join (incidentRaw, agencyRaw, by = c("AGENCY_ID" = "AGENCY_ID")) %>%
  group_by(COUNTY_NAME) %>%
  summarize(incidentCount=n()) %>%
  ## for now, filtering out dirty counties
  filter(!grepl(";", COUNTY_NAME))

##create average arrestee age per county

arresteeJoin <- left_join (arresteeRaw, incidentRaw, by = c("INCIDENT_ID" = "INCIDENT_ID")) %>%
  left_join(., agencyRaw, by = c("AGENCY_ID" = "AGENCY_ID"))

##create county average arrestee age summary
arresteeAgeAvg <- arresteeJoin %>%
  group_by(COUNTY_NAME) %>%
  summarize(AVERAGE_AGE = mean(AGE_NUM), ARREST_COUNT = n())


##for cross-county counts, create an algorith to split counts
  multiCounty <- left_join (incidentRaw, agencyRaw, by = c("AGENCY_ID" = "AGENCY_ID")) %>%
    group_by(COUNTY_NAME) %>%
    summarize(incidentCount=n()) %>%
    ## for now, filtering in dirty counties
    filter(grepl(";", COUNTY_NAME))
  
  multiCounty <- left_join (multiCounty, arresteeAgeAvg, by = c("COUNTY_NAME"="COUNTY_NAME")) %>%
    select(!AVERAGE_AGE)
  

  ##fix values for Columbus - need to get more granular on agency alignment for prod version
  multiCounty$COUNTY_NAME <- gsub("DELAWARE; FAIRFIELD; FRANKLIN", "FRANKLIN", multiCounty$COUNTY_NAME)
  
  
  multiCounty <- mutate(multiCounty, divFactor = str_count(multiCounty$COUNTY_NAME, ";")) 
  multiCounty <- mutate(multiCounty, divFactor = divFactor + 1) 
  multiCounty <- mutate(multiCounty, incidentSplit = incidentCount / divFactor) 
  multiCounty <- mutate(multiCounty, arrestSplit = ARREST_COUNT / divFactor)
  multiCounty <- separate_rows(multiCounty, COUNTY_NAME)
  multiCounty <- group_by (multiCounty, COUNTY_NAME) %>%
    summarize(incidentSplit=sum(incidentSplit), arrestSplit=sum(arrestSplit))

  ##after summarizing splitIncidents, arrestSplit into counties, rejoin incidentByCounty
  incidentByCounty <- left_join (incidentByCounty, multiCounty, by = c("COUNTY_NAME" = "COUNTY_NAME")) %>%
    left_join (., arresteeAgeAvg, by = c("COUNTY_NAME" = "COUNTY_NAME"))
  
  ##remove the NAs 
  incidentByCounty[is.na(incidentByCounty)] <- 0

  ##final cleaning
  incidentByCounty <- mutate(incidentByCounty, 
                             incidentCount = incidentCount + incidentSplit, 
                             ARREST_COUNT = ARREST_COUNT + arrestSplit) %>%
    select(COUNTY_NAME, incidentCount, AVERAGE_AGE, ARREST_COUNT)

  incidentByCounty <- mutate(incidentByCounty, incidentCount = round(incidentCount))
  incidentByCounty <- mutate(incidentByCounty, AVERAGE_AGE = round(AVERAGE_AGE))
  incidentByCounty <- mutate(incidentByCounty, ARREST_COUNT = round(ARREST_COUNT))
    


##add county population and calculate avg incidents & arrests per 1,000

incidentByCounty <- left_join (incidentByCounty, countyPopulation, by = c("COUNTY_NAME" = "CTYNAME")) 
  
incidentByCounty <- mutate(incidentByCounty, incidentRate1000 = incidentByCounty$incidentCount*1000/incidentByCounty$CENSUS2010POP)

incidentByCounty <- mutate(incidentByCounty, incidentRate1000 = round(incidentRate1000))

incidentByCounty <- mutate(incidentByCounty, arrestRate1000 = incidentByCounty$ARREST_COUNT*1000/incidentByCounty$CENSUS2010POP)

incidentByCounty <- mutate(incidentByCounty, arrestRate1000 = round(arrestRate1000))

##join the acSurvey data - poverty, median income
incidentByCounty <- left_join (incidentByCounty, acSurvey, by = c("COUNTY_NAME" = "County"))


##create an incident detail file to denormalize incident level info
##multi-county situations still exist for each incident

incidentDetail <- right_join (incidentRaw, offenseRaw, by = c("INCIDENT_ID" = "INCIDENT_ID")) %>%
  select(INCIDENT_ID, OFFENSE_ID, OFFENSE_TYPE_ID, AGENCY_ID)

incidentDetail <- left_join (incidentDetail, agencyRaw, by = c("AGENCY_ID" = "AGENCY_ID")) %>%
  select(INCIDENT_ID, OFFENSE_ID, OFFENSE_TYPE_ID, COUNTY_NAME)
  
incidentDetail <- left_join (incidentDetail, offenseDecodeRaw, 
                             by = c("OFFENSE_TYPE_ID" = "OFFENSE_TYPE_ID")) %>%
  select(INCIDENT_ID, OFFENSE_NAME, COUNTY_NAME) %>%
  group_by(OFFENSE_NAME, COUNTY_NAME) %>%
             summarize (OFFENSE_COUNT = n())
           


##load in state visual objects, need to align sexilly to directory structure
states <- readOGR(paste0("./NIBRS_Data_Prep/county/cb_2015_us_county_20m.shp"),
                  layer = "cb_2015_us_county_20m", GDAL1_integer64_policy = TRUE)

##get a statekey...?
Statekey<-read.csv( paste0('./NIBRS_Data_Prep/STATEFPtoSTATENAME_Key.csv'), colClasses=c('character'))

##add state key to state file
states<-merge(x=states, y=Statekey, by="STATEFP", all=TRUE)

##confine to Ohio
SingleState <- subset(states, states$STATENAME %in% c(
  "Ohio"
))

##fix caps
SingleState$NAME <- toupper(SingleState$NAME)

##merge with incidentsByCounty
SingleState<-sp::merge(x=SingleState, y=incidentByCounty, by.x="NAME", by.y="COUNTY_NAME", by=x)

##create the map object & set zoom
map<-leaflet(SingleState,options = leafletOptions(zoomControl = FALSE, zoomLevelFixed = TRUE, dragging=FALSE, minZoom = 7, maxZoom = 7) ) %>%
  #Worked: map<-leaflet(SingleState,options = leafletOptions(zoomControl = FALSE, scrollWheelZoom = FALSE, minZoom = 7, maxZoom = 7) ) %>%  
  
  addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              layerId = ~NAME,
              fillColor = ~colorQuantile("Reds",incidentRate1000 )(incidentRate1000),
              highlightOptions = highlightOptions(color = "white", weight = 2,
                                                  bringToFront = TRUE),
              popup = ~as.factor(paste0("<b><font size=\"4\"><center>County: </b>",SingleState$NAME,"</font></center>","<b>% Incident Count </b>",SingleState$incidentCount ,"<br/>")))

map<-map %>% setView(-82.1, 39.9,  zoom = 7)
  

##outputs
##export incidentByCounty CSV
write.csv(incidentByCounty, file = "Incident_by_County.csv", row.names = FALSE)
##export incidentDetail CSV
write.csv(incidentDetail, file = "Incident_Detail.csv", row.names = FALSE)
##print the map object
map

