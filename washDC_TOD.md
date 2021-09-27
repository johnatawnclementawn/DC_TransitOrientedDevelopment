---
title: "Transit Oriented Development in Washington DC"
author: "Johnathan Clementi"
date: '9/24/2021'
output:
  html_document:
    keep_md: true
    code_folding: "hide"
---



# Introduction 

Dear policy maker,
If I only had five minutes of your time, here is an issue that I would like you to think about: Transit Oriented Development, specifically in Washington DC.

Transit Oriented Development, or TOD, is the focused development of mixed-use, pedestrian oriented communities around public transportation access points. TOD is an important urban planning tool in that it increases quality of life of constituents by increasing access to amenities while simultaneously increasing ridership, and therefore efficiency of transit systems. For TOD to be effective, it must meet two criteria: scale and land capitalization [1]. The scale at which transit systems run is important to their overall efficiency and effectiveness. Increasing population density drives the use of transit systems, but currently, most transit systems in the US operate at a loss on revenue per passenger ride [1].  Therefore, one goal of TOD is to increase population density in proximity to transit. With increasing population comes economic opportunities that serve people. Further, the second goal of TOD is that areas surrounding transit nodes will become more valuable as people compete for access to transit and other TOD amenities. With increases in land value comes the opportunity for governments to increase their tax base.

To give you an indication of your constituents’’ demand for TOD, we have put together a brief analysis comparing changes in population, income, rent prices, poverty, and crime across spatial (TOD vs non-TOD census tracts) and temporal (2000, 2010, and 2017 census data) axes. We have included all the code used to manipulate the data and create the figures for greater transparency and future replication of this analysis. We hope that this generates more questions than answers, and is used primarily to start the conversation regarding TOD in the District. 


# Retrieving and Wrangling Census, Transit, and Crime Data

#### Load Libraries, Styling Options, and Custom Functions 

```r
rm(list=ls()) ##Clear my objects from memory

library(tidyverse)
library(tidycensus)
library(sf)
library(tmap) # mapping, install if you don't have it
library(kableExtra)
library(rgdal)

options(scipen=999)
options(tigris_class = "sf")

palette5 <- c("#f0f9e8","#bae4bc","#7bccc4","#43a2ca","#0868ac") #colorwheel

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.text.x = element_text(size = 14))
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 16,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey80", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey80", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]],
                 c(.01,.2,.4,.6,.8), na.rm=T), digits = 3))
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}


# We will use the function provided by Dr. Ken Steif in his Public Policy Analytics book:
# Multi-ring Buffer
multipleRingBuffer <- function(inputPolygon, maxDistance, interval) 
{
  #create a list of distances that we'll iterate through to create each ring
  distances <- seq(0, maxDistance, interval)
  #we'll start with the second value in that list - the first is '0'
  distancesCounter <- 2
  #total number of rings we're going to create
  numberOfRings <- floor(maxDistance / interval)
  #a counter of number of rings
  numberOfRingsCounter <- 1
  #initialize an otuput data frame (that is not an sf)
  allRings <- data.frame()
  
  #while number of rings  counteris less than the specified nubmer of rings
  while (numberOfRingsCounter <= numberOfRings) 
  {
    #if we're interested in a negative buffer and this is the first buffer
    #(ie. not distance = '0' in the distances list)
    if(distances[distancesCounter] < 0 & distancesCounter == 2)
    {
      #buffer the input by the first distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #different that buffer from the input polygon to get the first ring
      buffer1_ <- st_difference(inputPolygon, buffer1)
      #cast this sf as a polygon geometry type
      thisRing <- st_cast(buffer1_, "POLYGON")
      #take the last column which is 'geometry'
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add a new field, 'distance' so we know how far the distance is for a give ring
      thisRing$distance <- distances[distancesCounter]
    }
    
    
    #otherwise, if this is the second or more ring (and a negative buffer)
    else if(distances[distancesCounter] < 0 & distancesCounter > 2) 
    {
      #buffer by a specific distance
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create the next smallest buffer
      buffer2 <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #This can then be used to difference out a buffer running from 660 to 1320
      #This works because differencing 1320ft by 660ft = a buffer between 660 & 1320.
      #bc the area after 660ft in buffer2 = NA.
      thisRing <- st_difference(buffer2,buffer1)
      #cast as apolygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #get the last field
      thisRing <- as.data.frame(thisRing$geometry)
      #create the distance field
      thisRing$distance <- distances[distancesCounter]
    }
    
    #Otherwise, if its a positive buffer
    else 
    {
      #Create a positive buffer
      buffer1 <- st_buffer(inputPolygon, distances[distancesCounter])
      #create a positive buffer that is one distance smaller. So if its the first buffer
      #distance, buffer1_ will = 0. 
      buffer1_ <- st_buffer(inputPolygon, distances[distancesCounter-1])
      #difference the two buffers
      thisRing <- st_difference(buffer1,buffer1_)
      #cast as a polygon
      thisRing <- st_cast(thisRing, "POLYGON")
      #geometry column as a data frame
      thisRing <- as.data.frame(thisRing[,ncol(thisRing)])
      #add teh distance
      thisRing$distance <- distances[distancesCounter]
    }  
    
    #rbind this ring to the rest of the rings
    allRings <- rbind(allRings, thisRing)
    #iterate the distance counter
    distancesCounter <- distancesCounter + 1
    #iterate the number of rings counter
    numberOfRingsCounter <- numberOfRingsCounter + 1
  }
  
  #convert the allRings data frame to an sf data frame
  allRings <- st_as_sf(allRings)
}

figCounter <- 1
```


#### Retrieve data from Census Bureau using tidycensus

```r
# Declare census variables of interest
census_var2000 <- c("P001001", "P006002", "PCT025009", "PCT025050", "P053001", "H056001", "P092001")
census_var2017 <- c("B25026_001E", "B02001_002E", "B15001_009E", "B15001_050E", "B19013_001E", "B25058_001E", "B06012_002E")
# total pop - p001001 or B25026_001E
# median age of males - p006002 (2000) or b02001_002e
# total male: 18-24 w/ bachelors degree - pct025009 or b15001_009e
# total female: 1824 w/ bachelors degree - pct025050 or b15001_050e
# median household income - p053001 or b19013_001e
# median contract rent - h056001 or b25058_001e
# total living in poverty - p092001 or b06012_002e

tractsDC.00 <- get_decennial(geography = "tract",
                                 year = 2000,
                                 variables = census_var2000,
                                 geometry = TRUE,
                                 state = 11999,
                                 output = "wide")


tractsDC.17 <- get_acs(geography = "tract", 
                           variables = census_var2017, 
                           year=2017, 
                           state=11999,
                           geometry=TRUE, 
                           output="wide")
```

#### Wrangle Census Data:

```r
cleanTracts00 <- 
  tractsDC.00 %>%
  dplyr::select( -NAME, -geometry) %>%
  #spread(variable, estimate) %>%
  #st_drop_geometry() %>% #Drop geometry is the new way of removing the sf geometry class from the dataframe
	st_transform('ESRI:102746') %>%
  rename(TotalPop = P001001, 
  			 Whites = P006002, 
  			 MaleBachelors = PCT025009,
         FemaleBachelors = PCT025050, 
  			 MedHHInc = P053001, 
  			 MedRent = H056001,
         TotalPoverty = P092001) %>%
	mutate(MedRentAdjusted = (MedRent * 1.423), # Adjust rent for inflation
				 MedHHIncAdjusted = (MedHHInc * 1.423), # Adjust income for inflation
				 pctWhite = ifelse(TotalPop > 0, Whites / TotalPop, 0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop), 0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2000") %>%
	mutate(pctPoverty.scaled = pctPoverty * 100,
			 pctBachelors.scaled = pctBachelors * 100,
			 pctWhite.scaled = pctWhite * 100) %>%
  dplyr::select(-Whites,-FemaleBachelors,-MaleBachelors,-TotalPoverty)


cleanTracts17 <- 
	tractsDC.17 %>%
	dplyr::select(-geometry, -B25026_001M, -B02001_002M, -B15001_009M, 
								-B15001_050M, -B19013_001M, -B25058_001M, -B06012_002M) %>% # Remove margin of error columns
	st_transform('ESRI:102746') %>%
  rename(TotalPop = B25026_001E, 
  			 Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
  			 MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
  			 MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  #dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(MedRentAdjusted = (MedRent * 1),
  			 MedHHIncAdjusted = (MedHHInc * 1),
  			 pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2017") %>%
	mutate(pctPoverty.scaled = pctPoverty * 100,
				 pctBachelors.scaled = pctBachelors * 100,
				 pctWhite.scaled = pctWhite * 100) %>%
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty, -NAME)



allTracts <- rbind(cleanTracts00,cleanTracts17)
```

#### Retrieve & Clean Washington Metro Data

```r
# Although the Metro extends into multiple counties and cities in Virginia and Maryland, we will only examine stations in DC
metroStops <- st_read("https://opendata.arcgis.com/datasets/54018b7f06b943f2af278bbe415df1de_52.geojson") %>%
	st_transform('ESRI:102746')
metroLines <- st_read("https://opendata.arcgis.com/datasets/a29b9dbb2f00459db2b0c3c56faca297_106.geojson") %>%
	st_transform('ESRI:102746')

# We need to clean up the stops data a bit
# There are some stops in which multiple lines visit the stop. For our case, we will take the first stop listed
metroStops <- 
	metroStops %>%
	mutate(LINE = 
				 	plyr::revalue(LINE, 
						replace = c(
			 				"green, yellow" = "green", 
			 				"grn, yllw, orange, blue, slvr" = "green", 
				 			"red, green, yellow" = "red", 
			 				"red, blue, orange, silver" = "red",
				 			"blue, orange, silver" = "blue",
				 			"orange, blue, silver" = "orange"
					 	)
				 	)
				) 
```

#### Load and clean crime data & additional census data

```r
# Retrieve 2010 census data
# Note: can use the census_vars2017 for the 2010 ACS - they are the same field identiers
tractsDC.10 <- get_acs(geography = "tract", 
                           variables = census_var2017, 
                           year=2010, 
                           state=11999,
                           geometry=TRUE, 
                           output="wide")

# Clean 2010 ACS data
cleanTracts10 <- 
	tractsDC.10 %>%
	dplyr::select(-geometry, -B25026_001M, -B02001_002M, -B15001_009M, 
								-B15001_050M, -B19013_001M, -B25058_001M, -B06012_002M) %>% # Remove margin of error columns
	st_transform('ESRI:102746') %>%
  rename(TotalPop = B25026_001E, 
  			 Whites = B02001_002E,
         FemaleBachelors = B15001_050E, 
  			 MaleBachelors = B15001_009E,
         MedHHInc = B19013_001E, 
  			 MedRent = B25058_001E,
         TotalPoverty = B06012_002E) %>%
  #dplyr::select(-NAME, -starts_with("B")) %>%
  mutate(MedRentAdjusted = (MedRent * 1.124), #adjust for inflation to 2017
  			 MedHHIncAdjusted = (MedHHInc * 1.124), #adjust for inflation to 2017
  			 pctWhite = ifelse(TotalPop > 0, Whites / TotalPop,0),
         pctBachelors = ifelse(TotalPop > 0, ((FemaleBachelors + MaleBachelors) / TotalPop),0),
         pctPoverty = ifelse(TotalPop > 0, TotalPoverty / TotalPop, 0),
         year = "2010") %>%
	mutate(pctPoverty.scaled = pctPoverty * 100,
				 pctBachelors.scaled = pctBachelors * 100,
				 pctWhite.scaled = pctWhite * 100,
				 MedRentAdjusted = MedRent * 1.124) %>% # Adjust for inflation from 2010 to 2017 
  dplyr::select(-Whites, -FemaleBachelors, -MaleBachelors, -TotalPoverty, -NAME)


allTracts10_17 <- rbind(cleanTracts10,cleanTracts17)


# 2010 crime data (earliest available, is 2008, but use 2010 for decenial census)
crime10 <- st_read("https://opendata.arcgis.com/datasets/fdacfbdda7654e06a161352247d3a2f0_34.geojson") %>%
	st_transform('ESRI:102746') %>%
	filter(grepl('THEFT',OFFENSE)) %>%
	mutate(crimeYear = "2010", OFFENSE = "THEFT", crimeCounter = 1) %>%
	dplyr::select(OFFENSE,crimeYear,crimeCounter)

# 2017 crime data (consistent with upper end of census data we are analyzing)
crime17 <- st_read("https://opendata.arcgis.com/datasets/6af5cb8dc38e4bcbac8168b27ee104aa_38.geojson") %>%
	st_transform('ESRI:102746') %>%
	filter(grepl('THEFT',OFFENSE)) %>%
	mutate(crimeYear = "2017", OFFENSE = "THEFT",crimeCounter = 1) %>%
	dplyr::select(OFFENSE,crimeYear,crimeCounter) 

# Place all crime data in same df
allCrime <- rbind(crime10, crime17)
```


# Map Metro Stops & Lines

Here is a map of the metro stops we are interested in, colored by line that they serve. For generalization purposes, the stations that serve multiple lines have been aggregated. This aggregation does not play a role in our analysis.

```r
# Map of Metro Stops
ggplot() + 
  geom_sf(data=st_union(cleanTracts00)) +
  geom_sf(data=metroStops, 
          aes(colour = LINE), 
          show.legend = "point", size= 2) +
  scale_colour_manual(values = c("blue", "green", "orange", "red"), name = "Metro Line") +
  labs(title="Metro Stops", 
       subtitle="Washington DC", 
       caption=paste0("Figure ", figCounter,": Washington DC Metro Station locations, colored by Metro Line\nData from US Census Bureau & Open Data DC")) +
  mapTheme()
```

![](washDC_TOD_files/figure-html/metroViz-1.png)<!-- -->




# Analysis

#### Identifying TOD and Non-TOD Census Tracts:


```r
# First, lets define regions around each stop
# Create 0.5 mile (2640 foot) buffer around each transit station
# 0.5 mile is generally how far people are willing to walk to transportation
stopBuffers <- 
  st_buffer(metroStops, 2640) %>%
    mutate(Legend = "Buffer")
      

# Sometimes those regions overlap, we need to get rid of those overlaps
# Create an sf object with ONLY the unioned buffer
buffer <- 
  st_union(stopBuffers) %>%
    st_sf() %>%
    mutate(Legend = "Unioned Buffer")

# Lets indentify the census tracts that overlap with the station buffered regions
# We will evaluate if the census tract centroid is within the station buffer region

### THIS IS AN INTERMEDIATE STEP AND MAY NOT NEED ITS OWN VARIABLE ###
selectCentroids <-
  st_centroid(cleanTracts00)[buffer,] %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(cleanTracts00, GEOID)) %>%
  st_sf() %>%
  dplyr::select(TotalPop) %>%
  mutate(Selection_Type = "Select by Centroids")


# We want to be able to identify the tracts by whether they are TOD or Non-TOD
# Here we evaluate whether the census tract centroid is within the TOD buffer region
# and appropriately assign it a "TOD" or "Non-TOD" string
allTracts.group <- 
  rbind(
    st_centroid(allTracts)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTracts)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTracts) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD"))

# For visualization purposes down the line, we will create an sf object
# that has the outline of all TOD census tracts
#Create Outline of TOD Tracts for use in mapping
TOD_region <-
  allTracts.group%>%
    dplyr::select(TOD)%>%
  filter(TOD=="TOD") %>%
  st_union()%>%
      st_sf()
```


# Visualizizing Trends in Census Data:

#### TOD vs Non-TOD tracts across time

Here we have identified the census tracts in which one or more Metro stops are located. These tracts will be identified as tracts that are possible locations for Transit Oriented Development and labeled TOD. Tracts that do not have a Metro stop are labeled as non-TOD. The difference in TOD and non-TOD will serve as the backbone of our analysis.

```r
figCounter = figCounter + 1

# Lets visualize and compare the TOD/Non-TOD tracts across time
ggplot(allTracts.group)+
  geom_sf(data = st_union(cleanTracts17))+
  geom_sf(aes(fill = TOD)) +
  scale_fill_manual(values = c("grey80","orange")) +
  labs(title = "TOD and Non-TOD Census Tracts",
       subtitle="Washington DC",
       caption=paste0("Figure ", figCounter,": TOD and Non-TOD census tracts in 2000 and 2017. TOD census tracts are those where the 
                centroid is located within a half-mile of a Metro station\nData from US Census Bureau & Open Data DC")) +
  facet_wrap(~year)+
  mapTheme() 
```

![](washDC_TOD_files/figure-html/viz1TOD-1.png)<!-- -->

#### Trends in population across space and time

This figure illustrates the total population by tract reported for the 2000 Census and estimated for the 2017 American Community Survey (ACS). Our data for the first part of the analysis come from these two sources. In this figure, notice how there are not major swings in population across the District. While some tracts may increase or decrease, in aggregate they generally remain the same. Our TOD tracts have been highlighted in a red outline. 

```r
figCounter = figCounter + 1

# Lets visualize and compare total population within the TOD/Non-TOD tracts across time
ggplot(allTracts.group)+
  geom_sf(data = st_union(cleanTracts17))+
	geom_sf(aes(fill = q5(TotalPop))) +
	geom_sf(data = TOD_region, fill="transparent", color ="red", size=0.8)+
  scale_fill_manual(values =  palette5,
  									labels = qBr(allTracts.group, "TotalPop"), 
                    name = "Popluation\n(Quintile Breaks)") +
  labs(title = "Total Population 2000-2017",
       subtitle="Washington DC",
       caption=paste0("Figure ",figCounter, ": Total population by census tract in 2000 and 2017\nData from US Census Bureau & Open Data DC")) +
  facet_wrap(~year)+
  mapTheme() 
```

![](washDC_TOD_files/figure-html/vizPopTOD-1.png)<!-- -->

#### Trends in median household income across space and time

Here we illustrate the changes in median household income by tract. These figures have been adjusted for inflation but notice that across most TOD tracts the median household income has increased dramatically. Most TOD tracts, especially in the Downtown, Dupont Circle, and Mount Vernon Square neighborhoods, have increased.

```r
figCounter = figCounter + 1

ggplot(allTracts.group)+
  geom_sf(data = st_union(cleanTracts17))+
	geom_sf(aes(fill = q5(MedHHIncAdjusted))) +
	geom_sf(data = TOD_region, fill="transparent", color ="red", size=0.8)+
  scale_fill_manual(values =  palette5,
  									labels = qBr(allTracts.group, "MedHHIncAdjusted"), 
                    name = "Median Household Income\n(Quintile Breaks)") +
  labs(title = "Median Household Income 2000-2017",
       subtitle="Washington DC",
       caption=paste0("Figure ",figCounter, ": Median Household Income ($ adjusted for inflation) by census tract in 2000 and 2017\nData from US Census Bureau & Open Data DC")) +
  facet_wrap(~year)+
  mapTheme()
```

![](washDC_TOD_files/figure-html/vizIncomeTOD-1.png)<!-- -->

#### Trends in median rent prices across space and time

While income has increased in select parts of the District, rent has increased across all tracts. At first glance, the rent market does seem to value TOD tracts more than non-TOD markets. We will confirm this later.

```r
figCounter = figCounter + 1

### ADJUSTED FOR INFLATION!!!
ggplot(allTracts.group)+
  geom_sf(data = st_union(cleanTracts17))+
	geom_sf(aes(fill = q5(MedRentAdjusted))) +
	geom_sf(data = TOD_region, fill="transparent", color ="red", size=0.8)+
  scale_fill_manual(values =  palette5,
  									labels = qBr(allTracts.group, "MedRentAdjusted"), 
                    name = "Median Rent Price($)\n(Quintile Breaks)") +
  labs(title = "Median Rent Price ($) 2000-2017",
       subtitle="Washington DC",
       caption=paste0("Figure ", figCounter, ": Median Rent Price ($ adjusted for inflation) by census tract in 2000 and 2017\nData from US Census Bureau & Open Data DC")) +
  facet_wrap(~year)+
  mapTheme()
```

![](washDC_TOD_files/figure-html/vizRentTOD2-1.png)<!-- -->


#### Trends in poverty across space and time

This figure seems to indicate a drastic decrease in the percent of people in poverty in the District. There are several factors that could be driving this. One could be that policy measures put in place have increased District constituents’ well-being and ability to rise out of poverty. Another explanation is that wealthier people have displaced those in poverty, indicating the process of gentrification throughout the District. 

```r
figCounter = figCounter + 1

ggplot(allTracts.group)+
  geom_sf(data = st_union(cleanTracts17))+
	geom_sf(aes(fill = q5(pctPoverty.scaled))) +
	geom_sf(data = TOD_region, fill="transparent", color ="red", size=0.8)+
  scale_fill_manual(values =  palette5,
  									labels = qBr(allTracts.group, "pctPoverty.scaled"), 
                    name = "Percent People\nin Poverty\n(Quintile Breaks)") +
  labs(title = "Percent People in Poverty 2000-2017",
       subtitle="Washington DC",
       caption=paste0("Figure ", figCounter, ": Percent of population in poverty by census tract in 2000 and 2017\nData from US Census Bureau & Open Data DC")) +
  facet_wrap(~year)+
  mapTheme()
```

![](washDC_TOD_files/figure-html/vizPovTOD-1.png)<!-- -->

#### Tabulating change in indicators across time and TOD / Non-TOD tracts


```r
allTracts.Summary <- 
  st_drop_geometry(allTracts.group) %>%
    group_by(year, TOD) %>%
    summarize(Rent = mean(MedRent, na.rm = T),
              Population = mean(TotalPop, na.rm = T),
              Percent_White = mean(pctWhite, na.rm = T),
              Percent_Bach = mean(pctBachelors, na.rm = T),
              Percent_Poverty = mean(pctPoverty, na.rm = T)
    					) 

allTracts.SummaryTable <-
	allTracts.Summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value)

table1 <- 
	kable(allTracts.SummaryTable) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 1")

table1
```

<table class="table" style="margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:right;"> 2000: Non-TOD </th>
   <th style="text-align:right;"> 2000: TOD </th>
   <th style="text-align:right;"> 2017: Non-TOD </th>
   <th style="text-align:right;"> 2017: TOD </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Percent_Bach </td>
   <td style="text-align:right;"> 0.02 </td>
   <td style="text-align:right;"> 0.03 </td>
   <td style="text-align:right;"> 0.02 </td>
   <td style="text-align:right;"> 0.03 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Percent_Poverty </td>
   <td style="text-align:right;"> 0.40 </td>
   <td style="text-align:right;"> 0.45 </td>
   <td style="text-align:right;"> 0.19 </td>
   <td style="text-align:right;"> 0.17 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Percent_White </td>
   <td style="text-align:right;"> 0.24 </td>
   <td style="text-align:right;"> 0.31 </td>
   <td style="text-align:right;"> 0.34 </td>
   <td style="text-align:right;"> 0.57 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Population </td>
   <td style="text-align:right;"> 3106.44 </td>
   <td style="text-align:right;"> 2957.05 </td>
   <td style="text-align:right;"> 3466.05 </td>
   <td style="text-align:right;"> 3633.08 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Rent </td>
   <td style="text-align:right;"> 563.78 </td>
   <td style="text-align:right;"> 564.74 </td>
   <td style="text-align:right;"> 1285.75 </td>
   <td style="text-align:right;"> 1537.99 </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; " colspan="100%"><span style="font-style: italic;"><br></span></td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> Table 1</td></tr>
</tfoot>
</table>

#### Visualizing change in indicators across time and TOD / Non-TOD tracts

The primary take away from this plot is that Washington DC has likely gone through a process of prioritization of transit enabled neighborhoods. However, gentrification has followed this market prioritization of TOD. The population has become overwhelmingly white and wealthy, while generally not increasing in density. Additionally, rent prices have increased dramatically, and the population is moving towards being more educated than before.

```r
plotCounter <- 1

allTracts.SummaryPlot <-
	allTracts.Summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Variable, scales = "free", ncol=5) +
    scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
    labs(title = "Indicator differences across time and space",
    		 subtitle="Washington DC",
    		 caption=paste0("Plot ", plotCounter, ". Data from US Census Bureau")) +
    plotTheme() + theme(legend.position="bottom")

allTracts.SummaryPlot
```

![](washDC_TOD_files/figure-html/plotIndicators-1.png)<!-- -->

```r
plotCounter = plotCounter + 1
```

#### Visualizing change in population within 0.5 miles of a Metro stop

Here are two additional visualizations of the change in population and rents, but this time focusing on only populations and rent within half-mile distance of a Metro stop. These figures are useful for confirming our hypotheses above – that population has stayed relatively stagnant, while rents have increased dramatically. They are possibly problematic for granular analysis of how individual stops have changed over time in that this analysis may overcount both rent and population. Because we do not have individual point data for population and rent, we use the aggregated data provided by the census. Therefore, we have visualized each stop by the average rent or total population that is associated with the tracts that are within half-a-mile of the stop, not just the disaggregated rent or population figures that would be associated with only the region that falls within 0.5 miles of the stop.

```r
# Lets find the intersecting regions of the tracts and our previously created 0.5 mile regions around the metro stops
station_tracts <- st_intersection(allTracts, stopBuffers)

# Lets summarize those data so we can join it back to the station points and visualize it
station_summary <-
	station_tracts %>%
	st_drop_geometry() %>%
	group_by(year, GIS_ID) %>%
	summarize(Population = sum(TotalPop),
						Rent = mean(MedRent, na.rm = T)
						) %>%
	filter()

# Complete the join of the summarized data to the station points
station_GeoSummary <-
	left_join(metroStops, station_summary, by = "GIS_ID") %>%
	dplyr::select(-WEB_URL, -TRAININFO_URL)

# Now that we have the data ready, lets visualize it

figCounter = figCounter + 1

# Graduated Symbols map visualizing population
ggplot(station_GeoSummary) +
	geom_sf(data = allTracts)+
	geom_sf(aes(size = Population), color = "#7bccc4") +
  labs(title = "Population with access to Metro stops 2000-2017",
       subtitle="Washington DC",
       caption=paste0("Figure ", figCounter, ": total population within 0.5 mile walking distance of a metro stop in 2000 and 2017\nData from US Census Bureau & Open Data DC")) +
	facet_wrap(~year) +
	mapTheme()
```

![](washDC_TOD_files/figure-html/vizPopStops-1.png)<!-- -->

```r
figCounter = figCounter + 1

# Graduated Symbols map visualizing rent
ggplot(station_GeoSummary) +
	geom_sf(data = allTracts)+
	geom_sf(aes(size = Rent), color = "#43a2ca") +
	labs(title = "Rent prices near Metro stops 2000-2017",
       subtitle="Washington DC",
       caption=paste0("Figure ", figCounter, ": Average rent price ($) for homes within 0.5 mile walking distance of a metro stop in 2000 and 2017\nData from US Census Bureau & Open Data DC")) +
	facet_wrap(~year) +
	mapTheme()
```

![](washDC_TOD_files/figure-html/vizPopStops-2.png)<!-- -->

#### Visualizing change in rent as a function of distance from metro stations

To get an idea of whether people are prioritizing access to transit, we analyze rent prices as a function of distance from a metro stop. A driver of this relationship is the willingness of people to walk to access transit. Generally, we see that rent for homes within 2 miles of a transit node are prioritized at the same level. However, homes beyond two miles seem to be prioritized more. Perhaps there is something more to this relationship – maybe more space is available as you get further from a metro stop. 

```r
figCounter = figCounter + 1

coolRingBuffer <- multipleRingBuffer(st_union(metroStops), 37830, 2640)

ggplot(coolRingBuffer) + 
  geom_sf(data=coolRingBuffer, 
          aes(colour = distance), 
          show.legend = "polygon", size= 2) +
  scale_colour_gradient() +
	geom_sf(data=st_union(cleanTracts00), fill="transparent", color="red",size =1) +
  labs(title="Metro Stops", 
       subtitle="District of Columbia", 
       caption=paste0("Figure ", figCounter, ": 2640ft (0.5 mile) rings around DC metro stops\nData from US Census Bureau & Open Data DC")) +
  mapTheme()
```

![](washDC_TOD_files/figure-html/vizMRB-1.png)<!-- -->
	
	

```r
# Use multipleRingBuffer to draw regions around the metro stops
# Then join the census data (containing rent data) to those rings
allTracts.rings <-
  st_join(st_centroid(dplyr::select(allTracts, GEOID, year)), 
          multipleRingBuffer(st_union(metroStops), 37830, 2640)) %>%
  st_drop_geometry() %>%
  left_join(dplyr::select(allTracts, GEOID, MedRentAdjusted, year), 
            by=c("GEOID"="GEOID", "year"="year")) %>%
  st_sf() %>%
  mutate(distance = distance / 5280) #convert to miles

# Lets clean up the data so we can visualize it as a line graph
# We need to get rid of the geometry, group by year and distance
# and then summarize the rents found at each level (calculate avg)
allTracts.rings.summary <-
	allTracts.rings %>%
	st_drop_geometry() %>%
	group_by(distance, year) %>%
	summarize(Rent = mean(MedRentAdjusted, na.rm = TRUE)) %>%
	mutate(Rent = round(Rent, 2))

# Now let's visualize it
ggplot(allTracts.rings.summary,
			 aes(x =distance, y=Rent, color=year)) +
	geom_line() +
	geom_point() +
	labs(title = "Rent price as a function of distance from Metro stop",
    		 subtitle="Washington DC",
    		 caption=paste0("Plot ", plotCounter, ". Rent price ($) for homes at a certain distance (miles) from Metro stops\nRent price adjusted for inflation\nData from US Census Bureau")) +
    plotTheme() + theme(legend.position="bottom")
```

![](washDC_TOD_files/figure-html/plotRentDistStop-1.png)<!-- -->

```r
plotCounter = plotCounter + 1
```

# Trends in Crime, Transit, and Rent

#### Associate crime data with TOD tracts

A note: we have switched to using 2010 and 2017 data for the below analysis. This is to stay consistent with the crime data that are available from Open Data DC. 

```r
# Intersect crimes with the station buffer/census tracts from 2010 & 2017 (instead of station_tracts which is 2000 & 2017)
station_tracts10_17 <- st_intersection(allTracts10_17, stopBuffers)


crime_station_tracts <- st_join(station_tracts10_17, allCrime) %>%
	filter(year == crimeYear) %>%
	dplyr::select(GEOID, year, MedRentAdjusted, GIS_ID, crimeYear, OFFENSE, crimeCounter)

# Lets summarize those data so we can join it back to the station points and visualize it
rent_crime_byStation <-
	crime_station_tracts %>%
	st_drop_geometry() %>%
	group_by(year, GIS_ID) %>%
	summarize(Rent = mean(MedRentAdjusted, na.rm = T),
						Thefts = sum(crimeCounter)
						) %>%
	filter()

# Complete the join of the summarized data to the station points
rentCrime_GeoByStation <-
	left_join(metroStops, rent_crime_byStation, by = "GIS_ID") %>%
	dplyr::select(-WEB_URL, -TRAININFO_URL)
```

#### Visualize changes in rent and crime rates within 0.5 miles of a Metro stop

Just as with the population and rent figures above, be caution of the rent values in this figure as they are aggregated based on census tract, not just within 0.5 miles from a Metro stop. However, the crime data are not subject to these caveats as they are point data aggregated by regions within 0.5 miles of a Metro stop. 

```r
# Now that we have the data ready, lets visualize it
figCounter = figCounter + 1

# Graduated Symbols map visualizing rent
ggplot(rentCrime_GeoByStation) +
	geom_sf(data = allTracts10_17) +
	geom_sf(aes(size = Rent), color = "#43a2ca") +
	labs(title = "Rent prices near Metro stops 2010-2017",
       subtitle="Washington DC",
       caption=paste0("Figure ", figCounter, ": Average rent price ($) for homes within 0.5 mile walking distance of a metro stop in 2010 and 2017\nRent price adjusted for inflation\nData from US Census Bureau & Open Data DC")) +
	facet_wrap(~year) +
	mapTheme()
```

![](washDC_TOD_files/figure-html/vizCrime-1.png)<!-- -->

```r
figCounter = figCounter + 1

# Graduated Symbols map visualizing crime 
ggplot(rentCrime_GeoByStation) +
	geom_sf(data = allTracts10_17)+
	geom_sf(aes(size = Thefts), color = "#0868ac") +
	labs(title = "Crime rates near Metro stops 2010-2017",
       subtitle="Washington DC",
       caption=paste0("Figure ", figCounter, ": Number of crimes within 0.5 mile walking distance of a metro stop in 2010 and 2017\nData from US Census Bureau & Open Data DC")) +
	facet_wrap(~year) +
	mapTheme()
```

![](washDC_TOD_files/figure-html/vizCrime-2.png)<!-- -->

#### Tabulate differenes in crime & rend across TOD / Non-TOD tracts

```r
# Identify 2010 & 2017 tracts by TOD & Non-TOD
allTracts10_17.group <- 
  rbind(
    st_centroid(allTracts10_17)[buffer,] %>%
      st_drop_geometry() %>%
      left_join(allTracts10_17) %>%
      st_sf() %>%
      mutate(TOD = "TOD"),
    st_centroid(allTracts10_17)[buffer, op = st_disjoint] %>%
      st_drop_geometry() %>%
      left_join(allTracts10_17) %>%
      st_sf() %>%
      mutate(TOD = "Non-TOD"))

# For comparison of rent and crime within TOD & Non-TOD, join crime data to allTracts df for 2010 & 2017
crime_tracts <- st_join(allTracts10_17.group, allCrime) %>%
	filter(year == crimeYear) %>%
	dplyr::select(GEOID, year, MedRentAdjusted, TOD, crimeYear, OFFENSE, crimeCounter)

# Summarize tracts by total crimes and avg rent per year
crime_tracts.summary <-
	st_drop_geometry(crime_tracts) %>%
	group_by(year, TOD) %>%
	summarize(Rent = mean(MedRentAdjusted, na.rm = T),
						Thefts = sum(crimeCounter, na.rm = T)
						)

# Clean up summary 
crimeTracts.SummaryTable <-
	crime_tracts.summary %>%
  unite(year.TOD, year, TOD, sep = ": ", remove = T) %>%
  gather(Variable, Value, -year.TOD) %>%
  mutate(Value = round(Value, 2)) %>%
  spread(year.TOD, Value)

# Print nice kable table
table2 <- 
	kable(crimeTracts.SummaryTable) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 2")

table2
```

<table class="table" style="margin-left: auto; margin-right: auto;border-bottom: 0;">
 <thead>
  <tr>
   <th style="text-align:left;"> Variable </th>
   <th style="text-align:right;"> 2010: Non-TOD </th>
   <th style="text-align:right;"> 2010: TOD </th>
   <th style="text-align:right;"> 2017: Non-TOD </th>
   <th style="text-align:right;"> 2017: TOD </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Rent </td>
   <td style="text-align:right;"> 1060.86 </td>
   <td style="text-align:right;"> 1365.29 </td>
   <td style="text-align:right;"> 1271.45 </td>
   <td style="text-align:right;"> 1662.79 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Thefts </td>
   <td style="text-align:right;"> 10027.00 </td>
   <td style="text-align:right;"> 10753.00 </td>
   <td style="text-align:right;"> 12541.00 </td>
   <td style="text-align:right;"> 14512.00 </td>
  </tr>
</tbody>
<tfoot>
<tr><td style="padding: 0; " colspan="100%"><span style="font-style: italic;"><br></span></td></tr>
<tr><td style="padding: 0; " colspan="100%">
<sup></sup> Table 2</td></tr>
</tfoot>
</table>


#### Visualizing change in rent and crime indicators across time and TOD / Non-TOD tracts

These figures seem to indicate that there was a general increase in theft across both TOD and non-TOD tracts, but crimes in TOD tracts increased at a greater rate. It would be interesting to compare this to crime data collected by the Metro. Are these crimes occurring on transit services, or within the communities that surround them?

```r
crimeTracts.SummaryPlot <-
	crime_tracts.summary %>%
  gather(Variable, Value, -year, -TOD) %>%
  ggplot(aes(year, Value, fill = TOD)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Variable, scales = "free", ncol=5) +
    scale_fill_manual(values = c("#bae4bc", "#0868ac")) +
    labs(title = "Crime rates and rent prices in TOD vs non-TOD tracts",
    		 subtitle="Washington DC",
       caption=paste0("Plot ", plotCounter, ". Data from US Census Bureau & Open Data DC")) +
    plotTheme() + theme(legend.position="bottom")

crimeTracts.SummaryPlot
```

![](washDC_TOD_files/figure-html/plotCrime-1.png)<!-- -->

# Conclusions
Overall, the results from our analysis of Washington DC TOD suggests that the residents of the District may value transit accessible neighborhoods over other neighborhoods. It is important, though, to consider the spatial biases of this analysis and the possible ramifications of promoting TOD, namely gentrification. 


#### Citations
1. [Public Policy Analytics, Steif K., 2021, accessed 2021-09-23](https://urbanspatial.github.io/PublicPolicyAnalytics/)
