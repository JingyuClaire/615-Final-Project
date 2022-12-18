library(tidyverse)
library(magrittr)
library(ggplot2)
library(stringr)
library(hrbrthemes)
library(viridis)
library(sf)
library(leaflet)
library(ggmap)

# Please read below before running this code:

# In step 1 and 2, I picked 11 weeks at random for each month from Nov 2021 to Sep 2022 (can't find 
# Oct 2022 and later data), and I call this data "selected_week". 
# Reminder: This selected_week is different from the "selected_week" I used to do the EDA, because the process is random. 

# In step 3, I picked 3 pairs of stops for each line of the T, and I 
# get the location data and put them together. 
# Reminder: You can skip this step by downloading "locate_new.csv"

# In step 4, I generate a shiny app using locate_new
# You can skip running step 1 to 3 by downloading locate_new.csv and starting Step 4
# Reminder: I tried to publish my Shiny app but the web page shows nothing, I put the
# link at the end.

#### Step 1: same process as EDA report: import the data, cleaning and exploration####
y21HRQ4 <- read.csv("HRTravelTimesQ4_21.csv")
y21LRQ4 <- read.csv("LRTravelTimesQ4_21.csv")
HRQ1 <- read.csv("2022-Q1_HRTravelTimes.csv")
LRQ1 <- read.csv("2022-Q1_LRTravelTimes.csv")
HRQ2 <- read.csv("2022-Q2_HRTravelTimes.csv")
LRQ2 <- read.csv("2022-Q2_LRTravelTimes.csv")
HRQ3 <- read.csv("2022-Q3_HRTravelTimes.csv")
LRQ3 <- read.csv("2022-Q3_LRTravelTimes.csv")

# service_date is Nov 2021 to Sep 2022
date4 = unique(y21LRQ4$service_date) # Oct to Dec
date1 = unique(HRQ1$service_date) # Jan to Mar
date2 = unique(LRQ2$service_date) # Apr to Jun
date3 = unique(LRQ3$service_date) # Jul to Sep

# HR and LR's route_id are different
HRroute = unique(HRQ3$route_id) # orange, blue, red
LRroute = unique(LRQ3$route_id) # Green-BCDE, Mattapan

# delete October 2021 data
delete = grep("-10-", y21HRQ4$service_date)
y21HRQ4 %<>% filter(!row_number() %in% delete) 

delete_lr = grep("-10-", y21LRQ4$service_date)
y21LRQ4 %<>% filter(!row_number() %in% delete_lr)


#### Step 2:pick a week randomly from each month (11 months)####

# pick two weeks from each of two months in 2021 (Nov to Dec)
selected_week = data.frame()
for (i in 11:12){
  # select the week in HR (for blue, red, orange)
  HR_row = grep(paste("-", i, "-", sep = ""), y21HRQ4$service_date)
  HR <- y21HRQ4 %>% slice(HR_row) # a month's data of HR
  LR_row = grep(paste("-", i, "-", sep = ""), y21LRQ4$service_date)
  LR <- y21LRQ4 %>% slice(LR_row) # a month's data in LR
  
  days = unique(HR$service_date)
  set.seed(20)
  start = sample(1:(length(days)-6),1)
  week = c(start:(start+6)) # finish selecting one week in this month
  
  # pick the data at the selected week day by day
  for (j in start:(start +6)){
    date_row  = grep(days[j], HR$service_date )
    date  <- HR %>% slice(date_row )
    selected_week = bind_rows(selected_week, date )
    
    date_row_LR = grep(days[j], LR$service_date )
    date_LR <- LR %>% slice(date_row_LR)
    selected_week = bind_rows(selected_week, date_LR)
    
  }
  
}

# pick 9 weeks randomly from each of 9 months in 2022 (Jan to Sep)
y2022 = bind_rows(HRQ1, LRQ1, HRQ2, LRQ2, HRQ3, LRQ3) # prepare the data of 2022
y2022_date = unique(y2022$service_date)

for (k in 1:9){
  # select the week in HR (for blue, red, orange)
  row = grep(paste("-0", k, "-", sep = ""), y2022$service_date)
  month <- y2022 %>% slice(row) # a month's data of HR & LR
  
  days22 = unique(month$service_date)
  set.seed(55)
  start22 = sample(1:(length(days22)-6),1)
  week22 = c(start22:(start22+6)) # finish selecting one week in this month
  
  # pick the data at the selected week day by day
  for (m in start22:(start22 +6)){
    date_row22  = grep(days22[m], month$service_date )
    date22  <- month %>% slice(date_row22 )
    selected_week = bind_rows(selected_week, date22 )
  }
  
}
##### Step 3: import the data of longitude and latitude of stops#####

# selected_week <- read.csv("selected_week.csv") 
locat_raw <- read.csv("MBTA_Systemwide_GTFS_Map_other.csv")
locat <- locat_raw %>% select(stop_code, stop_name, stop_lat, 
                              stop_lon, zone_id, municipality)

# pick 3 pairs of stops on each different line: orange, red, blue, green-BCDE, mattapan
routes = unique(selected_week$route_id)
selected_week$from_to_id <- str_c(selected_week$from_stop_id, sep = "-", selected_week$to_stop_id )

pairs <- data.frame()
for (i in 1:length(routes)) {
  selected_route <- selected_week %>% slice(grep(routes[i], selected_week$route_id))
  set.seed(40)
  tmp_pairs <- sample(unique(selected_route$from_to_id),3)
  tmp_df <- data.frame(pairs_of_stops = tmp_pairs, route_name = rep(routes[i],3))
  pairs <- bind_rows(pairs, tmp_df)
}

# finished with picking the pairs of stops
pairs %<>% separate(col= pairs_of_stops,
                    into = c("from_stop_id", "to_stop_id"),
                    sep = "-",
                    fill = "right")

pairs_new <- pairs %>% pivot_longer( cols = from_stop_id:to_stop_id,
                                names_to = "direction",
                                values_to = "stop_id")
stop_id <- pairs_new$stop_id

# we add the location data of stops to the stop pairs data
tmp_locate <- data.frame()
for (i in 1:dim(pairs_new)[1]) {
  find_locate <- locat %>% slice(grep(stop_id[i], locat$stop_code))
  tmp_locate <- bind_rows(tmp_locate, find_locate)
}
locate_new <- bind_cols(pairs_new,tmp_locate)

locate_new %<>% select(!all_of("stop_code")) 

##### Step 4: now we get the data to do the shiny app#####
# or you can download the data locate_new.csv and import it
# locate_new <- read.csv("locate_new.csv")
locate_new$pairs_id = rep(c(1:24),each =  2 )

locate_sf<- st_as_sf(locate_new, coords=c("stop_lon","stop_lat" ),crs=4326)

library(shiny)
library(shinythemes)

ui <- fluidPage(navbarPage("MBTA Rapid Transit Travel Times", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
                           tabPanel("Stops on the Map", leafletOutput("map"), textOutput("text"), height = 700),
                          ) 
                )

server <- function(input, output, session) {
  # Tab: Stops on the Map
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addMarkers(data = locate_new, ~stop_lon, ~stop_lat, label = ~paste(route_name, direction, stop_name, pairs_id, sep = ";"))})
  
  output$text <- renderText({
    "This map shows the stops I chose, with their name, route, and direction"
  })
}

shinyApp(ui, server)
