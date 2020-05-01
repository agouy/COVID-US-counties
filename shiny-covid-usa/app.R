#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)





# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    


    output$distPlot <- renderPlot({
        
        library(rgdal)
        library(dplyr)
        library(leaflet)
        library(scales)
        library(dygraphs)
        library(crosstalk)
        library(plotly)
        
        ## Local paths or URLs to files
        
        # web-scraped data
        # Census data
        CENSUS.input <- "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"
        
        # NYT data
        NYT.url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"
        
        # Counties and states boundaries
        # downloaded from https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
        # and put in a 'shp' subdirectory
        MAP.dir <- "./shp"
        MAP.prefix <- "cb_2018_us_county_20m"
        STATE.prefix <- "cb_2018_us_state_20m"
        
        ## Graphical parameters
        line.wid <- 1.5
        dot.size <- 2.5
        
        ### Get population data
        census <- read.csv(CENSUS.input, sep = ",", header = TRUE)
        
        # Prepare county IDs (concatenate state and county IDs)
        census$GEOID <- paste0(
            formatC(census$STATE, width = 2, format = "d", flag = "0"),
            formatC(census$COUNTY, width = 3, format = "d", flag = "0")
        )
        
        # we keep only the county level from this dataset (encoded as SUMLEV = 50)
        census.st <- census[census$SUMLEV == 40, ]
        census <- census[census$SUMLEV == 50, ]
        
        # we just keep the GEOID (= FIPS) and latest census estimate (2019)
        census.small <- data.frame(
            GEOID = census$GEOID,
            pop = census$POPESTIMATE2019
        )
        rownames(census.small) <- census.small$GEOID
        census.small.st <- data.frame(
            GEOID = census.st$GEOID,
            pop = census.st$POPESTIMATE2019
        )
        rownames(census.small) <- census.small$GEOID
        nyt.data <- read.csv(
            NYT.url,
            stringsAsFactors = FALSE
        )
        
        # NAs are present in the dataset (no FIPS, i.e. not a county)
        # they correspond to special cases: the state level, NYC, islands
        # let's filter them out
        nyt.data.st <- nyt.data[nyt.data$county == "Unknown", ]
        
        # nyt.data <- nyt.data[!is.na(nyt.data$fips), ]
        
        # nyc.data <- nyt.data[nyt.data$county == "", ]
        
        ## Format county ID
        nyt.data$GEOID <- formatC(nyt.data$fips, width = 5, format = "d", flag = "0")
        
        nyt.data$ID <- paste0(nyt.data$date, nyt.data$fips)
        nyt.data$time <- as.numeric(as.Date(nyt.data$date))
        nyt.data$time <- nyt.data$time - min(nyt.data$time)
        
        nyt.data.st$time <- as.numeric(as.Date(nyt.data.st$date))
        nyt.data.st$time <- nyt.data.st$time - min(nyt.data.st$time)
        
        nyt.data$pop <- census.small[nyt.data$GEOID, ]$pop
        nyt.data$cases100k <- 100000 * nyt.data$cases / nyt.data$pop
        nyt.data$deaths100k <- 100000 * nyt.data$deaths / nyt.data$pop
        
        us.map <- readOGR(dsn = MAP.dir, layer = MAP.prefix, stringsAsFactors = FALSE)
        
        ## IMPORTANT preprocessing step
        
        # Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), 
        # Virgin Islands (78), American Samoa (60), Mariana Islands (69), 
        # Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
        us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                                "64", "68", "70", "74"),]
        
        # Make sure other islands are removed
        us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                                "95", "79"),]
        
        # us.map <- rmapshaper::ms_simplify(us.map, 0.05)
        
        ##We do the sq,e for the state layer
        us.st <- readOGR(dsn = MAP.dir, layer = STATE.prefix, stringsAsFactors = FALSE)
        us.st <- us.st[!us.st$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                             "64", "68", "70", "74"),]
        us.st <- us.st[!us.st$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                             "95", "79"),]
        
        # We first create a data frame containing data from the last day
        last.day <- (nyt.data[nyt.data$date == max(as.Date(nyt.data$date)), ])
        last.day <- last.day[order(last.day$cases, decreasing = TRUE), ]
        
        sum.per.st <- tapply(last.day$cases, last.day$state, sum)
        last.day.st <- data.frame(
            NAME = names(sum.per.st),
            cases = sum.per.st
        )
        
        # Merge spatial df with downloade ddata.
        leaf.nyt <- merge(us.map, last.day, by=c("GEOID"))
        leaf.nyt.st <- merge(us.st, last.day.st, by=c("NAME"))
        
        ### IMPORTANT: in the NYT dataset, the 5 counties of NYC are merged
        fips.nyc <- c(
            Bronx = "36005", 
            Queens = "36081",
            Kings = "36047",
            NewYork = "36061",
            Richmond =  "36085"
        )
        
        pop.nyc <- sum(census.small[census.small$GEOID %in% fips.nyc, ]$pop)
        nyc <- last.day[last.day$county == "New York City", ]
        leaf.nyt@data[leaf.nyt@data$GEOID %in% fips.nyc, ]$cases <- nyc$cases
        leaf.nyt@data[leaf.nyt@data$GEOID %in% fips.nyc, ]$cases100k <- 1e5 * nyc$cases / pop.nyc
        
        # Format popup data for leaflet map.
        popup_dat <- paste0("<strong>County: </strong>", 
                            leaf.nyt$NAME, 
                            "<br><strong>Cases: </strong>", 
                            round(leaf.nyt$cases),
                            "<br><strong>Cases per 100k: </strong>", 
                            round(leaf.nyt$cases100k),
                            "<br><strong>Deaths: </strong>", 
                            round(leaf.nyt$deaths),
                            "<br><strong>Deaths per 100k: </strong>", 
                            round(leaf.nyt$deaths100k))
        
        popup_dat_st <- paste0("<strong>State: </strong>", 
                               leaf.nyt.st$NAME, 
                               "<br><strong>Cases: </strong>", 
                               round(leaf.nyt.st$cases))
        
        # generate bins based on input$bins from ui.R
        pal <- colorNumeric("YlOrRd", log(leaf.nyt$cases+1), na.color = "#fff8e8")
        pal.st <- colorNumeric("YlOrRd", log(leaf.nyt.st$cases+1), na.color = "#fff8e8")
        
        
        # test <- leaf.nyt[leaf.nyt$NAME == "Bronx", ]
        # nyc <- leaf.nyt[leaf.nyt$NAME == "New York City", ]
        # nyt.data[nyt.data$county == "New York City", ]
        # test$cases
        ### Render map in leaflet
        map <- leaflet() %>% addTiles() %>%
            addPolygons(data = leaf.nyt.st,
                        fillColor = "white", 
                        fillOpacity = 0., 
                        color = "#BDBDC3", 
                        weight = 1, group = "County: total cases") %>%
            
            addPolygons(data = leaf.nyt,
                        fillColor = ~pal(log(cases+1)), 
                        fillOpacity = 0.8, 
                        color = "#BDBDC3", 
                        weight = 1,
                        popup = popup_dat, group = "County: total cases") %>%
            
            addPolygons(data = leaf.nyt.st,
                        fillColor = "white", 
                        fillOpacity = 0., 
                        color = "#BDBDC3", 
                        weight = 1, group = "County: cases per 100k") %>%
            
            addPolygons(data = leaf.nyt,
                        fillColor = ~pal(log(cases100k+1)), 
                        fillOpacity = 0.8, 
                        color = "#BDBDC3", 
                        weight = 1,
                        popup = popup_dat, group = "County: cases per 100k") %>%
            
            addPolygons(data = leaf.nyt.st,
                        fillColor = ~pal.st(log(cases+1)), 
                        fillOpacity = 0.8, 
                        color = "#BDBDC3",
                        popup = popup_dat_st,
                        weight = 1, group = "State: total cases") %>%
            
            # Layers control
            addLayersControl(
                # baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
                baseGroups = c("State: total cases", "County: total cases", "County: cases per 100k"),
                options = layersControlOptions(collapsed = FALSE)
            )
        
        wid <- toWebGL(map)
        wid
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
