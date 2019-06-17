
library(shiny)
library(DT)
library(leaflet)
library(sf)
library(tidyverse)
library(htmltools)
library(spatialEco)
library(rjson)
library(listviewer)
library(shinyWidgets)
library(plotly)


arcbzone <- readRDS('bzone.rds')
arcazone <- readRDS('azone.rds')
# funlookup <- read.csv('funlookup.csv')
scenariolist <- read.csv('result.csv')
funlookup <- read.csv('document_brief.csv')
deflator <- read.csv('defs/deflators.csv')
geo <- read.csv('defs/geo.csv')
units <- read.csv('defs/units.csv')
# Define UI for application that draws a histogram
ui <- navbarPage('VisionEval_ARC_test',
                 tabPanel('Intro',
                          h3('VisionEval:'),
                          fluidRow(
                              column(6, 
                                     p("VisionEval is a collaborative project in development to integrate the GreenSTEP family of strategic tools for performance-based transportation planning into a single open-source programming framework. Strategic tools are designed to evaluate many alternative futures and policies to help state and metropolitan area governments address pressing issues, despite uncertainty. The common framework enables new model features to be added in a 'plug-and-play' fashion so they can be easily shared among models. Use the resources below to learn more about this exciting project and how your agency can get involved.")
                              ),
                              column(6)
                          ) ,
                          br(),
                          h3('Resource:'),
                          p('HomePage:',tags$a(href="https://visioneval.org/","https://visioneval.org/")),
                          p('Github: ',tags$a(href="https://github.com/visioneval/visioneval","https://github.com/visioneval/visioneval")),
                          p('wiki:', tags$a(href="https://github.com/VisionEval/VisionEval/wiki","https://github.com/VisionEval/VisionEval/wiki"))
                 ),
                 tabPanel('PUMS',
                          p('The user is encouraged to substitute PUMS data for their region for the default data (which is Oregon) in the VESimHouseholds source package.'),
                          p('1. Use "Process_2000_PUMS.R "script from https://github.com/gregorbj/Process_2000_PUMS to generate pums_household.csv & pums_persons.csv for your region'),
                          p('2. Put this two file under VE-installer-windows/ve-lib/3.5.2/VESimHouseholds/exdata to repalce the default file'),
                          p('3. Double-click VE-installer-windows/VisionEval.bat to complete the installation and start VisionEval '),
                          p('4. If multi edtion of R installed on your machine, swith to 3.5.2, which is currently VE-installr buit for, before run you model.If you are using RStudio,click "Tools" -> "Global Options" -> "General" -> change the R version ')
                 ),
                 tabPanel('defs',
                          h4('1. model_parameters.json'),
                          jsoneditOutput('model_paramerters'),
                          h4('2. run_parameters.json'),
                          jsoneditOutput('run_paramerters'),
                          
                          h4('3. geo.csv'),
                          DT::dataTableOutput('geo'),
                          h6('  ARC Geography'),
                          fluidRow(
                              column(2),
                              column(5,HTML('<img src="ATL_VE_ABZONE.PNG" alt="Smiley face" height="500" width="500">')),
                              column(5,p('Region: Atlanta metropolitan area'),
                                     p('MArea: Atlanta metropolitan area'),
                                     p('Azones: 20 counties'),
                                     p('Bzones: 78 group of TAZs')
                              )
                          ),
                          
                          h4('4. deflators.csv'),
                          DT::dataTableOutput('deflator'),
                          
                          h4('5. units.csv'),
                          DT::dataTableOutput('units')
                          
                 ),
                 tabPanel('inputs',
                          selectInput('input_s','Select an user input',
                                      list('VESimHouseholds'       = list('azone_hh_pop_by_age.csv',
                                                                          'azone_hhsize_targets.csv',
                                                                          'azone_gq_pop_by_age.csv', 
                                                                          'azone_relative_employment.csv', 
                                                                          'azone_per_cap_inc.csv'), 
                                           'VELanduse'             = list('bzone_dwelling_units.csv', 
                                                                          'bzone_hh_inc_qrtl_prop.csv',
                                                                          'bzone_employment.csv',
                                                                          'bzone_lat_lon.csv',
                                                                          'bzone_urban-town_du_proportions.csv', 
                                                                          'bzone_unprotected_area.csv',
                                                                          'bzone_network_design.csv', 
                                                                          'bzone_urban-mixed-use_prop.csv',
                                                                          'bzone_parking.csv',
                                                                          'bzone_travel_demand_mgt.csv',
                                                                          'bzone_carsvc_availability.csv' ), 
                                           'VETransportSupply'     = list('marea_transit_service.csv',
                                                                          'bzone_transit_service.csv',
                                                                          'marea_lane_miles.csv'), 
                                           'VEHouseholdVehicles'   = list('region_hh_driver_adjust_prop.csv',
                                                                          'azone_hh_lttrk_prop.csv',
                                                                          'azone_carsvc_characteristics.csv',
                                                                          'azone_hh_veh_mean_age.csv',
                                                                          'azone_hh_veh_own_taxes.csv',
                                                                          'azone_payd_insurance_prop.csv'), 
                                           'VEHouseholdTravel'     = list('azone_prop_sov_dvmt_diverted.csv'), 
                                           'VEPowertrainsAndFuels' = list('azone_electricity_carbon_intensity.csv',
                                                                          'marea_transit_ave_fuel_carbon_intensity.csv',
                                                                          'marea_transit_biofuel_mix.csv',
                                                                          'marea_transit_fuel.csv',
                                                                          'marea_transit_powertrain_prop.csv',
                                                                          'region_ave_fuel_carbon_intensity.csv',
                                                                          'region_carsvc_powertrain_prop.csv',
                                                                          'region_comsvc_powertrain_prop.csv',
                                                                          'region_hvytrk_powertrain_prop.csv',
                                                                          'Azone_charging_availability.csv'), 
                                           'VETravelPerformance'   = list('region_base_year_dvmt.csv',
                                                                          'marea_base_year_dvmt.csv',
                                                                          'marea_dvmt_split_by_road_class.csv',
                                                                          'marea_operations_deployment.csv',
                                                                          'other_ops_effectiveness.csv',
                                                                          'marea_congestion_charges.csv',
                                                                          'marea_speed_smooth_ecodrive.csv',
                                                                          'azone_vehicle_access_times.csv',
                                                                          'azone_fuel_power_cost.csv',
                                                                          'azone_veh_use_taxes.csv',
                                                                          'region_prop_externalities_paid.csv',
                                                                          'region_co2e_costs.csv',
                                                                          'region_road_cost.csv',
                                                                          'region_comsvc_lttrk_prop.csv',
                                                                          'region_comsvc_veh_mean_age.csv')
                                      )),
                          h3('Input for :'),
                          textOutput('filename'),# Package/Module/...csv; another look up table
                          h3('Model functions:'),
                          textOutput('functions'),
                          h3('Input description:'),
                          uiOutput('fieldname'),
                          
                          br(),
                          DT::dataTableOutput('dt'),
                          selectInput("field_s", "Select field:",
                                      c("Item A", "Item B", "Item C")),
                          
                          fluidRow(column(6, h5('base year'),leafletOutput("baseyear")),
                                   column(6, h5('scenario'), leafletOutput("scenario")))
                 ),
                 tabPanel('outputs',
                          # only summary
                          DT::dataTableOutput('result')
                 ),
                 tabPanel('Scenarios', 
                          # currently we have ? 5-10 scenarios
                          # what would be include in this tab ? select scenarios  
                          # histogram
                          # datatable,  select one records,  showing on the plot;
                          # plotly ? or ggplot2 ? or highchart? ,
                          # introduction what mean is the LRTAC
                          h3('ARC: Scenarios'),
                          fluidRow(column(6, 
                                          checkboxGroupButtons(
                                              inputId = "CheckboxL",
                                              label = "L: Base Scenarios with altered sociodemographic, land use, and transportation inputs",
                                              choices = c("Full Steam Ahead - L1" = 1,
                                                          "Fierce Headwinds- L2" = 2,
                                                          "Tech Reigns- L3" =3),
                                              direction = "vertical",
                                              selected = 1
                                          ),
                                          checkboxGroupButtons(
                                              inputId = "CheckboxR",
                                              label = "R: Arterial Lane Mile Growth",
                                              choices = c("Low: Planned growth from the Regional Plan (3%)- R1" = 1, 
                                                          "Medium: Double the planned growth (6%)- R2" = 2,
                                                          "High: Triple the planned growth (9%)- R3" = 3),
                                              direction = "vertical",
                                              selected = 1
                                          ),
                                          
                                          checkboxGroupButtons(
                                              inputId = "CheckboxT",
                                              label = "T: Transit Service Growth",
                                              choices = c("Low: Planned growth from the Regional Plan (70%)- T1" = 1, 
                                                          "Medium: Double planned growth (140%) -T2" = 2,
                                                          "High: Triple planned growth (210%) -T3" = 3),
                                              direction = "vertical",
                                              selected = 1
                                          ),
                                          
                                          checkboxGroupButtons(
                                              inputId = "CheckboxA",
                                              label = "A: Autonomous Vehicles/Carsharing",
                                              choices = c("Low: Limited autonomous vehicles and carsharing available -A1" = 1, 
                                                          "High: Full deployment of autonomous vehicles and carsharing - A2" = 2),
                                              direction = "vertical",
                                              selected = 1
                                          ),
                                          checkboxGroupButtons(
                                              inputId = "CheckboxC",
                                              label = "C:Congestion Charges",
                                              choices = c("Low: No congestion charges- C1" = 1, 
                                                          "Medium: Limited congestion charges on limited access highways- C2" = 2,
                                                          "High: Extensive congestion charges on limited access highways, and less, but significant on arterial highways- C3" = 3),
                                              direction = "vertical",
                                              selected = 1
                                          ),
                                          DT::dataTableOutput('scenario_table')
                          ),
                          column(6, 
                                 # #ggplot
                                 # column(4,
                                 #        h6('Population residing in zone'),plotlyOutput("pop"),
                                 #        hr(),
                                 #        h6('Average daily vehicle miles per capita traveled by the household in autos or light trucks'), plotlyOutput('perdvmt'),
                                 #        hr(),
                                 #        h6('Average number of public transit trips per year per capita'), plotlyOutput('pertransit'),
                                 #        hr(),
                                 #        h6('Gasoline equivalent gallons consumed per day by household vehicle travel'),plotlyOutput('fuel')
                                 #        ),
                                 # column(4,
                                 #        h6('Number of workers residing in zone'),plotlyOutput("worker"),
                                 #        hr(),
                                 #        h6('Average number of walk trips per year per capita'),plotlyOutput('perwalk'),
                                 #        hr(),
                                 #        h6('Average number of vehicle trips per year per capita'),plotlyOutput('pervehicle'),
                                 #        hr(),
                                 #        h6('Grams of carbon-dioxide equivalents produced per day by household vehicle travel'),plotlyOutput('CO2e')
                                 #        ),
                                 # column(4,
                                 #        h6("Total daily vehicle miles traveled by the household in autos or light trucks"), plotlyOutput("tdvmt"),
                                 #        hr(),
                                 #        h6('Average number of bicycle trips per year per capita'),plotlyOutput("perbike"),
                                 #        hr(),
                                 #        h6('Total light-duty vehicle delay (hours per mile) on urban area roads'),plotlyOutput('delay')
                                 #        )
                                 fluidRow(column(4,h6('Population residing in zone'),plotlyOutput("pop")),
                                          column(4,h6('Number of workers residing in zone'),plotlyOutput("worker")),
                                          column(4,h6("Total daily vehicle miles traveled by the household in autos or light trucks"), plotlyOutput("tdvmt"))),
                                 hr(),
                                 fluidRow(column(4,h6('Average daily vehicle miles per capita traveled by the household in autos or light trucks'), plotlyOutput('perdvmt')),
                                          column(4,h6('Average number of walk trips per year per capita'),plotlyOutput('perwalk')),
                                          column(4,h6('Average number of bicycle trips per year per capita'),plotlyOutput("perbike"))),
                                 hr(),
                                 fluidRow(column(4,h6('Average number of public transit trips per year per capita'), plotlyOutput('pertransit')),
                                          column(4,h6('Average number of vehicle trips per year per capita'),plotlyOutput('pervehicle')),
                                          column(4,h6('Total light-duty vehicle delay (hours per mile) on urban area roads'),plotlyOutput('delay'))),
                                 hr(),
                                 fluidRow(column(4,h6('Gasoline equivalent gallons consumed per day by household vehicle travel'),plotlyOutput('fuel')),
                                          column(4,h6('Grams of carbon-dioxide equivalents produced per day by household vehicle travel'),plotlyOutput('CO2e')),
                                          column(4))
                          )
                          )
                          
                          
                          # ,
                          # DT::dataTableOutput('DTScenario')
                 )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    output$model_paramerters <- renderJsonedit(jsonedit(fromJSON(file = 'defs/model_parameters.json')))
    output$run_paramerters <- renderJsonedit(jsonedit(fromJSON(file = 'defs/run_parameters.json')))
    
    output$deflator <- DT::renderDataTable({
        DT::datatable(deflator, options = list(searching = FALSE,
                                               lengthMenu = list(c(5, -1), c('5', 'All')),
                                               pageLength = 5))
    })
    output$geo <- DT::renderDataTable({
        DT::datatable(geo, options = list(searching = FALSE,
                                          lengthMenu = list(c(5, -1), c('5', 'All')),
                                          pageLength = 5))
    })
    output$units <- DT::renderDataTable({
        DT::datatable(units, options = list(searching = FALSE,
                                            lengthMenu = list(c(5, -1), c('5', 'All')),
                                            pageLength = 5))
    })
    output$filename <- renderText({
        paste0(funlookup$Package[which(funlookup$filename==input$input_s)],'/',funlookup$Module[which(funlookup$filename==input$input_s)])
    })
    output$functions <- renderText({
        paste0(funlookup$Usefor[which(funlookup$filename==input$input_s)])
    })
    output$fieldname <- renderUI({
        HTML(paste0(funlookup$Explain[which(funlookup$filename==input$input_s)])) 
    })
    output$dt <- DT::renderDataTable({
        inputcsv <- read.csv(paste0('inputs/',input$input_s))
        
        
        if (exists('inputcsv')) {
            DT::datatable(inputcsv, options = list(searching = FALSE,
                                                   lengthMenu = list(c(5, -1), c('5', 'All')),
                                                   pageLength = 5))
        }
    })
    output$baseyear <- renderLeaflet({
        inputcsv <- read.csv(paste0('inputs/',input$input_s))
        if(nrow(inputcsv)==156){   #bzonedata
            map1 <- left_join(arcbzone,inputcsv[which(inputcsv$Year=='2015'),],by = c("DIVISION" = "Geo"))
            # pal <- colorBin('YlOrRd', c(min(inputcsv$SFDU),max(inputcsv$SFDU)))
            pal <- eval(parse(text=paste0("colorBin('YlOrRd', c(min(inputcsv$",input$field_s,"),max(inputcsv$",input$field_s, ")),bins = 5)")))
            # labs <- as.list(paste0(map1$DIVISION,': ',map1$SFDU))
            labs <- eval(parse(text=paste0("as.list(paste0(map1$DIVISION,': ',map1$",input$field_s,"))")))
            leaflet(map1) %>% 
                setView(lng =  -84.386330, lat = 33.753746, zoom = 8) %>%
                addProviderTiles(providers$CartoDB.Positron,    group = "Light") %>%
                addPolygons(color = "#444444", weight = 1, 
                            # fillColor = ~pal(SFDU),
                            fillColor = eval(parse(text=paste0('~pal(',input$field_s,')'))),
                            
                            # fillColor = eval(parse(text=paste0('~colorQuantile("YlOrRd", ','SFDU',', na.color = "#808080", alpha = FALSE)(','SFDU',')'))),
                            
                            fillOpacity = 0.8,
                            label = ~lapply(labs, HTML),
                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                bringToFront = TRUE)) %>%
                addLegend("bottomright",  pal = pal, values = eval(parse(text=paste0("~",input$field_s))),
                          title = paste0(input$field_s),
                          opacity = 0.8,
                          layerId = 1)  
            
        }
        else if(nrow(inputcsv)==40){  #azonedata
            map1 <- left_join(arcazone,inputcsv[which(inputcsv$Year=='2015'),],by = c("NAME10" = "Geo"))
            # pal <- colorBin('YlOrRd', c(min(inputcsv$SFDU),max(inputcsv$SFDU)))
            pal <- eval(parse(text=paste0("colorBin('YlOrRd', c(min(inputcsv$",input$field_s,"),max(inputcsv$",input$field_s, ")),bins = 5)")))
            labs <- eval(parse(text=paste0("as.list(paste0(map1$NAME10,': ',map1$",input$field_s,"))")))
            
            leaflet(map1) %>% 
                setView(lng =  -84.386330, lat = 33.753746, zoom = 8) %>%
                addProviderTiles(providers$CartoDB.Positron,    group = "Light") %>%
                addPolygons(color = "#444444", weight = 1, 
                            # fillColor = ~pal(SFDU),
                            fillColor = eval(parse(text=paste0('~pal(',input$field_s,')'))),
                            
                            # fillColor = eval(parse(text=paste0('~colorQuantile("YlOrRd", ','SFDU',', na.color = "#808080", alpha = FALSE)(','SFDU',')'))),
                            
                            fillOpacity = 0.8,
                            label = ~lapply(labs, HTML),
                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                bringToFront = TRUE)) %>%
                addLegend("bottomright",  pal = pal, values = eval(parse(text=paste0("~",input$field_s))),
                          title = paste0(input$field_s),
                          opacity = 0.8,
                          layerId = 1)  
            
        }
    })
    output$scenario <- renderLeaflet({
        inputcsv <- read.csv(paste0('inputs/',input$input_s))
        if(nrow(inputcsv)==156){   #bzonedata
            map2 <- left_join(arcbzone,inputcsv[which(inputcsv$Year=='2050'),],by = c("DIVISION" = "Geo"))
            # pal <- colorBin('YlOrRd', c(min(inputcsv$SFDU),max(inputcsv$SFDU)))
            pal <- eval(parse(text=paste0("colorBin('YlOrRd', c(min(inputcsv$",input$field_s,"),max(inputcsv$",input$field_s, ")),bins = 5)")))
            labs <- eval(parse(text=paste0("as.list(paste0(map2$DIVISION,': ',map2$",input$field_s,"))")))
            
            leaflet(map2) %>%
                setView(lng =  -84.386330, lat = 33.753746, zoom = 8) %>%
                addProviderTiles(providers$CartoDB.Positron,    group = "Light") %>%
                addPolygons( color = "#444444", weight = 1,
                             fillColor = eval(parse(text=paste0('~pal(',input$field_s,')'))),
                             fillOpacity = 0.8,
                             
                             label = ~lapply(labs, HTML),
                             highlightOptions = highlightOptions(color = "yellow", weight = 2,
                                                                 bringToFront = TRUE)) %>%
                addLegend("bottomright",  pal = pal, values = eval(parse(text=paste0("~",input$field_s))),
                          title = paste0(input$field_s),
                          opacity = 0.8,
                          layerId = 1)
        }     else if(nrow(inputcsv)==40){  #azonedata
            map2 <- left_join(arcazone,inputcsv[which(inputcsv$Year=='2050'),],by = c("NAME10" = "Geo"))
            # pal <- colorBin('YlOrRd', c(min(inputcsv$SFDU),max(inputcsv$SFDU)))
            pal <- eval(parse(text=paste0("colorBin('YlOrRd', c(min(inputcsv$",input$field_s,"),max(inputcsv$",input$field_s, ")),bins = 5)")))
            labs <- eval(parse(text=paste0("as.list(paste0(map2$NAME10,': ',map2$",input$field_s,"))")))
            
            leaflet(map2) %>% 
                setView(lng =  -84.386330, lat = 33.753746, zoom = 8) %>%
                addProviderTiles(providers$CartoDB.Positron,    group = "Light") %>%
                addPolygons(color = "#444444", weight = 1, 
                            # fillColor = ~pal(SFDU),
                            fillColor = eval(parse(text=paste0('~pal(',input$field_s,')'))),
                            
                            # fillColor = eval(parse(text=paste0('~colorQuantile("YlOrRd", ','SFDU',', na.color = "#808080", alpha = FALSE)(','SFDU',')'))),
                            
                            fillOpacity = 0.8,
                            label = ~lapply(labs, HTML),
                            highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                bringToFront = TRUE)) %>%
                addLegend("bottomright",  pal = pal, values = eval(parse(text=paste0("~",input$field_s))),
                          title = paste0(input$field_s),
                          opacity = 0.8,
                          layerId = 1)  
            
        }
    })
    
    observe({
        inputcsv <- read.csv(paste0('inputs/',input$input_s))
        
        # Can use character(0) to remove all choices
        if (is.null(inputcsv))
            inputcsv <- character(0)
        
        # Can also set the label and select items
        updateSelectInput(session, "field_s",
                          label = paste("Select field label"),
                          choices = colnames(inputcsv)[3:length(colnames(inputcsv))],
                          # selected = head(colnames(inputcsv), 1)
                          selected = colnames(inputcsv)[3] # !!! DF EDITED !!!
                          
                          
        )
    })
    
    # output
    output$result <- DT::renderDataTable({
        load('Datastore/2015/Bzone/Pop.Rda') # load 2015 population data
        pop2015 <- sum(Dataset)
        load('Datastore/2050/Bzone/Pop.Rda') # load 2050 population data
        pop2050 <- sum(Dataset)
        load('Datastore/2015/Bzone/NumWkr.Rda') # load 2015 worker data
        numwkr2015 <- sum(Dataset)
        load('Datastore/2050/Bzone/NumWkr.Rda') # load 2050 worker data
        numwkr2050 <- sum(Dataset)
        # load('Datastore/2015/Household/Dvmt.Rda')# load 2015 dvmt data
        # dvmt2015 <- sum(Dataset)
        # load('Datastore/2050/Household/Dvmt.Rda')# load 2050 dvmt data
        # dvmt2050 <- sum(Dataset)
        # load('Datastore/2015/Marea/VanDvmt.Rda')# load 2015 vandvmt data
        # vandvmt2015 <- sum(Dataset)
        # load('Datastore/2050/Marea/VanDvmt.Rda')# load 2050 vandvmt data
        # vandvmt2050 <- sum(Dataset)
        # load('Datastore/2015/Marea/BusDvmt.Rda')# load 2015 BusDvmt data
        # BusDvmt2015 <- sum(Dataset)
        # load('Datastore/2050/Marea/BusDvmt.Rda')# load 2050 BusDvmt data
        # BusDvmt2050 <- sum(Dataset)
        # load('Datastore/2015/Marea/RailDvmt.Rda')# load 2015 RailDvmt data
        # RailDvmt2015 <- sum(Dataset)
        # load('Datastore/2050/Marea/RailDvmt.Rda')# load 2050 RailDvmt data
        # RailDvmt2050 <- sum(Dataset)
        load('Datastore/2015/Marea/LdvFwyArtDvmt.Rda')# load 2015 LdvFwyArtDvmt data
        LdvFwyArtDvmt2015 <- sum(Dataset)
        load('Datastore/2050/Marea/LdvFwyArtDvmt.Rda')# load 2050 LdvFwyArtDvmt data
        LdvFwyArtDvmt2050 <- sum(Dataset)
        load('Datastore/2015/Marea/LdvOthDvmt.Rda')# load 2015 LdvOthDvmt data
        LdvOthDvmt2015 <- sum(Dataset)
        load('Datastore/2050/Marea/LdvOthDvmt.Rda')# load 2050 LdvOthDvmt data
        LdvOthDvmt2050 <- sum(Dataset)
        
        
        
        load('Datastore/2015/Household/WalkTrips.Rda')# load 2015 walktrip data
        walktrips2015 <- sum(Dataset)
        load('Datastore/2050/Household/WalkTrips.Rda')# load 2050 walktrip data
        walktrips2050 <- sum(Dataset)
        load('Datastore/2015/Household/BikeTrips.Rda')# load 2015 biketrip data
        BikeTrips2015 <- sum(Dataset)
        load('Datastore/2050/Household/BikeTrips.Rda')# load 2050 biketrip data
        BikeTrips2050 <- sum(Dataset)
        load('Datastore/2015/Household/TransitTrips.Rda')# load 2015 transittrip data
        TransitTrips2015 <- sum(Dataset)
        load('Datastore/2050/Household/TransitTrips.Rda')# load 2050 transittrip data
        TransitTrips2050 <- sum(Dataset)
        load('Datastore/2015/Household/VehicleTrips.Rda')# load 2015 VehicleTrips data
        VehicleTrips2015 <- sum(Dataset)
        load('Datastore/2050/Household/VehicleTrips.Rda')# load 2050 VehicleTrips data
        VehicleTrips2050 <- sum(Dataset)
        load('Datastore/2015/Marea/LdvTotDelay.Rda')# load 2015 LdvTotDelay data
        LdvTotDelay2015 <- sum(Dataset)
        load('Datastore/2050/Marea/LdvTotDelay.Rda')# load 2050 LdvTotDelay data
        LdvTotDelay2050 <- sum(Dataset)
        load('Datastore/2015/Household/DailyGGE.Rda')# load 2015 DailyGGE data
        DailyGGE2015 <- sum(Dataset)
        load('Datastore/2050/Household/DailyGGE.Rda')# load 2050 DailyGGE data
        DailyGGE2050 <- sum(Dataset)
        load('Datastore/2015/Household/DailyCO2e.Rda')# load 2015 DailyCO2e data
        DailyCO2e2015 <- sum(Dataset)
        load('Datastore/2050/Household/DailyCO2e.Rda')# load 2050 DailyCO2e data
        DailyCO2e2050 <- sum(Dataset)
        # dvmtper2050 <- sum(dvmt2050 , vandvmt2050 , BusDvmt2050 , RailDvmt2050) / pop2050
        # dvmtper2015 <- sum(dvmt2015 , vandvmt2015 , BusDvmt2015 , RailDvmt2015) / pop2015
        
        dvmt2015 <- LdvOthDvmt2015 + LdvFwyArtDvmt2015
        dvmt2050 <- LdvOthDvmt2050 + LdvFwyArtDvmt2050
        dvmtper2050 <- dvmt2050 / pop2050
        dvmtper2015 <- dvmt2015 / pop2015
        VehicleTrips2050 <- VehicleTrips2050 / pop2050
        VehicleTrips2015 <- VehicleTrips2015 / pop2015
        
        TransitTrips2050 <- TransitTrips2050/pop2050
        TransitTrips2015 <- TransitTrips2015/pop2015
        
        walktrips2015 <- walktrips2015/pop2015
        walktrips2050 <- walktrips2050/pop2050
        BikeTrips2015 <- BikeTrips2015/pop2015
        BikeTrips2050 <- BikeTrips2050/pop2050
        
        # LdvTotDelay2015 <- LdvTotDelay2015
        # LdvTotDelay2050 <- LdvTotDelay2050
        pop2015 <- format(round(pop2015,0), big.mark=",")
        pop2050 <- format(round(pop2050,0), big.mark=",")
        numwkr2015 <- format(round(numwkr2015,0), big.mark=",")
        numwkr2050 <- format(round(numwkr2050,0), big.mark=",")
        dvmt2015 <- format(round(dvmt2015,2), big.mark=",")
        dvmt2050 <- format(round(dvmt2050,2), big.mark=",")
        dvmtper2015 <- round(dvmtper2015,2)
        dvmtper2050 <- round(dvmtper2050,2)
        walktrips2015 <- round(walktrips2015*365,2)
        walktrips2050 <- round(walktrips2050*365,2)
        BikeTrips2015 <- round(BikeTrips2015*365,2)
        BikeTrips2050 <- round(BikeTrips2050*365,2)
        TransitTrips2015 <- round(TransitTrips2015*365,2)
        TransitTrips2050 <- round(TransitTrips2050*365,2)
        VehicleTrips2015 <- round(VehicleTrips2015*365,2)
        VehicleTrips2050 <- round(VehicleTrips2050*365,2)
        
        LdvTotDelay2015 <- format(round(LdvTotDelay2015,2), big.mark=",")
        LdvTotDelay2050 <- format(round(LdvTotDelay2050,2), big.mark=",")
        DailyGGE2015 <- format(round(DailyGGE2015,2),big.mark=",")
        DailyGGE2050 <- format(round(DailyGGE2050,2),big.mark=",")
        DailyCO2e2015 <- format(round(DailyCO2e2015,2),big.mark=",")
        DailyCO2e2050 <- format(round(DailyCO2e2050,2),big.mark=",")
        # 
        Description <- c('Population residing in zone',
                         'Number of workers residing in zone',
                         'Total daily vehicle miles traveled by the household in autos or light trucks',
                         'Average daily vehicle miles per capita traveled by the household in autos or light trucks',
                         'Average number of walk trips per year per capita',
                         'Average number of bicycle trips per year per capita',
                         'Average number of public transit trips per year per capita',
                         'Average number of vehicle trips per year per capita',
                         'Total light-duty vehicle delay (hours per mile) on urban area roads',
                         'Gasoline equivalent gallons consumed per day by household vehicle travel',
                         'Grams of carbon-dioxide equivalents produced per day by household vehicle travel'
        )
        data2015 <- c(pop2015,numwkr2015,dvmt2015,dvmtper2015, walktrips2015,BikeTrips2015,TransitTrips2015,VehicleTrips2015, LdvTotDelay2015,DailyGGE2015,DailyCO2e2015)
        data2050 <- c(pop2050,numwkr2050,dvmt2050,dvmtper2050, walktrips2050,BikeTrips2050,TransitTrips2050,VehicleTrips2050, LdvTotDelay2050,DailyGGE2050,DailyCO2e2050)
        result <- data.frame(Description,data2015,data2050)
        colnames(result) <- c("Description",'2015','2050')
        
        DT::datatable(result, options = list(searching = FALSE, pageLength = 15))
    })
    #---------------------------------------
    # renderplotly change color depends on input 
    # 'rgba(31,120,180,1)'----blue---selected
    # 'rgba(204,204,204,1)' ----grey---unselected
    colorbarplot <- reactive({
        color <- rep('rgba(204,204,204,1)',nrow(scenariolist))
        color[which(  (scenariolist$L %in% input$CheckboxL)
                      & (scenariolist$R %in% input$CheckboxR)
                      & (scenariolist$T %in% input$CheckboxT)
                      & (scenariolist$A %in% input$CheckboxA)
        )] <- rep('rgba(31,120,180,1)', length(color[which(  (scenariolist$L %in% input$CheckboxL)
                                                             & (scenariolist$R %in% input$CheckboxR)
                                                             & (scenariolist$T %in% input$CheckboxT)
                                                             & (scenariolist$A %in% input$CheckboxA))]
        )
        )
        color
    })
    
    output$pop <- renderPlotly({
        plot_ly(scenariolist, x = ~X, y = ~Population,type = 'bar',marker=list(color = colorbarplot()))%>%
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = "Population"))
        
    })
    output$perwalk <- renderPlotly({
        plot_ly(scenariolist, x = ~X, y = ~bike.trip.per.year.per.capita,type = 'bar',marker=list(color = colorbarplot())) %>%
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = "Bike Trips per year per capita"))
    })
    output$delay <- renderPlotly({
        plot_ly(scenariolist, x = ~X, y = ~Ldv.delay.total,type = 'bar',marker=list(color = colorbarplot())) %>% 
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = "Ldv total delay (Hours)"))
    })
    output$worker <- renderPlotly({
        plot_ly(scenariolist, x = ~X, y = ~N.Worker, type = 'bar',marker=list(color = colorbarplot()))%>% 
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))
    })
    output$perbike <- renderPlotly({
        plot_ly(scenariolist, x = ~X, y = ~bike.trip.per.year.per.capita, type = 'bar', marker=list(color = colorbarplot()))%>% 
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))
    })
    output$fuel <- renderPlotly({
        plot_ly(scenariolist, x = ~X, y = ~Gasoline.equivalent.gallons,
                type = 'bar',
                marker=list(color = colorbarplot()))%>% 
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))
    })
    output$tdvmt <- renderPlotly({
        plot_ly(scenariolist, x = ~X, y = ~LdvVMT.total, type = 'bar',marker=list(color = colorbarplot()))%>% 
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))
    })
    output$pertransit <- renderPlotly({
        plot_ly(scenariolist, x = ~X, y = ~Public.transit.trips.per.year.per.capita, 
                type = 'bar' ,marker=list(color = colorbarplot()))%>%
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))
        
    })
    output$CO2e <- renderPlotly({
        plot_ly(scenariolist, x = ~X, y = ~CO2e.g., 
                type = 'bar' ,marker=list(color = colorbarplot())) %>%
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))
    })
    output$perdvmt <- renderPlotly({
        plot_ly(scenariolist, x = ~X, y = ~Ldv.DVMT.pe.capita, 
                type = 'bar' ,marker=list(color = colorbarplot())) %>%
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))
    })
    output$pervehicle <- renderPlotly({
        plot_ly(scenariolist, x = ~X, y = ~Vehicle.trip.per.year.per.capita, 
                type = 'bar' ,marker=list(color = colorbarplot())) %>%
            layout(title = "",
                   xaxis = list(title = ""),
                   yaxis = list(title = ""))
    })
    
    output$scenario_table <- DT::renderDataTable({
        
        # select L R T A C
        scenariolist1 <- scenariolist[which(  (scenariolist$L %in% input$CheckboxL)
                                              & (scenariolist$R %in% input$CheckboxR)
                                              & (scenariolist$T %in% input$CheckboxT)
                                              & (scenariolist$A %in% input$CheckboxA)
        ), ]
        # scenariolist1 <- scenariolist
        scenariolist2 <- scenariolist1[, c('X','Population','N.Worker','LdvVMT.total','Ldv.DVMT.pe.capita','walk.trips.per.year.per.capita',
                                           'bike.trip.per.year.per.capita', 'Public.transit.trips.per.year.per.capita','Vehicle.trip.per.year.per.capita',
                                           'Ldv.delay.total','Gasoline.equivalent.gallons','CO2e.g.')] 
        DT::datatable(scenariolist2,
                      colnames = c('Scenario' = 'X', 
                                   'POP' = 'Population',
                                   'WKR' = 'N.Worker',
                                   'LDV VMT TOT' = 'LdvVMT.total',
                                   'LDV Dvmt PC' = 'Ldv.DVMT.pe.capita' ,
                                   'Walk trips PC' = 'walk.trips.per.year.per.capita',
                                   'bike trips PC' = 'bike.trip.per.year.per.capita',
                                   'Transit trips PC' = 'Public.transit.trips.per.year.per.capita' ,
                                   'VEH trips PC' = 'Vehicle.trip.per.year.per.capita' ,
                                   'LDV DLY TOT ' = 'Ldv.delay.total',
                                   'GAS EQUIV used' = 'Gasoline.equivalent.gallons' ,
                                   'CO2 EQUIV emission' = 'CO2e.g.' ),
                      options = list(searching = FALSE, pageLength = 15),rownames = F)
        
    })
    
}



# Run the application 
shinyApp(ui = ui, server = server)

