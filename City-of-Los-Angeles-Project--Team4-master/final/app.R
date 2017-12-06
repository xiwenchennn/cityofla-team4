
######
######Please run the working place.R 【FIRST】before you run this shiny app#####
#####


library(shiny)
library(shinythemes)
library(lubridate)
library(dplyr)
library(leaflet)


ui <-navbarPage(
  theme = shinytheme("flatly"),
  title = "LA City Homelessness Project",
  
  tabPanel("Introduction",
           fluidPage(
             br(),
             br(),
             h3("Overviews and Goals"),
             #text in paragraph
             fluidRow(
               br(),
               column(6,
                      p(tags$b("Overviews:"),"In 2017, there are more than ", tags$em("50,000", style = "color:blue"),
                        "on the street in the LA County, and about", tags$em("75%", style = "color:blue"),"of homeless people are unsheltered.", 
                        tags$em("2645", style = "color:blue"), "crimes was committed towards homeless people from Auguest 2016 to Auguest 2017, 
                        and about", tags$em("30%", style = "color:blue"), "of the crimes are assulting with deadly weapon. 
                        The increasing serverity of homeless density in the Los Angeles is partly because the increasing rents. In 2017, 
                        the median price of 1B1B apartment is $1,997. The median rental price incresed 30% between 2000 and 2015, but the 
                        median income did not change during this time."
                        
                      ),
                      p(tags$b("Project Goals:"), "Our goal is to measure the",tags$em("homeless density",style = "color:blue"),"in the greater Los Angeles Area,
               identify potential", tags$em("risks and health issues",style = "color:blue"), "towards homeless people,
                            figure up the", tags$em("services", style = "color:blue"), "provided by the governments and instituions,
                            and provide implantable", tags$em("recommendations", style = "color:blue"), "for the city of Los Angeles on the homeless issues.")),
               column(6,img(src = "homeless1.png", width = "100%", height = "100%", aligh = "left"))),
             br(),
             br(),
             fluidRow(
               column(6,br(),img(src='homeless.png', width = "100%", height = "100%", align = "left"),
                      h6("There is a great increase in unsheltered homeless people from 2015 - 2017, which results in the huge increase in total homeless people."),style = "color:grey"),
               column(6, h3("Data Sets"), 
                      br(),
                     p("The data sets are provided by the city of Los Angeles,
                                            and the data is collected in the Greater Los Angeles area.
                                            The data included the 311 Calls, Crime Data, Shelter Data, Homeless Counts, and Tract maps."),
                      p(tags$b("311 Calls:"), "Received 311 Calls from various Apps and Devices: Locations and Times."),
                      p(tags$b("Crime Data:"), "Reported crimes towards homeless people in the Greater Los Angeles Area: Locations, Times and Crime Types"),
                      p(tags$b("Shelter Data:"), "Shelter in Greater Los Angeles Area: Location, Types and Times "),
                      p(tags$b("Homeless Count Data"), "Homeless count by tract and community: Location, Times, Family Numbers, with/without Youth, and Sheltered/Unsheltered ")))
             
             
             
             
             )),

  navbarMenu("Data Analysis",
             
             tabPanel("Homeless Data", fluidPage(
               sidebarLayout(
                 mainPanel(leafletOutput("tot_map"),class = "panel panel-default",width = "100%", height = "100%"),
                           absolutePanel(id = "homelesscontrol",fixed = TRUE,
                               draggable = TRUE, top = "auto", left = "auto", right = 20, bottom = "auto",
                               width = 400, height = "auto",
                              
                               
                               h4("Explore the homeless by Tract or by Council District"),
                               selectInput(inputId = "crime_dataset",
                                           label = "DATASETS",
                                           choices = list("By District", "By Tract"),
                                           selected = "By District")
                 )
                 
               ) 
             ),
             fluidRow(
               column(6, h4(tags$ul(tags$li("From the district level and the tract level maps, 
                                            the downtown area of Los Angeles city has the greatest number of homeless people.")))),
               column(6)
             )),
             
             
             tabPanel("311 Call Data", fluidPage(
              
          
               
               sidebarLayout(
                 
                 mainPanel(leafletOutput("call"),class = "panel panel-default",width = "100%", height = "100%"),
                 absolutePanel(id = "311control",
                               draggable = TRUE, top = "auto", left = 50, right = "auto", bottom = "auto",
                               width = "100%", height = "auto",
                               h4("Create maps with information from 311call dataset."),
                               sliderInput("week", 
                                           label = "Week:",
                                           min = 1, max = 44, value = 5, width = "80%")
                                 
                   )
                
                        
               )
             ),
             fluidRow(br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
                      br(),
               column(11,h4(tags$ul(tags$li("The red area is the Los Angeles city, where we focused our analysis on. Each blue circle represents a 311 call received. 
                                            The area that contains most of blue circles indicates higher number of 311 calls received."))),
               column(1)
             )),
             
             fluidRow(
                      br(),
                      column(6, plotOutput("device311")),
                      column(6, h4(tags$ul(tags$li("We plotted the number of 311 calls received by the difference source, such as mobile App and calls. We can see from the chart that about 50% of people reports homeless by mobile App. Other common sources used to report homeless people are self-service, call and emails. 
"),
                                           br(),tags$li("Since there are more people like to use mobile app to report homeless, the city of Los Angeles could perfect their apps to simplify the reporting process. Moreover, city of Los Angeles can also send out notification to the residents using apps to let them know when the cases they reported are resolved.")))),
                      br(),
                      br(),
                      br()
                      
                      ),
             fluidRow(
               br(),
               br(),
               column(6,plotOutput("month311")),
               column(6, h4(tags$ul(tags$li("We can see a clear increasing trend of the number of 311 received from February to September. The months that have the highest 311 calls are August, September and October."),
                                    br(),tags$li("There are more 311 calls received in summer than the winter time. One reason that in summer the number of 311 calls is higher is that there are more homeless people on the street in the summer. Another possible reason is that in the summer people are more active and like to go outside. Therefore, they may have more chance to see homeless people outside. 
"),
                                    br(),tags$li("The city of Los Angeles could provide more shelters and services in the summer time, when the number of homeless people are on the peak. In the winter time, the city of Los Angeles could encourage residents to report the homeless they saw."))))
             )
             ),
             
             tabPanel("Crime Data", fluidPage(
               sidebarLayout(
                 mainPanel(leafletOutput("crime_map"),
                  
                           class = "panel panel-default",width = "100%", height = "100%"),
                 absolutePanel(id = "crimecontrol",
                               draggable = TRUE, top = "auto", left = 50, right = "auto", bottom = "auto",
                               width = 800, height = "auto",
                               
                               
                               h4("Explore number of crimes by Community or by Tract"),
                               
                   selectInput(inputId = "dataset",
                               label = "DATASETS",
                               choices = list("By District", "By Tract"),
                               selected = "By District")
                 )
                )
                
               ), fluidRow(br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           br(),
                           column(6, plotOutput("hour_crime")),
                           column(6, h4(tags$ul(tags$li("We plotted the number of crimes reported by hour. In general, there are more crimes reported at noon and midnight. 
"),
                                                br(),tags$li("It’s reasonable that more crimes reported at midnight because criminals usually show up in the evening and there is less police on the street. However, the possible reason that there are more crimes reported at noon is there are more witnesses on the street because people usually go out for lunch.
"),
                                                br(),tags$li("The suggestion for reducing the number of homeless victims is that to add more police on the street at midnight, and set up an emergency service line for the homeless people.")))
                                  )
               ),
               fluidRow(br(),
                      
                      
                        br(),
                        column(6,plotOutput("monthly_crime")),
                        column(6,h4(tags$ul(tags$li("We plotted the number of crimes reported from August 2016 to August 2017, and try to figure out the relationship between months and the number of crimes. It’s obvious to see from the graph that the number of crimes reported in the winter season (from November to February) is greater than the number of crimes reported in the summer season (from June to September).
"),
                                            br(),tags$li("One possible explanation of this trend is that there are more homeless people on the street in the summer, so they will face high risks of being hurt. Meanwhile, people tend to be more active in summer than in winter. More outdoor activities in summer months might be a possible cause of this trend.
"),
                                            br(),tags$li("Even in the same month, we could see that the number of crimes reported in 2017 is greater than that reported in 2016, which is consistent with the trend of homeless population from 2016 to 2017.
"))))
                
               )),

             
             tabPanel("Shelter Data", 
                      fluidPage(
                 mainPanel(leafletOutput("shelter"),
                           class = "panel panel-default",width = "100%", height = "100%"),
                helpText("Zoom in and Zoom out to see more specific distributions of shelters")
             
             ),
             fluidRow(
               column(11, h4(tags$ul(tags$li("Most of the shelters are distributed around Los Angeles City. Among those, 37 shelters are located in the downtown area of Los Angeles City as is depicted in the above screen shot.
"),
                                    br(),tags$li("It is necessary to build more shelters where homeless people are most gathered.
")
                                    ))),
               column(1)
             ))
             ),
  
  navbarMenu("Risk Measurement",
             tabPanel("Unsheltered Severity",
             
            fluidPage(
              
               fluidRow(
                 column(6,plotOutput("severetop", width = "100%", height = "400px")),
                 column(6, plotOutput("decmap",width = "100%", height = "400px"))
              
             ),
             br(),
             br(),
            

             fluidRow(
               column(6,plotOutput("severebottom",width = "100%", height = "400px")),
               column(6, plotOutput("mymap",width = "100%", height = "400px"))
             ),
               
             fluidRow(
              h4(tags$ul(tags$li("When looking at unsheltered people in the County of Los Angeles, we developed a measurement called “Unsheltered Severity” Index for each city.",   tags$em("Formula: Unsheltered Severity = Unsheltered Count x Unsheltered Percentage", style = "color:blue"),"We developed this measurement because we noticed that certain cities have very high amount of unsheltered homeless, yet the percentage compared the total homeless people is relatively low because the total amount of homeless people is very high. Similarly, certain cities have low number of homeless people compared to other cities, yet the percentage of unsheltered homeless is very high."),br(),tags$li("After calculating the “Unsheltered Severity” index for each city and for each year, we calculated the change in severity over the years. For the cities with data for all three years (2015 – 2017), we calculated the change from 2015 to 2017. For cities with data for only two years (2016 – 2017), we calculated the change from 2016 to 2017. We then ranked the cities with the top 10 increase and decrease in “Unsheltered Severity”. Cities showing increase in unsheltered severity imply that they are doing worse over the years, and cities showing decrease in unsheltered severity mean that they are doing better in diminishing unsheltered homeless. We also made two graphs to demonstrate the locations of the cities on geographical maps for the top increasing and decreasing cities. The size of the dot represents the scale of increase or decrease in severity. The bigger the dot, the more change we see over the years. 
")
                      
                         ,
                         br(),tags$li("We see that the top 10 cities that see the most increase in unsheltered severity are Topanga, Palmdale, Whittier Narrows, West Compton, Irwindale, Malibu, Lynwood, South San Gabriel, Montebello, and Los Angeles County (in order). These cities should investigate the reasons behind the increase, especially Topanga that sees an increase of 125 index points over the years. The top 10 cities that see the most decrease in unsheltered severity are Santa Fe Springs, Unincorporated Westlake Village, Avocado Heights, Westmont/ West Athens, Vemon, Unincorporated Channel Islands, Lancaster, Palmdale, Bellflower, and Commerce (in order). We can also conduct more research to explore what has caused the decrease and perhaps implement similar measures to other cities.  
")))
                 )
             
            ))
            ,
             
  
            tabPanel("Homeless Density", fluidPage(
              mainPanel(leafletOutput("tot_density"),
                        class = "panel panel-default",width = "100%", height = "100%"
                       ),
              helpText("Density = Total Homeless Population/ Area of District (Square Mile)"),
              fluidRow(
                column(11, h4(tags$ul(tags$li("This chart shows a geographical chart of Los Angeles City map, in which the color scale represents the density of total homeless people density by council district. Density is calculated as the total homeless people divided by council district area. The darker the area, the higher the density.  
"),
                                     br(),tags$li("From the map, we can see that council district 14 has the highest density with more than 400 homeless people per square mile, almost high as three times as the second highest district. District 14 consists of all or part of the neighborhoods of the Downtown, Boyle Heights, Eagle Rock, El Sereno, Garvanza, Glassell Park, Lincoln Heights, and Monterey Hills. In accordance with the total homeless people distribution, downtown area also has high density even when considering area in.
")))),
                column(1)
            ))),
             
      tabPanel("Crime Rate", fluidPage(
               mainPanel(leafletOutput("crimedensity"),
                         class = "panel panel-default",width = "100%", height = "100%"),
               helpText("Density = Total Crime Number/ Total Homeless People"),
             
             fluidRow(
               column(11, h4(tags$ul(tags$li("Crime density is calculated as the number of crime victims divided by total homeless people in the area. The darker the color of the district, the higher the crime density."),
                                    br(),tags$li("We see that the areas with the highest density (dark blue) are adjacent to each other. The top three districts with the highest crime density are Council District number 2, 14, and 1. District 2 covers mostly the communities of North Hollywood, Studio City, Sun Valley. And District 14 and 1 covers most of downtown Los Angeles area. District 2 has an exceptionally high crime density of 0.196 crimes per square mile, while the second-place district 14 has a crime density of 0.116 crimes per square mile, and third-place district 1 has a density of 0.111. In general, we observed that the areas closer to downtown usually has a higher crime density.
")))),
               column(1))))),
      
      tabPanel("Recommendations",
               fluidPage(
                 h3("Causes of Homeless:"),
                 p(tags$ul(
                   tags$li("Povery: Lack of affordable housing"), 
                   tags$li("Mental illness: Discharged from mental hospitals"), 
                   tags$li("Released from the criminal justice system"))
                 ),
                 h3("Recommendations:"),
                 p("To achieve the goal of reducing homelessness in LA by 50% by the year of 2020, our recommendation comes in two parts. First is to decrease the number of currently homeless people. Second is prevention."),
                 p("1. Affordable Housings – \"Tiny Houses\"", tags$ul(tags$li("First way to decrease the number of currently homeless people is to build more affordable housing. 
Areas with higher homeless density, severity, and crime rate according to the data should be given priority. In addition, on top of traditional permanent housing accommodations, we found there is a project called “tiny houses”. It’s a low-cost temporary housing that serves as a bridge between gutter and permanent housing. 
                                                                               It has all the living necessities inside and can greatly reduce health and crime risks for the homeless."))), 
                 p("2. Branches for Successful Shelters – Focus on Career Training", tags$ul(tags$li("Second way to decrease the number homeless is to provide more Shelter and Services. Similarly, they should be built in the most severe areas as identified by the data. We recommend opening more branches of existing successful shelters. For example, the Union Station Homeless Service in Pasadena helped reduce homeless in the city by 54% since 2009. The shelters should be more focused on career training, job seeking, and mental consultation, instead of merely providing a place to sleep.
"))),
                 p("3. Easier Accesses to Shelters and Services", tags$ul(tags$li("With more facilities in place, we also need to have easier access to the services. Currently, only 16% of homeless people seek local services. This low ratio is largely due to difficult access to services. We recommend setting up service points at local stores such as 7-11, Walgreen, and CVS. The service points should provide free calling service to local shelter, easy application process to shelters, basic drugs for those in need, consultation service for mental issues, and report for crimes and dangers.
"))),
                 p("4. Employment Programs", tags$ul(tags$li("Since the majority of homelessness is caused by unemployment, we recommend setting up better employment programs to help the homeless better seek a job. First, we can recruit people who previously graduated from successful shelter to build new shelters in the severe areas. Depending on their individual skills, they can be hired as construction workers, administrative staff, cooks, or teachers. Second, we suggest initiating a “Buddy Program” to connect previous homeless people with new homeless people. People escaped from homeless can introduce job opportunities, provide mental support, and teach ways to combat homelessness and poverty.
"))),
                   
                   p("5. Homeless Prevention", tags$ul(tags$li("The second part of our recommendation is prevention. Currently, 25% of single adult homeless suffer from mental illness, and 20% of people released from incarceration go to the street. The news and public has concluded that people released from mental hospitals and prisons without proper accommodations are a huge cause for homelessness. Therefore, we recommend forming better connection between mental hospitals and local homeless services, as well as between jails and homeless services. It can prevent mentally-ill people released from hospitals from going on the street, as well as those released from the prison. Since they usually need to readjust to lives outside of the hospital and prison, they can spend time in shelter services to acquire the basic skills necessary for employment.
")))
                   
                   
                   
                 )))




server <- function(input, output, session){
  
  
  datasets <- observe({
    if(input$crime_dataset == "By District"){
  
      
      renderTable ({total_commu = data_census17_tract %>%
        group_by(CD) %>%
        summarise(Number = sum(totPeople))
      total_commu$CD = as.character(total_commu$CD)
      commu_combine = left_join(community_map, total_commu,
                                by = c("id" = "CD"))
      })
      community1 = distinct(commu_combine, id, Number)
      commuid = community1$id
    
      commupolygons<-lapply(commuid, function(x) polyFunc(x, dat=commu_combine))
      
      commusp.polygon<-SpatialPolygons(commupolygons)
      commudf.polygon<-SpatialPolygonsDataFrame(commusp.polygon,
                                                data=data.frame(row.names=commuid, community1))
      pal <- colorNumeric(
        palette = "Blues",
        domain = commudf.polygon$Number
      )
      
      
      output$tot_map = renderLeaflet({
        
        leaflet(width = "100%", height = 600)%>%
          addTiles() %>%
          addPolygons(data = commudf.polygon,
                      fillColor = ~pal(Number),
                      color = "#5297A8", 
                      fillOpacity = 0.8, 
                      weight = 1, 
                      smoothFactor = 1)%>%
          addLegend(pal = pal, 
                    values = commudf.polygon$Number, 
                    position = "bottomright", 
                    title = "Number of Homeless People by District")
        
        
      })
    }
    else{
      
      tract_map = fortify(tract_map2,region = "CT10")
      
      tract_map$id = as.numeric(tract_map$id)
      
      ladata = data_census17_tract %>%
        filter(City == "Los Angeles") %>%
        arrange(-totPeople) %>%
        slice(2:1004)
      
      combine = tract_map %>%
        left_join(ladata,
                  by = c("id"="tract")) %>%
        filter(City == "Los Angeles")
      
      
      
      tract_map1 = distinct(combine, id,  totPeople)
      tractid <- tract_map1$id
      
      polyFunc<-function(groupname, dat){
        poly<-filter(dat, id==groupname) %>%
          dplyr::select(long, lat)
        return(Polygons(list(Polygon(poly)), groupname))
      }
      
      polygons<-lapply(tractid, function(x) polyFunc(x, dat=combine))
      
      sp.polygon<-SpatialPolygons(polygons)
      df.polygon<-SpatialPolygonsDataFrame(sp.polygon,
                                           data=data.frame(row.names=tractid, tract_map1))
      pal <- colorNumeric(
        palette = "Blues",
        domain = df.polygon$totPeople
      )
      
      output$tot_map = renderLeaflet({
        leaflet(width = "100%", height = 600)%>%
        addTiles() %>%
        addPolygons(data = df.polygon,
                    fillColor = ~pal(totPeople),
                    color = "#5297A8", 
                    fillOpacity = 0.8, 
                    weight = 1, 
                    smoothFactor = 1)%>%
        addLegend(pal = pal, 
                  values = df.polygon$totPeople, 
                  position = "bottomright", 
                  title = "Number of Homeless People by Tract")
  })
    }
    
  })
  

  ### 311 call  
  data_311$Week = week(data_311$CREATEDDATE)
  
  output$call <- renderLeaflet({
    
    data_week311 = data_311 %>%
      filter(Week == input$week) 
    
    data_week311%>%
      leaflet()%>%  
      addTiles() %>%
      addPolygons(data = community_map1,
                  color = "#c8515f", 
                  fillOpacity = 0.5, 
                  weight = 1, 
                  smoothFactor = 1) %>%
      setView(lng = -118.2437, lat = 34.065, zoom = 10) %>%
      addCircleMarkers(lng=data_week311[,23], lat=data_week311[,22],
                       radius = 6, stroke = FALSE, fillOpacity = 0.5)
  })
    
  
  output$device311 <- renderPlot({
    
    ggplot(requestsource, aes(x = REQUESTSOURCE, y = count))+
      geom_bar(stat = "identity", fill = "lightblue")+
      labs(x = "Requestsource", y = "Number", title = "311 Call Top5 Resource")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background= element_blank(), axis.line = element_line(colour = "black"))+
      theme(plot.title = element_text(face = "bold",size = 26, hjust = 0.5))+
      theme(axis.title = element_text(size = 14))
  
  
  })  
  
  output$month311 <- renderPlot({
    
    data_311$month = str_sub(data_311$CREATEDDATE, start = 6, end = 7)
    data_311 %>%
      filter(month != "11") %>%
      group_by(month) %>%
      summarise(number = n()) %>%
      ggplot(aes(x = month, y = number))+
      geom_bar(stat = "identity", fill = "lightblue")+
      labs(x = "Month",y = "Number", title = "Number of 311 Calls by Month")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background= element_blank(), axis.line = element_line(colour = "black"))+
      theme(plot.title = element_text(face = "bold",size = 26, hjust = 0.5))+
      theme(axis.title = element_text(size = 14))
  })
  
  
  
  
  
##### Crime list  
  datasets <- observe({
    if(input$crime_dataset == "By District"){
      
    
      renderTable ({cd_crime = data_crime %>%
        left_join(data_census17_tract,
                  by = c("CT10" = "tract")) %>%
        group_by(CD) %>%
        summarise(Number = n())
      
      cd_crime$CD = as.character(cd_crime$CD)
      crime_combine = left_join(community_map, cd_crime,
                                by = c("id" = "CD"))
      })
      
      
      crime1 = distinct(crime_combine, id, Number)
      crimeid = crime1$id
      
      crimepolygons<-lapply(crimeid, function(x) polyFunc(x, dat=crime_combine))
      
      crimesp.polygon<-SpatialPolygons(crimepolygons)
      crimedf.polygon<-SpatialPolygonsDataFrame(crimesp.polygon,
                                                data=data.frame(row.names=crimeid, crime1))
      
      pal <- colorNumeric(
        palette = "Blues",
        domain = crimedf.polygon$Number
      )
      
      output$crime_map <- renderLeaflet({
        leaflet()%>%
          addTiles() %>%
          addPolygons(data = crimedf.polygon,
                      fillColor = ~pal(Number),
                      color = "#5297A8", 
                      fillOpacity = 0.8, 
                      weight = 1, 
                      smoothFactor = 1)%>%
          addLegend(pal = pal, 
                    values = crimedf.polygon$Number, 
                    position = "bottomright", 
                    title = "Number of Crime by District")
      })
    }
    
    else{
      
      crime_tract = data_crime %>%
        group_by(CT10) %>%
        summarise(Number = n())
      
      total_crime = tract_map %>%
        left_join(crime_tract,
                  by = c("id"="CT10"))
      
      crime_map = distinct(total_crime, id,  Number)
      crime_map = crime_map %>%
        filter(Number != "NA")
      crimemapid <- crime_map$id
      
      crimepolygons<-lapply(crimemapid, function(x) polyFunc(x, dat=total_crime))
      
      sp.polygon_crime<-SpatialPolygons(crimepolygons)
      df.polygon_crime<-SpatialPolygonsDataFrame(sp.polygon_crime,
                                                 data=data.frame(row.names=crimemapid, crime_map))
      pal <- colorNumeric(
        palette = "Blues",
        domain = df.polygon_crime$Number
      )
      
     output$crime_map <- renderLeaflet({
       leaflet()%>%
         addTiles() %>%
         addPolygons(data = df.polygon_crime,
                     fillColor = ~pal(Number),
                     color = "#5297A8", 
                     fillOpacity = 0.5, 
                     weight = 1, 
                     smoothFactor = 1)%>%
         addLegend(pal = pal, 
                   values = df.polygon_crime$Number, 
                   position = "bottomright", 
                   title = "Number of Crime by Tract")
     })
      
    }
  })
  
  
  ###Crime
  data_crime$TIME.OCCURRED = 
    format(strptime(substr(as.POSIXct(sprintf("%04.0f", data_crime$TIME.OCCURRED), 
                                      format="%H%M"), 12, 16), '%H:%M'), '%I:%M %p')
  
  data_crime$TIME.OCCURRED = strptime(data_crime$TIME.OCCURRED, "%I:%M %p")
  
  data_crime$TIME.OCCURRED = 
    str_sub(string = data_crime$TIME.OCCURRED, start = 12, end = 13)
  
  
  output$hour_crime = renderPlot({
    data_crime %>%
      group_by(TIME.OCCURRED) %>%
      summarise(number_occured = n()) %>%
      ggplot(aes(x = TIME.OCCURRED, y = number_occured))+
      geom_bar(stat = "identity", fill = "light blue")+
      labs(y = "The Number of Crime", x = "Hour", title = "The Number of Crime Reported by Hour")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            panel.background= element_blank(), axis.line = element_line(colour = "black"))+
      theme(plot.title = element_text(face = "bold",size = 18))+
      theme(axis.title = element_text(size = 14))+
      coord_fixed(ratio = 1/10)
  }) 
  
  
  la_shelter = data_shelter %>%
    filter(CITY == "Los Angeles")
  xy <- la_shelter[,c(1,2)]
  sp_lashelter = SpatialPointsDataFrame(coords = xy, data = la_shelter,
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  
  ###pal <- colorFactor(palette = "Blues", domain = la_shelter$ZIP)
  
  
  output$shelter<-renderLeaflet({
    leaflet(sp_lashelter) %>%
      addTiles() %>%
      addPolygons(data = community_map1,
                  color = "lightblue", 
                  fillOpacity = 0.3, 
                  weight = 1, 
                  smoothFactor = 1) %>%
      addMarkers(lng=la_shelter[,1], lat=la_shelter[,2], popup="shelters",clusterOptions = markerClusterOptions())
  
      
  })
 
  
  ####total density
  
  #ommunity_map = fortify(community_map1, region = "DISTRICT")
  #communitymap$DISTRICT = as.numeric(communitymap$DISTRICT)
  #communitymap$SQ_MI = as.numeric(communitymap$SQ_MI)
  
  communitymap$DISTRICT = as.character(communitymap$DISTRICT)
  data_census17_tract$CD = as.character(data_census17_tract$CD)
  communitymap$SQ_MI = as.numeric(as.character(communitymap$SQ_MI))
  total_commu$CD = as.character(total_commu$CD)
  total_commu = data_census17_tract %>%
    group_by(CD) %>%
    summarise(Number = sum(totPeople)) %>%
    left_join(communitymap,
              by = c("CD" = "DISTRICT")) %>%
    mutate(Density = Number/SQ_MI)
  
  
  total_commu$CD = as.character(total_commu$CD)
  commu_combine = left_join(community_map, total_commu,
                            by = c("id" = "CD"))
  
  community1 = distinct(commu_combine, id, Density)
  commuid = community1$id
  
  commupolygons<-lapply(commuid, function(x) polyFunc(x, dat=commu_combine))
  
  commusp.polygon<-SpatialPolygons(commupolygons)
  commudf.polygon<-SpatialPolygonsDataFrame(commusp.polygon,
                                            data=data.frame(row.names=commuid, community1))
  
  
  output$tot_density <- renderLeaflet({
    pal <- colorNumeric(
      palette = "Blues",
      domain = commudf.polygon$Density
    )
    
    leaflet()%>%
      addTiles() %>%
      addPolygons(data = commudf.polygon,
                  fillColor = ~pal(Density),
                  ##color = "#5297A8", 
                  fillOpacity = 0.8, 
                  weight = 1, 
                  smoothFactor = 1)%>%
      addLegend(pal = pal, 
                values = commudf.polygon$Density, 
                position = "bottomright", 
                title = "Density of Homeless People by District")
  }) 

  
  ###Crime density
  
  data_community = data_census17_tract %>%
    group_by(CD) %>%
    summarise(total = sum(totPeople))
  
  cd_crime = data_crime %>%
    left_join(data_census17_tract,
              by = c("CT10" = "tract")) %>%
    group_by(CD) %>%
    summarise(Number = n()) %>%
    left_join(data_community,
              by = "CD") %>%
    mutate(density = Number/total)
  
  cd_crime$CD = as.character(cd_crime$CD)
  crime_combine = left_join(community_map, cd_crime,
                            by = c("id" = "CD"))
  
  crime1 = distinct(crime_combine, id, density)
  crimeid = crime1$id
  
  crimepolygons<-lapply(crimeid, function(x) polyFunc(x, dat=crime_combine))
  
  crimesp.polygon<-SpatialPolygons(crimepolygons)
  crimedf.polygon<-SpatialPolygonsDataFrame(crimesp.polygon,
                                            data=data.frame(row.names=crimeid, crime1))
  
  pal <- colorNumeric(
    palette = "Blues",
    domain = crimedf.polygon$density
  )
  
  output$crimedensity<- renderLeaflet({
    leaflet()%>%
      addTiles() %>%
      addPolygons(data = crimedf.polygon,
                  fillColor = ~pal(density),
                  color = "#5297A8", 
                  fillOpacity = 0.8, 
                  weight = 1, 
                  smoothFactor = 1)%>%
      addLegend(pal = pal, 
                values = crimedf.polygon$density, 
                position = "bottomright", 
                title = "Density of Crime by District")
    
  }) 
  
  
  ### severity
  unsheltered2015 = homeless_count_2015%>%
    dplyr::select(City,X2015_Total,Sheltered,SQMI)%>%
    mutate(unsheltered = X2015_Total - Sheltered,
           perc_unsheltered = ifelse(X2015_Total<=0,0,unsheltered/X2015_Total))%>%
    group_by(City)%>%
    summarise("unsheltered_perc_15" = mean(perc_unsheltered),
              "unsheltered_num_15" = mean(unsheltered))%>%
    arrange(desc(unsheltered_perc_15))
  
  ## YEAR 2016 showing %-unsheltered in each city
  unsheltered2016 = homeless_count_2016%>%
    dplyr::select(City,totUnsheltPeople,totPeople)%>%
    mutate(perc_unsheltered = ifelse(totPeople<=0,0,totUnsheltPeople/totPeople))%>%
    group_by(City)%>%
    summarise("unsheltered_perc_16" = mean(perc_unsheltered),
              "unsheltered_num_16" = mean(totUnsheltPeople))%>%
    arrange(desc(unsheltered_perc_16))
  
  ## YEAR 2017 %-unsheltered in each city
  unsheltered2017 = data_census17_tract%>%
    dplyr::select(City,totUnsheltPeople,totPeople)%>%
    mutate(perc_unsheltered = ifelse(totPeople<=0,0,totUnsheltPeople/totPeople))%>%
    group_by(City)%>%
    summarise("unsheltered_perc_17" = mean(perc_unsheltered),
              "unsheltered_num_17" = mean(totUnsheltPeople))%>%
    arrange(desc(unsheltered_perc_17))
  
  ## 3 YEAR Percent unsheltered people in each city
  ##        Number of unsheltered people in each city
  ##        Number of shelters in each city
  "unsheltered15-16" = left_join(unsheltered2016,unsheltered2015,by = "City")
  "unsheltered15-17" = left_join(`unsheltered15-16`,unsheltered2017,by ="City")%>%
   dplyr:: select(City = City,"2015_perc" = unsheltered_perc_15,
           "2015_num" = unsheltered_num_15,
           "2016_perc" = unsheltered_perc_16,
           "2016_num" = unsheltered_num_16,
           "2017_perc" = unsheltered_perc_17,
           "2017_num" = unsheltered_num_17)
  
  shelter_count = data_shelter %>%
    group_by(CITY)%>%
    summarise(num_shelter = n())
  `unsheltered15-17` = full_join(`unsheltered15-17`,shelter_count, by = c("City"="CITY"))
  
  ## 3 Year Unsheltered Severity = percent unsheltered x number of unsheltered people
  unsheltered_severity = `unsheltered15-17`%>%
    mutate("15_severity" = `2015_perc`*`2015_num`,
           "16_severity" = `2016_perc`*`2016_num`,
           "17_severity" = `2017_perc`*`2017_num`,
           changeInSeverity = ifelse(is.na(`15_severity`),
                                     `17_severity`-`16_severity`,
                                     `17_severity`-`15_severity`))%>%
    dplyr:: select(City,`15_severity`,`16_severity`,`17_severity`,changeInSeverity)%>%
    arrange(changeInSeverity)
  
  severityIncTop10 = unsheltered_severity%>%
    arrange(changeInSeverity)%>%
    top_n(10)
  
  output$severetop<- renderPlot({
    
    ggplot(severityIncTop10,aes(x=reorder(City,changeInSeverity),
                                y=changeInSeverity))+
      geom_col(stat = "identity",fill = "lightblue")+coord_flip()+
      geom_text(aes(label=round(changeInSeverity,digits = 0)),position = position_nudge(x=0,y=5))+
      ylab("Increase in Unsheltered Severity")+guides(fill = FALSE)+
      xlab("City")+
      ggtitle("Top 10 Cities with Greateset INCREASE in Unshltered Severity")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background= element_blank(), axis.line = element_line(colour = "black"))+
      theme(plot.title = element_text(face = "bold",size = 15))+
      theme(axis.title = element_text(size = 14))
    
  }) 
  
  output$severebottom<- renderPlot({
    
    severityDecTop10 = unsheltered_severity%>%
      arrange(changeInSeverity)%>%
      slice(1:10)%>%
      mutate(decrease = changeInSeverity*-1)
    
    ggplot(severityDecTop10, aes(x=reorder(City,decrease),y=decrease))+
      geom_col(stats = "identity",fill = "lightblue")+coord_flip()+
      geom_text(aes(label=round(changeInSeverity,digits = 0)),position = position_nudge(x=0,y=1.5))+
      ylab("Decrease in Unsheltered Severity")+guides(fill=FALSE)+
      xlab("City")+
      ggtitle("Top 10 Cities with Greatest DECREASE in Unsheltered Severity")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background= element_blank(), axis.line = element_line(colour = "black"))+
      theme(plot.title = element_text(face = "bold",size = 14))+
      theme(axis.title = element_text(size = 14))
    
  })
  
  ####severity map
  severityDecTop10 = unsheltered_severity%>%
    arrange(changeInSeverity)%>%
    slice(1:10)%>%
    mutate(decrease = changeInSeverity*-1)
  
  ## getting lon and lat for Top DEC& INC Cities
  severityDecTop10$City = paste(severityDecTop10$City,",CA")
  severityDecTop10$City = str_replace(severityDecTop10$City,"Unincorporated","")
  SeverityDecCoords = geocode(severityDecTop10$City)
  SeverityDecCoords = SeverityDecCoords%>%
    mutate(City = severityDecTop10$City)%>%
    mutate(Rank = c(1:10))
  severityDecTop10 = full_join(severityDecTop10,SeverityDecCoords,by = "City")
  
  
  
  #### GRAPH ## Spatial Map of Top DEC Cities
  
  output$mymap <- renderPlot({
    LAMap = get_map("Malibu",zoom = 8,maptype = "terrain")
    ggmap(LAMap)+
      geom_point(data = severityDecTop10,
                 aes(x=lon,y=lat,size = severityDecTop10$decrease*40,
                     color = "green"))+
      geom_text(data = severityDecTop10,aes(x=lon,y=lat+0.05,label=Rank,size = 100))+
      scale_color_manual(values = "darkgreen")+
      theme(legend.position = "none")+
      ggtitle("Location of Top Cities with Greatest Decrease in Unsheltered Severity")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background= element_blank(), axis.line = element_line(colour = "black"))+
      theme(plot.title = element_text(face = "bold",size = 12))+
      theme(axis.title = element_text(size = 14))
  })
  
  #### GRAPH ## Spatial Map of Top INC Cities
  severityIncTop10 = unsheltered_severity%>%
    arrange(changeInSeverity)%>%
    top_n(10)
  
  
  severityIncTop10$City = paste(severityIncTop10$City,",CA")
  severityIncTop10$City = str_replace(severityIncTop10$City,"Unincorporated","")
  SeverityIncCoords = geocode(severityIncTop10$City)
  severityIncCoords = SeverityIncCoords%>%
    mutate(City = severityIncTop10$City)%>%
    mutate(Rank = c(10:1))
  severityIncTop10 = full_join(severityIncTop10,severityIncCoords,by = "City") 
  
  output$decmap<- renderPlot({
    LAMap_zoom = get_map("Santa Monica",zoom = 9,maptype = "terrain")
    ggmap(LAMap_zoom)+
      geom_point(data = severityIncTop10,
                 aes(x=lon,y=lat,size = severityIncTop10$changeInSeverity*30,color = "red"))+
      geom_text(data = severityIncTop10,aes(x=lon,y=lat,label=City,size = 50))+
      scale_color_manual(values = "darkred")+
      theme(legend.position = "none")+
      ggtitle("Location of Top Cities with Greatest Increase in Unsheltered Severity")+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background= element_blank(), axis.line = element_line(colour = "black"))+
      theme(plot.title = element_text(face = "bold",size = 12))+
      theme(axis.title = element_text(size = 14))
  })
  
  
  
output$monthly_crime <-  renderPlot({

  renderTable({
  data_crime$DATE.OCCURRED = mdy(data_crime$DATE.OCCURRED)
  data_crime$month = str_sub(data_crime$DATE.OCCURRED, start=3,end = 7)
  monthcrime = data_crime%>%
    group_by(month)%>%
    summarise(number = n())
})

  
    ggplot(monthcrime, aes(x = as.factor(month), y = number, group = 1))+
    geom_point()+
    geom_line()+
    theme_bw()+
    ggtitle("The number of crimes reported in 2016 - 2017")+
    xlab("Month")+
    ylab("Number")
})
  
  }
  
  
  


shinyApp(ui,server)







