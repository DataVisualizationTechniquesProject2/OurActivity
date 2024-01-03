library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(dplyr)
library(fresh)
library(lubridate)
library(tidyverse)
library(viridis)
library(htmltools)
library(scales)
library(leaflet)
library(geosphere)
library(htmlwidgets)

#setwd("C:\\Users\\macie\\Desktop\\STUDIA\\SEMESTR3\\Techniki Wizualizacji Danych\\PROJEKTY\\Project2\\MM")
activities_Ola <- read.csv("activities_Ola.csv")
ActivitiesTogether <- read.csv("ActivitiesTogether_Maciek.csv")
ActivitiesTogether$startTime <- as.POSIXct(ActivitiesTogether$startTime, format = "%Y-%m-%d %H:%M:%S")
ActivitiesTogether$Rok <- format(ActivitiesTogether$startTime, "%Y")
ActivitiesTogether$RokAndMonth <- format(ActivitiesTogether$startTime, "%Y-%m")
ActivitiesTogether$Rok <- as.numeric(ActivitiesTogether$Rok)
ActivitiesTogether$Day <- wday(ActivitiesTogether$startTime, week_start = 1)
ActivitiesTogether$Month <- month(ActivitiesTogether$startTime)
zmienne = c("Distance (km)", "Time (minutes)","Average Speed (km/h)")
zmienne_heat <- c("Number of records", "Distance (km)", "Time (minutes)")
Kroki_Calorie <- read.csv("Kroki_Kalorie_Maciek.csv")
HeartRate_Maciek <- read.csv("HeartRate_Maciek.csv")
HeartRate_Maciek$recordDay <- as.POSIXct(HeartRate_Maciek$recordDay)

# Create the theme
mytheme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "200px",
    dark_bg = "#D8DEE9",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#FFF",
    box_bg = "#D8DEE9", 
    info_box_bg = "#D8DEE9"
  )
)


# HEADER

# dodalam obrazek ale niestety go ucinai jest w złym kolorze i na razie nie wiem jak to porawic, najwyzej z niego zrezygnujemy
# czcionka i kolor to tylko tak na próbe, sprawdzałam jak to mniej więcej działa z tym HTML i CSS

header <- dashboardHeader(
  title = tags$div(style = "color: white; font-size: 25px; font-family: 'Arial, sans-serif';", 
                   HTML('<img src="https://pixy.org/src/38/384909.png" style="height:30px; margin-right:10px;">'),
                   "Our Bike Activity"))

# SIDEBAR

sidebar <- dashboardSidebar(
  
  width = 700,
  
  sidebarMenu(
  menuItem("TOP 5", tabName = "top5", icon = icon("map")),
  menuItem("Short/Medium/Long", tabName = "sml", icon = icon("chart-simple")),
  menuItem("Competition", tabName = "competition", icon = icon("trophy")),
  menuItem("Individual Statistics", tabName = "individualstat", icon = icon('chart-line'), startExpanded = F,
            menuSubItem('Maciek', tabName = "individualMaciek", icon = icon('m')),
            menuSubItem('Kuba', tabName = "individualKuba", icon = icon("k")),
            menuSubItem('Ola', tabName = "individualOla", icon = icon("o"))
           )
  )
  
)

# BODY

body <- dashboardBody(
  
  use_theme(mytheme),
  
  tabItems(
    
    ### Kuba ###
    
       tabItem(tabName = "top5",
           
            fluidRow(
              box(title = "Top 5 tracks",
                  textOutput("top5description")),
              box(sidebarMenu(id = "sidebarmenuTop5",
                              selectInput(
                                inputId = "track",
                                label = "Choose person:",
                                choices = c("Kuba", "Maciek", "Ola"))
                              
              ),
              sidebarMenu(id="sidebarmenuTrack",
                          selectInput(
                            inputId = "number_of_track",
                            label = "Choose interesting track:",
                            choices = c("1","2","3","4","5")))
              )),
            fluidRow(
              box(leafletOutput("mapOfTrack")),
              box(title="Track description",
                  textOutput("descriptionOfTrack"),width = 6)
            ),
            
    ),
    tabItem(tabName = "individualKuba"),
    
    
    ### Ola ###
    
    tabItem(tabName = "sml",
            
            fluidRow(
              box(title = "Small/Medium/Long Tracks",
                  textOutput("smldescription")),
              sidebarMenu(id = "sidebarmenu",
                          selectInput(
                            inputId = "person",
                            label = "Choose person:",
                            choices = c("Kuba", "Maciek", "Ola"))
            )),
            
            fluidRow(
              box(plotOutput("hisPlot"), width = 6),
              
              box(sliderInput("zakres1",
                                 "Choose the range of years",
                                 value = c(2020, 2023),
                                 min = 2020,
                                 max = 2023,
                                 step = 1),
                  textOutput("smlchart1"),
                  width = 6
                     )
            ),
            
            fluidRow(
              box(width = 6),
              
              box(plotOutput("violinPlot"), width = 6)
              ),
            
            fluidRow(
              box(plotOutput("densityPlot"), width = 6),
              
              box(width = 6)
            )
    ),
    
    tabItem(tabName = "individualOla",
            
            fluidRow(
              box(title = "Individual Statistics Ola")
              
            )
            
            ),
    
    ### Maciek ###
    
    tabItem(tabName = "competition",
            fluidRow(
              box(
                title = "Competiton",
                textOutput("competitiondescribtion")
              ),
              box(
                selectInput("zmienna",
                            "For which variable do you want to summarize?",
                            zmienne,
                           ),
                sliderInput("zakres",
                            "Choose the range of years",
                            value = c(min(2020), max(ActivitiesTogether$Rok)),
                            min = 2020,
                            max = max(ActivitiesTogether$Rok),
                            step = 1),
                width = 6
                 ),
            ),
            
            fluidRow(
              box(
                title = textOutput("competition1PlotTitle"),
                plotlyOutput("competition1"),
                width = 6
              ),
              box(
                textOutput("competitionInteractivityDesc"),
                width=6
              )
            ),
            
            fluidRow(
              box(
               width=6 
              ),
              box(
                plotlyOutput("competition2")
              )
            )
    ),
    
    tabItem(tabName = "individualMaciek",
            fluidRow(
              box(
                title = textOutput("individualMaciekdescribtion")
              )
            ),
            fluidRow(
              box(
                title = textOutput("individualMaciek1Desc"),
                width = 3
              ),
              box(
                title = "Corelation between steps and calories",
                numericInput("limitKcal", "Type in calories limit", value=max(Kroki_Calorie$Kalorie)),
                plotlyOutput("individualMaciek1"),
                width = 9
              )
            ),
            fluidRow(
              box(
                title = "In which month do I walk the most?",
                plotlyOutput("individualMaciek2"),
                width = 6
              ),
              box(
                title = textOutput("individualMaciek2Desc"),
                width = 6
              )
            ),
            fluidRow(
              box(title = textOutput("individualMaciek3Desc"),
                  width = 12
                  )
            ),
            fluidRow(
              box(title = "Average heart rate during exercising per month",
                  plotlyOutput("individualMaciek3"),
                  width = 12
                  )
            ),
            fluidRow(
              box(           
                selectInput("zmienna_heat",
                            "Choose the variable to analyse",
                            zmienne_heat),
                width = 2),
              box(title = textOutput("individualMaciek4PlotTitle"),
                  plotlyOutput("individualMaciek4"),
                  width = 5),
              box(title = textOutput("individualMaciek5PlotTitle"),
                  plotlyOutput("individualMaciek5"),
                  width = 5)
            )
    )
    
  
  
  ## STYLES
  
  # tags$head(
  #     # # HTML('.main-sidebar { width: 250px; }'),
  #     # # HTML('.content-wrapper, .right-side { margin-left: 250px; }'),
  #     # HTML('.main-header .logo { width: 300px }'),
  #     # HTML('main-header { width: 400px }')
  # )
  
  )
)
dashboardPage(header, sidebar, body)

ui <- dashboardPage(header, sidebar, body)




server <- function(input, output) {

    output$hisPlot <- renderPlot({
      
      ### Ola ###
      
      activities_Ola$Type <- factor(activities_Ola$Type, levels = c("Short", "Medium", "Long")) 
      
      activities_Ola %>% 
        filter(Osoba == input$person) %>% 
        group_by(Type) %>% 
        summarise(n = n()) -> summarizeData
      
      as.numeric(max(summarizeData$n)) -> max_coord 
    
      
      activities_Ola %>%
        filter(activities_Ola$Year >= input$zakres1[1],
               activities_Ola$Year <= input$zakres1[2]) %>% 
        filter(Osoba == input$person)-> activities_plot

      
      ggplot(activities_plot, aes(x = Type, fill = Type)) +
      geom_bar() +
      scale_fill_manual(values = c("chartreuse4", "dodgerblue2", "brown2")) +
      labs(title = "Number of tracks",
           x = "Type") +
      theme_minimal()+
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
              axis.title.y = element_blank()) +
      coord_cartesian(ylim = c(0, max_coord))
        

    })
    
    
    
    output$violinPlot <- renderPlot({
      
      activities_Ola$Type <- factor(activities_Ola$Type, levels = c("Short", "Medium", "Long"))
      
      activities_Ola %>%
        filter(Osoba == input$person)-> activities_plot
      
      ggplot(activities_plot, aes(x = Type, y = as.numeric(AvgSpeed), fill = Type)) +
        geom_violin() +
        scale_fill_manual(values = c("chartreuse4", "dodgerblue2", "brown2")) +
        labs(title = "Distribution of average speed",
             x = "Type",
             y = "Average Speed") +
        theme_minimal()+
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
        coord_cartesian(ylim = c(0, 35))
      
      
    })
    
    # output$scatterPlot <- renderPlot({
    #   
    #   activities_Ola %>%
    #     filter(Osoba == input$person)-> activities_plot
    #   
    #   ggplot(activities_plot, aes(x = as.numeric(DurationMinutes), y = as.numeric(AvgSpeed), color = Type)) +
    #     geom_point(size = 3) +
    #     scale_fill_manual(values = c("chartreuse4", "dodgerblue2", "brown2")) +
    #     labs(title = "Duration vs Speed",
    #          x = "Duration",
    #          y = "Average Speed") +
    #     theme_minimal()+
    #     theme(plot.title = element_text(hjust = 0.5))
    #   
    # })
    
    output$densityPlot <- renderPlot({
      
      activities_Ola$Type <- factor(activities_Ola$Type, levels = c("Short", "Medium", "Long"))
      
      activities_Ola %>%
        filter(Osoba == input$person)-> activities_plot
      
      ggplot(activities_plot, aes(x = Hour, fill = Type)) +
        geom_density(alpha = 0.6) +
        scale_fill_manual(values = c("chartreuse4", "dodgerblue2", "brown2")) +
        labs(title = "Density Plot of Activity Distribution Across Hours",
             x = "Hour") +
        theme_minimal()+
        theme(legend.position = "none", plot.title = element_text(hjust = 0.5),
              axis.title.y = element_blank())
      
    })
    
    output$smldescription <- renderText({
      "Short/Medium/Long is a tab where our cycling adventure where categorized 
      according to the distance. A ride spanning 0-40 kilometers is considered 
      a short track, 40-70 kilometers falls into the medium category, while 
      anything beyond 70 kilometers is viewed as a long and adventurous expedition."
    })
    
    output$smlchart1 <- renderText({
      "Explore our track completion trends with this interactive bar chart. 
      Categories (short, long, medium) are color-coded, and above slider let 
      you focus on specific years.Thanks to this chart, we can see our 
      preferences regarding track length and how they have changed over the years."
    })
      
      ### Kuba ###
      
    output$mapOfTrack <- renderLeaflet(
      {
        mydata<-read.csv(paste("Top5_Activities\\activity_",input$track,"_",input$number_of_track,".csv",sep=""))
        mydata$ele<-mydata$speed
        hotlinePlugin <- htmltools::htmlDependency(
          name = 'Leaflet.hotline',
          version = "0.4.0",
          src = c(file = normalizePath("hotline_plugin")),
          script = "leaflet.hotline.js"
        )
        
        registerPlugin <- function( map, plugin ) {
          map$dependencies <- c( map$dependencies, list( plugin ) )
          map
        }
        palette<- colorNumeric(palette = colorRampPalette(c("#008800","#ffff00","#ff0000"))(5),
                               domain = 0:40)
        
        mydata %>% group_by(kilometer) %>% slice(tail(which.max(row_number()),1)) -> last_lon_lat_each_km
        mydata %>% group_by(kilometer) %>% summarize(mean_speed=mean(speed)) -> avg_speed_per_km
        
        coords_of_kilometers<-last_lon_lat_each_km %>% left_join(avg_speed_per_km,by="kilometer") %>% select(lat,lon,kilometer,mean_speed)
        
        paste("Kilometer: ","<strong>",coords_of_kilometers$kilometer,"</strong>","<br>Average speed: ","<strong>",round(coords_of_kilometers$mean_speed,2),"</strong>",sep="") %>%
          lapply(htmltools::HTML) -> labels
        
        leaflet() %>% addTiles() %>%
          fitBounds( min(mydata$lon), min(mydata$lat), max(mydata$lon), max(mydata$lat) ) %>%
          registerPlugin(hotlinePlugin) %>%
          onRender("function(el, x, data) {
            data = HTMLWidgets.dataframeToD3(data);
            data = data.map(function(val) { return [val.lat, val.lon, val.ele]; });
            L.hotline(data, {min: 0, max: 40}).addTo(this);
          }", data = mydata ) %>% addLegend("bottomright",pal=palette,values=0:40,opacity=1,title="Speed [km/h]") %>% 
          addCircleMarkers(data=coords_of_kilometers,lat=~lat,lng=~lon,label=~labels,fillColor="blue",fillOpacity=1,stroke=F,radius=3)
        
      }
    )
    output$top5description <- renderText({
      "Explore our most interesting tracks. The map consists track route, which is coloured in various colors.
      Each color represents respective speed. Shades of green are used for presenting lower speeds, such as 0 km/h or 10 km/h. 
      Yellow and green color mixtures represent values of speed in range from 10 km/h to 20km/h. Yellow color corresponds to the value of 20km/h,
      red stands for every speed that is equal to 40 km/h or greater than it. Shades of mixed yellow and red are for values between 20 km/h and 40 km/h."
    })
    
    output$descriptionOfTrack <- renderText({
      text_Kuba_1<-"Monday, 5th of September 2022 at first glance did not seem to become a day, when I set my longest bike trip.  
        The starting time of the ride did not indicate that it would be one of the most amazing trips. 
        I messaged my friend Michał, and we met each other on the Most Południowy at 1:00 PM. We planned to reach our beloved Góra Kalwaria and then decide what to do next. 
        Everything went as planned, we also had a decent average speed, the weather was fantastic, so we decided to extend our ride and go to Warka. 
        We have been there quite a few times, but the road to Warka is always really demanding. There are a lot of trucks, so you have to keep the pace and be really cautious. 
        It wasn’t easy at all, but we have made to the city. We stayed there for a several minutes, chilled a little bit, and then started coming back. The route to Warsaw was completed without any major obstacles. 
        We had a really good average speed, I was feeling that my muscles aren’t that tired, so I have decided to extend the ride in Warsaw. 
        I managed to do extra 50 kilometres, I was extremely exhausted, but also very happy of this achievement."
      
      text_Kuba_2<-"I have been planning this trip with my bike comrade Michał for a year. 
       We couldn’t find the time for it, but finally on the last day of summer holidays, on 31.08.2021 we managed to realize our “dream”. Until we reached Warka, we didn’t have much difficulties. 
       The troubles began after leaving the city. We did not know the route perfectly and even with GPS we had difficulties. We landed in forest two times, the road surface was terrible, we were moving with really low speed. 
       But in the end, we reached our destination. We didn’t have time to stay there, as the departure time of train was near."
      
      text_Kuba_3<-"The route to Warka on June 22nd 2022 was another trip to our liked place. 
        We started cycling around 10:00 AM. The weather was quite bearable, there was around 22°C. The trip was mostly peaceful. We did not have any troubles and difficulties. 
        We stayed and chilled in Warka for some time and managed to comeback to Warsaw without any unexpected adventures. In the meantime we had a stop at our favourite grocery shop in Góra Kalwaria. 
        We recharged our batteries and rode with quite good pace to our final destination."
      
      text_Kuba_4<-"This trip was the only one from considered routes, that I rode alone. It was just an ordinary trip do Góra Kalwaria. However I did it on Sunday, which was actually quite surprising, because I usually don’t go cycling on that day. 
        I felt power on that day, as the average speed shows –  26,2 km/h. I did the first part in less than hour, I felt huge exhaust, so the second part was much more difficult. 
        However I managed to return to home with a lot of effort, but the prize waiting in home – lying on the bed and resting – was infusing hope and peace."
      
      text_Kuba_5<-"The last trip is the oldest one – from 29 June 2021. It was not filled with high speed, however it was quite interesting. 
       Me and my two friends decided to visit Zalew Zegrzyński. We rode through Jabłonna and Legionowo quite peacefully. Then we reached Nieporęt, our expected destination. 
       Even though we did not have bath costumes, we have decided to splash a little. It was really hot, so the danger of catching a cold was rather negligible. However it wasn’t the end of bathing. 
       Our friend’s girlfriend called him and asked whether him whether we would like to join her in Zielonka. We were really enthusiastic about it and quickly rode there. We had quite a lot of fun there too and then we finally split up and ended in our homes. I managed to do 100 km, so that’s a lovely result. 
       Great fun, which was there will certainly stay in my heart for a long time."
      
      text_Maciek_1<-"Aaas"
      text_Maciek_2<-"Bb"
      text_Maciek_3<-"Cc"
      text_Maciek_4<-"Dd"
      text_Maciek_5<-"Ee"
      
      text_Ola_1<-": On September 2nd, I decided to go for a bike ride along one of my favourite routes, known as the \"around the chimneys\" trail. 
        No, it doesn't mean that I passed by chimneys along the way. In my family, we refer to routes that lead through villages near our town as \"around the chimneys\" trails. 
        The route mainly follows side roads, so there isn't too much traffic on it, which makes this route really nice."
      
      text_Ola_2<-"A quick trip to the Wisła, unfortunately, this time without a break for splashing in the water. 
        This route is a very pleasant option for a slightly longer excursion. On the morning of August 13th, I took this route with my dad – a huge cycling enthusiast. 
        It was a very successful outing. Unfortunately, this route has one drawback – crossing a very busy street, which can be challenging and sometimes dangerous."
      
      text_Ola_3<-"A quick ride to Kołbiel with a satisfying average speed of 27.6 km/h. On the way to Kołbiel, 
        I rode along the express road S17, which wasn't the most enjoyable due to the noise from the busy street. However, on the way back home, 
        I took the route through the surrounding villages, which was significantly more pleasant."
      
      text_Ola_4<-"On July 23rd, I embarked on another cycling adventure, heading to Parysów, a beautiful town with a charming market square. 
        Just before entering the town, there's a steep hill, which climbing up is always a huge challenge. Afterward, I headed towards Michałówka. 
        Initially, I planned to return through Osieck. Unfortunately, it was a very hot July morning, and I could feel strength in my legs fading. 
        In Huta, I decided to shorten my route by a few kilometres and head back home."
      
      text_Ola_5<-"The cycling trip from June 11th is one of my favourites. Along the way, I encountered a group of cyclists who suggested that I join them. I gladly agreed. 
        Riding in a peloton is a completely different experience. Following behind other cyclists is much easier, and I don't tire as quickly. 
        The new friends turned out to be a very cool group of people. I hope we'll catch up on the road again!"
      
      eval(parse(text=paste("text",input$track,input$number_of_track,sep="_")))
    })
      
      
      ### Maciek ###
      output$competition1 <- renderPlotly({
        if(input$zmienna == "Distance (km)"){
          m = "Dystans"
          output$competition1PlotTitle <- renderText({
            "Total distance over the years"
          })
          df <- ActivitiesTogether %>% filter(Rok >= input$zakres[1],
                                              Rok <= input$zakres[2]) %>%
            group_by(Osoba) %>% 
            summarize(score = sum(!!sym(m)))
          yaxisRange = c(0,15000)
        }
        if(input$zmienna == "Time (minutes)"){
          m = "Czas"
          output$competition1PlotTitle <- renderText({
            "How much time have we spent cycling over the years (in minutes)"
          })
          df <- ActivitiesTogether %>% filter(Rok >= input$zakres[1],
                                              Rok <= input$zakres[2]) %>%
            group_by(Osoba) %>% 
            summarize(score = sum(!!sym(m)))
          yaxisRange = c(0,25000)
        }
        if(input$zmienna == "Average Speed (km/h)"){
          m = "ŚredniaPrędkość"
          output$competition1PlotTitle <- renderText({
            "Our average speed across all rides"
          })
          df <- ActivitiesTogether %>% filter(Rok >= input$zakres[1],
                                              Rok <= input$zakres[2]) %>%
            group_by(Osoba) %>% 
            summarize(score = mean(!!sym(m)))
          yaxisRange = c(0,50)
        }
      
      plot_ly(df,x=~Osoba,y=~score,color=~Osoba, type = "bar") %>% 
        layout(xaxis = list(title = "Person"), yaxis = list(title = input$zmienna,tickformat = ",d",range=yaxisRange))
      })
      
      output$competitiondescribtion <- renderText({
        "Competition tab is where our activity is summarised. Here we compare 
        ourselves depending on total distance, average speed or time
        spent on cycling in minutes."
      })
      
      output$competitionInteractivityDesc <- renderText({
        "Explore our scores with those two charts. The bar chart consists summarized score in
        selected period of time while linear chart shows the progress throughout years. You can easily change 
        considering topic on the top of this text and select years with slider. (Note that slider doesn't affect 
        linear chart. Otherwise this chart would make no sense)"
        
      })
      #competition2
      output$competition2 <- renderPlotly({
        if(input$zmienna == "Distance (km)"){
          m = "Dystans"
          df <- ActivitiesTogether %>% filter(Rok >=2020) %>%
            group_by(Osoba,Rok) %>% 
            summarize(score = sum(!!sym(m)))
        }
        if(input$zmienna == "Time (minutes)"){
          m = "Czas"
          df <- ActivitiesTogether %>% filter(Rok >=2020) %>%
            group_by(Osoba,Rok) %>% 
            summarize(score = sum(!!sym(m)))
        }
        if(input$zmienna == "Average Speed (km/h)"){
          m = "ŚredniaPrędkość"
          df <- ActivitiesTogether %>% filter(Rok >=2020) %>%
            group_by(Osoba,Rok) %>% 
            summarize(score = mean(!!sym(m)))
        }
        plot_ly(df,
                x=~Rok,
                y=~score,
                type="scatter",
                mode="lines",
                color = ~Osoba
        ) %>% layout(
          yaxis = list(title = input$zmienna),
          xaxis = list(title = "Year",tickmode = "array", tickvals = unique(df$Rok), 
                       ticktext = unique(df$Rok))
        )
      })
      
      output$individualMaciek1 <- renderPlotly({
        
        ggplotly(Kroki_Calorie  %>% filter(Kalorie <= input$limitKcal) %>%  ggplot(aes(x=Kroki,y=Kalorie)) + stat_density2d(geom="tile", aes(fill = after_stat(density)), contour = FALSE) +
          geom_point(colour = "white") + theme_minimal() +
          geom_smooth() +  
          theme(legend.position = "none") + labs(x = "Steps", y= "Calories (kcal)")) %>%
          layout(xaxis = list(rangeslider = list(type = "Kroki")))
      })
      
      output$individualMaciekdescribtion <- renderText({
        "Describtion of this section"
      })
      output$individualMaciek1Desc <- renderText({
        "Let's look at the chart on the right. You can manipulate the x axis by slider and y axis by typing the limit.
        The chart shows how calories change depending on the amount of steps. 
        If we properly check the smaller values as Steps <= 20000 and Calories <= 500 we would see the linear dependence
        highlighted. This may suggest that those records filled around the line may be from classic walking while those 
        records which have less steps but more Calorise buried are from cycling."
      })
      
      output$individualMaciek2 <- renderPlotly({
          Kroki_Calorie %>%
          mutate(miesiac = factor(month(recordDay)), rok=factor(year(recordDay))) %>% 
          filter(rok != "2019") %>% 
          group_by(miesiac, rok) %>% summarise(n = sum(Kroki)) %>%  
          plot_ly(x=~miesiac,y=~n,type="bar", color =~ rok) %>% 
          layout(yaxis = list(title = "Score",tickformat = ".d"), barmode = "stack")
      })
      
      output$individualMaciek2Desc <- renderText({
        "Describtion of 2 chart"
      })
      
      output$individualMaciek3 <- renderPlotly({
        ggplotly(HeartRate_Maciek %>%
          mutate(mean_pulse = mean(avgPulse),rok = factor(year(recordDay))) %>% 
          filter(rok != "2019") %>% 
          ggplot(aes(x = recordDay, y = avgPulse)) + 
          scale_x_datetime(labels = date_format("%Y-%m"), breaks = date_breaks("months")) + 
          geom_line(color = "darkred") +
          geom_hline(aes(yintercept = mean_pulse, color = "Average overall"), linetype = "dashed", size = 1) +
          labs(x = "",
               y = "beats per minute",
               color = "") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)))
      })
      
      output$individualMaciek3Desc <- renderText({
        "Describtion of 3 chart"
      })
      
      output$individualMaciek4 <- renderPlotly({
        custom_colors <- c("black", "yellow")
        if(input$zmienna_heat == "Number of records"){
          output$individualMaciek4PlotTitle <- renderText({"Number of records in each month"})
          df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Maciek") %>% group_by(Osoba,Month) %>% summarise(score=n())
        }
        if(input$zmienna_heat == "Distance (km)"){
          output$individualMaciek4PlotTitle <- renderText({"Distance (km) in each month"})
          df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Maciek") %>% group_by(Osoba,Month) %>% summarise(score = sum(Dystans))
        }
        if(input$zmienna_heat == "Time (minutes)"){
          output$individualMaciek4PlotTitle <- renderText({"Time (minutes) in each month"})
          df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Maciek") %>% group_by(Osoba,Month) %>% summarise(score = sum(Czas))
        }
        plot_ly(df, 
                x=~Osoba, 
                y=~Month, 
                z=~score, 
                type="heatmap",
                colors = custom_colors,
                colorbar = list(tickformat = ".d"),
                text = ~paste("Person: ", Osoba, "<br>Month: ", Month, paste("<br>",input$zmienna_heat, ":"), score),
                hoverinfo = "text"
        ) %>% 
          layout( 
            xaxis = list(title = ""),
            yaxis = list(title="Month",tickmode = "array", tickvals = unique(df$Month), 
                         ticktext = unique(df$Month))
          )
      })
      
      output$individualMaciek5 <- renderPlotly({
        custom_colors <- c("black", "yellow")
        if(input$zmienna_heat == "Number of records"){
          output$individualMaciek5PlotTitle <- renderText({"Number of records in each day"})
          df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Maciek") %>% group_by(Osoba,Day) %>% summarise(score=n())
        }
        if(input$zmienna_heat == "Distance (km)"){
          output$individualMaciek5PlotTitle <- renderText({"Distance (km) in each day"})
          df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Maciek") %>% group_by(Osoba,Day) %>% summarise(score = sum(Dystans))
        }
        if(input$zmienna_heat == "Time (minutes)"){
          output$individualMaciek5PlotTitle <- renderText({"Time (minutes) in each day"})
          df <- ActivitiesTogether %>% filter(Rok >=2020, Osoba == "Maciek") %>% group_by(Osoba,Day) %>% summarise(score = sum(Czas))
        }
        plot_ly(df, 
                x=~Osoba, 
                y=~Day, 
                z=~score, 
                type="heatmap",
                colors = custom_colors,
                colorbar = list(tickformat = ".d"),
                text = ~paste("Person: ", Osoba, "<br>Day: ", Day, paste("<br>",input$zmienna_heat, ":"), score),
                hoverinfo = "text"
        ) %>% 
          layout( 
            xaxis = list(title = ""),
            yaxis = list(title="Day",tickmode = "array", tickvals = unique(df$Day), 
                         ticktext = unique(df$Day))
          )
      })
      
    
}

shinyApp(ui = ui, server = server)
