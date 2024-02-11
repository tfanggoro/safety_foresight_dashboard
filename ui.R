library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(htmltools)
library(bslib)

# Define UI for application that draws a histogram
dashboardPage(
  #--------------Skin Color--------------     
  skin = "blue",
  
  #--------------Header--------------   
  dashboardHeader(
    title = "SAFETY FORESIGHT"
  ),
  
  #--------------Sidebar--------------   
  dashboardSidebar(
    
    #Menambahkan menu pada Sidebar
    sidebarMenu(
      # Tab menu ke-1
      menuItem("Main Page", tabName = "hal1", icon = icon("home")),
      
      # Tab menu ke-2
      menuItem("Incident Recap", tabName = "hal2", icon = icon("circle-down")),
      
      # Tab menu ke-3
      menuItem("Incident Forecasting", tabName = "hal3", icon = icon("cloud")),
      
      # Tab menu ke-4
      menuItem("About Me", tabName = "hal4")
    )
  ),
  
  
  #--------------Body-------------- 
  dashboardBody(
    tabItems(
      
      # Halaman Pertama
      tabItem(
        tabName = "hal1",
        
        #--------------Row 1 Page 1-------------- 
        fluidRow(
          box(width = 4,
              img(src = "images_port.jpeg", height = 275, width = 320)),
          box(width = 8,
          h2(tags$b("SAFETY FORESIGHT")),
          br(),
          div(style = "text-align:justify", 
              p("Jakarta Port stands as a pivotal hub for maritime operations, 
                facilitating crucial activities vital to regional commerce. 
                Despite experiencing modest growth of 0.65 percent in the flow of domestic containers 
                compared to the 7.62 million TEUs recorded in 2021, 
                recent activity surges have sparked concerns regarding potential increases in incidents.
                
                Harnessing the power of historical data, 
                we can delve into trends and patterns to navigate this challenge effectively. 
                By analyzing past occurrences, 
                we can equip the Safety Team with invaluable insights, 
                empowering them to craft robust mitigation plans to prevent both incident occurrence and recurrence.
                
                Moreover, these insights serve as the foundation for constructing predictive models, 
                enabling us to forecast the likelihood of future incidents. 
                Through proactive measures informed by data-driven foresight, 
                we can bolster operational resilience and ensure 
                the continued safety and efficiency of Jakarta Port's maritime activities.")
              )
          )
        ),
        fluidRow(
            box(width = 4,
                 
            #infobox ke-1
            infoBox(
              "Fatality",
              width = 12,
              icon = icon("person-falling-burst"),
              value = length(fatality$Tipe_Insiden),
              color = "black"),
            
            #infobox ke-2
            infoBox(
              "Dangerous Occurrence",
              width = 12,
              icon = icon("person-burst"),
              value = length(danger$Tipe_Insiden),
              color = "red"),
            
            #infobox ke-3
            infoBox(
              "First Aid / Injury",
              width = 12,
              icon = icon("user-injured"),
              value = length(injury$Tipe_Insiden),
              color = "orange"),
            
            #infobox ke-4
            infoBox(
              "Near Miss",
              width = 12,
              icon = icon("person-circle-exclamation"),
              value = length(nearmiss$Tipe_Insiden),
              color = "green")),
            
            box(
              width = 8,
              plotlyOutput(outputId = "plot_eda1")))),
      
    
      # Halaman Kedua
      tabItem(
        tabName = "hal2",
        fluidRow(
          box(
            width = 12,
            h3(tags$b("INCIDENT RECAP")),
            div(style = "text-align:justify", 
                p("Here, we examine monthly incident patterns for a more granular understanding of whether specific months consistently exhibit higher or lower incident rates.
              After that, we analyze incident patterns at the hourly level to reach a detailed examination of when these incidents are most likely to occur throughout the day.
              Next, we explore incidents based on type, this is a crucial step to gaining a comprehensive understanding of the nature and characteristics of the incidents.
              Lastly, we would like to analyze our reported incidents categorized by type comprehensively. 
              We aim to discern the frequently used words within the manual reports associated with each incident type. 
              These explorations will provide valuable insights into the specific language and key terms prevalent in our incident documentation, helping us enhance our understanding and response strategies.")
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            navset_tab(
              nav_panel(title = "2019",
                        plotlyOutput(outputId = "plot_eda3")),
              nav_panel(title = "2020", 
                        plotlyOutput(outputId = "plot_eda4")),
              nav_panel(title = "2021",
                        plotlyOutput(outputId = "plot_eda5")),
              nav_panel(title = "2022",
                        plotlyOutput(outputId = "plot_eda6")),
              nav_panel(title = "2023", 
                        plotlyOutput(outputId = "plot_eda7"))
            )
          ),
          box(
            width = 6,
            navset_tab(
              nav_panel(title = "2019 - 2023",
                        plotlyOutput(outputId = "plot_eda_hour1")),
            )
          ),
          box(
            width = 6,
            navset_tab(
              nav_panel(title = "2019",
                        plotlyOutput(outputId = "plot_eda8")),
              nav_panel(title = "2020",
                        plotlyOutput(outputId = "plot_eda9")),
              nav_panel(title = "2021",
                        plotlyOutput(outputId = "plot_eda10")),
              nav_panel(title = "2022",
                        plotlyOutput(outputId = "plot_eda11")),
              nav_panel(title = "2023",
                        plotlyOutput(outputId = "plot_eda12"))
            )
          ),
          box(
            width = 6,
            navset_tab(
              nav_panel(title = "Fatality",
                        plotOutput(outputId = "wordcloud1")),
              nav_panel(title = "Dangerous Occurrence", 
                        plotOutput(outputId = "wordcloud2")),
              nav_panel(title = "First Aid / Injury",
                        plotOutput(outputId = "wordcloud3")),
              nav_panel(title = "Near Miss",
                        plotOutput(outputId = "wordcloud4"))
            )
          )
        )
      ),
        
      # Halaman Kedua
      tabItem(
        tabName = "hal3",
        
        #--------------Row 1 Page 2-------------- 
        fluidRow(
          box(
              width = 12,
              h3(tags$b("INCIDENT FORECASTING")),
              div(style = "text-align:justify", 
              p("Welcome to our state-of-the-art forecasting model, meticulously crafted to help you predict future incidents with unparalleled accuracy and dependability. 
              By harnessing advanced algorithms, this intuitive tool empowers you to anticipate incident trends over a timeframe tailored to your needs.
              Our Time Series Forecasting Model is engineered to forecast incidents over a span of 5 years, equivalent to 60 months, 
              allowing you to gain invaluable insights into future occurrences. Through a meticulous analysis of historical data and prevailing trends, 
              our sophisticated system generates a comprehensive forecast, providing clarity on the expected incidents. 
              Lets go find check at what time you would like us to forecast!"))
              ),
          
        
            box(
              width = 3,
              selectInput(
                "input_sel",
                label = "Select Month",
                choices = forecast_m$ds[61:120],
                selected = forecast_m$ds[61]),
              
              uiOutput("date_forecast")),
            
            box(
            width = 4,
            uiOutput("forecast_number")
            ),
          
            box(
             width = 5,
             uiOutput("alert_text")
            ),
            
            box(
              width = 12,
              plotlyOutput(outputId = "plot_act_pred")
            )
          ),
        ),
      
      # Halaman Empat
      tabItem(
        tabName = "hal4",
        fluidRow(
          box(
            width = 12,
            h2(tags$b("TAUFAN ANGGORO ADHI")),
            div(style = "text-align:justify", 
                p("Passionate HSE Administrator | Committed to Health, Safety, and Environment Excellence | Dedicated to Ensuring a Safe Workplace Environment "),
                p("Data Analyst & Science Enthusiast: Leveraging Insights for Strategic Decision-Making")
                )
            ),
          infoBox(
            "LINKEDIN",
            width = 3,
            icon = icon("linkedin"),
            href = "https://www.linkedin.com/in/taufanadhi/",
            color = "blue"),
          infoBox(
            "GITHUB",
            width = 3,
            icon = icon("github"),
            href = "https://github.com/tfanggoro",
            color = "black"),
          infoBox(
            "PUBLIC TABLEAU",
            width = 3,
            icon = icon("chart-column"),
            href = "https://public.tableau.com/app/profile/taufan.anggoro.adhi/vizzes",
            color = "red"),
          infoBox(
            "INSTAGRAM",
            width = 3,
            icon = icon("instagram"),
            href = "https://www.instagram.com/taufanadhi/",
            color = "fuchsia")
          )
        )
        )
      )
   )
