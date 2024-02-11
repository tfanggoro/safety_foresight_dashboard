#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  
output$plot_eda1 <- renderPlotly({
  
  #copy dataset from rawdata
  eda_1 <- rawdata %>% mutate_at(.vars = c('Hari','Tahun','Jam','Tipe_Insiden','Jenis_Alat','Lokasi','Penyebab','Akibat','Status','Shift'), as.factor)
  
  #sorting month name
  eda_1$Bulan <-  factor(eda_1$Bulan, levels = month.name)
  
  #extracting number of incident per month
  eda_1 <- eda_1 %>%   
    group_by(Tahun, Bulan) %>% 
    summarise("Total_Incident" = length(Department)) %>% 
    ungroup() %>% 
    arrange(Tahun,Bulan) %>% 
    mutate(label_1 = glue("{Bulan} {Tahun}
                         Number of Incident: {Total_Incident}"))
  
  eda_1$Date <- as.yearmon(paste(eda_1$Tahun, match(eda_1$Bulan, month.name)), "%Y %m")
  
  plot1 <- ggplot(data = eda_1, aes(x = Date, 
                                    y = Total_Incident))+
    
    geom_line(col="red")+
    geom_point(aes(text = label_1),col="black", size = 1)+
    labs(title = "Incident Trend Analysis",
         x = NULL,
         y = "Number of Incident")+
    scale_y_continuous(limits = c(0,20))+
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
  
  ggplotly(plot1, tooltip = "text")
  
})

output$plot_eda2 <- renderPlotly({
  
  eda_2 <- rawdata %>% 
    group_by(Tahun) %>% 
    summarise("Total_Incident" = length(Department)) %>% 
    ungroup() %>% 
    mutate(label_2 = glue("{Total_Incident} Incident"))
  
  plot2 <- ggplot(data = eda_2,aes(x = Tahun, 
                                   y = Total_Incident,
                                   text = label_2))+
    geom_col(fill = "cyan")+
    labs(title = "Total Number of Incident per Year",
         x = NULL,
         y = "Number of Incident")+
    scale_y_continuous(limits = c(0,125))+
    theme_minimal()+
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
  
  ggplotly(plot2, tooltip = "text")
})

output$plot_eda3 <- renderPlotly({
  
  #copy dataset from rawdata
  data_eda_3 <- rawdata %>% mutate_at(.vars = c('Hari','Jam','Tipe_Insiden','Jenis_Alat','Lokasi','Penyebab','Akibat','Status','Shift'), as.factor)
  data_eda_3 <- data_eda_3 %>% filter(Tahun == 2019)
  
  #sorting month name
  data_eda_3$Bulan <-  factor(data_eda_3$Bulan, levels = month.name)
  
  
  eda_3 <- data_eda_3 %>% group_by(Bulan) %>% 
    summarise("Total_Incident" = length(Department)) %>% 
    ungroup() %>% 
    arrange(Bulan) %>% 
    mutate(label_3 = glue("{Bulan}
                          {Total_Incident} Incidents"))
  
  plot3 <- ggplot(data = eda_3, aes(x = Bulan, 
                                    y = Total_Incident,
                                    text = label_3))+
    geom_col(aes(fill = Total_Incident))+
    scale_fill_gradient(high = "black", low = "blue")+
    labs(title = "Categorize by Month",
         x = NULL,
         y = "Number of Incident")+
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggplotly(plot3, tooltip = "text")
})

output$plot_eda4 <- renderPlotly({
  
  #copy dataset from rawdata
  data_eda_4 <- rawdata %>% mutate_at(.vars = c('Hari','Jam','Tipe_Insiden','Jenis_Alat','Lokasi','Penyebab','Akibat','Status','Shift'), as.factor)
  data_eda_4 <- data_eda_4 %>% filter(Tahun == 2020)
  
  #sorting month name
  data_eda_4$Bulan <-  factor(data_eda_4$Bulan, levels = month.name)
  
  
  eda_4 <- data_eda_4 %>% group_by(Bulan) %>% 
    summarise("Total_Incident" = length(Department)) %>% 
    ungroup() %>% 
    arrange(Bulan) %>% 
    mutate(label_4 = glue("{Bulan}
                          {Total_Incident} Incidents"))
  
  plot4 <- ggplot(data = eda_4, aes(x = Bulan, 
                                    y = Total_Incident,
                                    text = label_4))+
    geom_col(aes(fill = Total_Incident))+
    scale_fill_gradient(high = "black", low = "blue")+
    labs(title = "Categorize by Month",
         x = NULL,
         y = "Number of Incident")+
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggplotly(plot4, tooltip = "text")
})

output$plot_eda5 <- renderPlotly({
  
  #copy dataset from rawdata
  data_eda_5 <- rawdata %>% mutate_at(.vars = c('Hari','Jam','Tipe_Insiden','Jenis_Alat','Lokasi','Penyebab','Akibat','Status','Shift'), as.factor)
  data_eda_5 <- data_eda_5 %>% filter(Tahun == 2021)
  
  #sorting month name
  data_eda_5$Bulan <-  factor(data_eda_5$Bulan, levels = month.name)
  
  
  eda_5 <- data_eda_5 %>% group_by(Bulan) %>% 
    summarise("Total_Incident" = length(Department)) %>% 
    ungroup() %>% 
    arrange(Bulan) %>% 
    mutate(label_5 = glue("{Bulan}
                          {Total_Incident} Incidents"))
  
  plot5 <- ggplot(data = eda_5, aes(x = Bulan, 
                                    y = Total_Incident,
                                    text = label_5))+
    geom_col(aes(fill = Total_Incident))+
    scale_fill_gradient(high = "black", low = "blue")+
    labs(title = "Categorize by Month",
         x = NULL,
         y = "Number of Incident")+
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggplotly(plot5, tooltip = "text")
})

output$plot_eda6 <- renderPlotly({
  
  #copy dataset from rawdata
  data_eda_6 <- rawdata %>% mutate_at(.vars = c('Hari','Jam','Tipe_Insiden','Jenis_Alat','Lokasi','Penyebab','Akibat','Status','Shift'), as.factor)
  data_eda_6 <- data_eda_6 %>% filter(Tahun == 2022)
  
  #sorting month name
  data_eda_6$Bulan <-  factor(data_eda_6$Bulan, levels = month.name)
  
  
  eda_6 <- data_eda_6 %>% group_by(Bulan) %>% 
    summarise("Total_Incident" = length(Department)) %>% 
    ungroup() %>% 
    arrange(Bulan) %>% 
    mutate(label_6 = glue("{Bulan}
                          {Total_Incident} Incidents"))
  
  plot6 <- ggplot(data = eda_6, aes(x = Bulan, 
                                    y = Total_Incident,
                                    text = label_6))+
    geom_col(aes(fill = Total_Incident))+
    scale_fill_gradient(high = "black", low = "blue")+
    labs(title = "Categorize by Month",
         x = NULL,
         y = "Number of Incident")+
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggplotly(plot6, tooltip = "text")
})

output$plot_eda7 <- renderPlotly({
  
  #copy dataset from rawdata
  data_eda_7 <- rawdata %>% mutate_at(.vars = c('Hari','Jam','Tipe_Insiden','Jenis_Alat','Lokasi','Penyebab','Akibat','Status','Shift'), as.factor)
  data_eda_7 <- data_eda_7 %>% filter(Tahun == 2023)
  
  #sorting month name
  data_eda_7$Bulan <-  factor(data_eda_7$Bulan, levels = month.name)
  
  
  eda_7 <- data_eda_7 %>% group_by(Bulan) %>% 
    summarise("Total_Incident" = length(Department)) %>% 
    ungroup() %>% 
    arrange(Bulan) %>% 
    mutate(label_7 = glue("{Bulan}
                          {Total_Incident} Incidents"))
  
  plot7 <- ggplot(data = eda_7, aes(x = Bulan, 
                                    y = Total_Incident,
                                    text = label_7))+
    geom_col(aes(fill = Total_Incident))+
    scale_fill_gradient(high = "black", low = "blue")+
    labs(title = "Categorize by Month",
         x = NULL,
         y = "Number of Incident")+
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggplotly(plot7, tooltip = "text")
})
       
output$plot_forecast <- renderDygraph({ 
  
#Load model
model <- readRDS("data_input/TS_model.RDS")
future <- make_future_dataframe(model, periods = 60, freq = "month")
forecast_m <- predict(model, future)
  
  # Create Plot of Forecast Model
plot_forecast <- dyplot.prophet(model, forecast_m)

  #
  
})

output$wordcloud1 <- renderPlot({

  corpus_fatality <- wordcloud_fatality %>% 
    VectorSource() %>%                                        
    VCorpus() %>% 
    tm_map(content_transformer(tolower)) %>%                  #Case folding
    tm_map(removeNumbers) %>%                                 #Remove numbers
    tm_map(removeWords,unlist(list_stopword_ind)) %>%         #Remove stopwords
    tm_map(removePunctuation) %>%                             #Remove punctuation
    tm_map(stripWhitespace)                                   #Remove white spaces
  
  wordcloud(corpus_fatality, 
            scale=c(3,0.5),     # Set min and max scale
            max.words=100,      # Set maximum number of words would be displayed
            random.order=FALSE, # Words in decreasing freq
            min.freq = 2,       # Minimum frequency of word that will be displayed
            rot.per=0.35,       # % of vertical words
            use.r.layout=FALSE, # Use C++ collision detection
            colors=brewer.pal(8, "Dark2"))
})

output$wordcloud2 <- renderPlot({
  
  corpus_injury <- wordcloud_injury %>% 
    VectorSource() %>%                                        
    VCorpus() %>% 
    tm_map(content_transformer(tolower)) %>%                  #Case folding
    tm_map(removeNumbers) %>%                                 #Remove numbers
    tm_map(removeWords,unlist(list_stopword_ind)) %>%         #Remove stopwords
    tm_map(removePunctuation) %>%                             #Remove punctuation
    tm_map(stripWhitespace)                                   #Remove white spaces
  
  wordcloud(corpus_injury, 
            scale=c(3,0.5),     # Set min and max scale
            max.words=100,      # Set maximum number of words would be displayed
            random.order=FALSE, # Words in decreasing freq
            min.freq = 3,       # Minimum frequency of word that will be displayed
            rot.per=0.35,       # % of vertical words
            use.r.layout=FALSE, # Use C++ collision detection
            colors=brewer.pal(8, "Dark2"))
})

output$wordcloud3 <- renderPlot({
 
   corpus_dangerous <- wordcloud_dangerous %>% 
    VectorSource() %>%                                        
    VCorpus() %>% 
    tm_map(content_transformer(tolower)) %>%                  #Case folding
    tm_map(removeNumbers) %>%                                 #Remove numbers
    tm_map(removeWords,unlist(list_stopword_ind)) %>%         #Remove stopwords
    tm_map(removePunctuation) %>%                             #Remove punctuation
    tm_map(stripWhitespace)                                   #Remove white spaces
  
  wordcloud(corpus_dangerous, 
            scale=c(3,0.5),     # Set min and max scale
            max.words=100,      # Set maximum number of words would be displayed
            random.order=FALSE, # Words in decreasing freq
            min.freq = 2,       # Minimum frequency of word that will be displayed
            rot.per=0.35,       # % of vertical words
            use.r.layout=FALSE, # Use C++ collision detection
            colors=brewer.pal(8, "Dark2"))
})

output$wordcloud4 <- renderPlot({
  
  corpus_nearmiss <- wordcloud_nearmiss %>% 
    VectorSource() %>%                                        
    VCorpus() %>% 
    tm_map(content_transformer(tolower)) %>%                  #Case folding
    tm_map(removeNumbers) %>%                                 #Remove numbers
    tm_map(removeWords,unlist(list_stopword_ind)) %>%         #Remove stopwords
    tm_map(removePunctuation) %>%                             #Remove punctuation
    tm_map(stripWhitespace)                                   #Remove white spaces
  
  wordcloud(corpus_nearmiss, 
            scale=c(3,0.5),     # Set min and max scale
            max.words=100,      # Set maximum number of words would be displayed
            random.order=FALSE, # Words in decreasing freq
            min.freq = 20,       # Minimum frequency of word that will be displayed
            rot.per=0.35,       # % of vertical words
            use.r.layout=FALSE, # Use C++ collision detection
            colors=brewer.pal(8, "Dark2"))
})


output$plot_eda8 <- renderPlotly({
  
  data_eda_8 <- rawdata %>% mutate_at(.vars = c('Hari','Jam','Tipe_Insiden','Jenis_Alat','Lokasi','Penyebab','Akibat','Status','Shift'), as.factor)
  data_eda_8 <- data_eda_8 %>% filter(Tahun == 2019)
  data_eda_8$Tipe_Insiden <-  factor(data_eda_8$Tipe_Insiden, levels = c("Fatality","Dangerous Occurrence","First Aid / Injury","Near Miss"))
  
  eda_8 <- data_eda_8 %>% group_by(Tipe_Insiden) %>% 
    summarise("Total_Incident" = length(Department)) %>% 
    ungroup() %>% 
    mutate(label8 = glue("Tipe Insiden: {Tipe_Insiden}
                          Total Incident: {Total_Incident}"))
  
  plot8 <- ggplot(data = eda_8,aes(x = Tipe_Insiden, y = Total_Incident, text = label8))+
    geom_col(aes(fill = Tipe_Insiden), position = "dodge")+
    labs(title = "Categorize by Incident Type",
         x = NULL,
         y = "Number of Incident")+
    scale_y_continuous(limits = c(0,150))+
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggplotly(plot8, tooltip = "text")
})

output$plot_eda9 <- renderPlotly({
  
  data_eda_9 <- rawdata %>% mutate_at(.vars = c('Hari','Jam','Tipe_Insiden','Jenis_Alat','Lokasi','Penyebab','Akibat','Status','Shift'), as.factor)
  data_eda_9 <- data_eda_9 %>% filter(Tahun == 2020)
  data_eda_9$Tipe_Insiden <-  factor(data_eda_9$Tipe_Insiden, levels = c("Fatality","Dangerous Occurrence","First Aid / Injury","Near Miss"))
  
  eda_9 <- data_eda_9 %>% group_by(Tipe_Insiden) %>% 
    summarise("Total_Incident" = length(Department)) %>% 
    ungroup() %>% 
    mutate(label9 = glue("Tipe Insiden: {Tipe_Insiden}
                          Total Incident: {Total_Incident}"))
  
  plot9 <- ggplot(data = eda_9,aes(x = Tipe_Insiden, y = Total_Incident, text = label9))+
    geom_col(aes(fill = Tipe_Insiden), position = "dodge")+
    labs(title = "Categorize by Incident Type",
         x = NULL,
         y = "Number of Incident")+
    scale_y_continuous(limits = c(0,150))+
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggplotly(plot9, tooltip = "text")
})

output$plot_eda10 <- renderPlotly({
  
  data_eda_10 <- rawdata %>% mutate_at(.vars = c('Hari','Jam','Tipe_Insiden','Jenis_Alat','Lokasi','Penyebab','Akibat','Status','Shift'), as.factor)
  data_eda_10 <- data_eda_10 %>% filter(Tahun == 2021)
  data_eda_10$Tipe_Insiden <-  factor(data_eda_10$Tipe_Insiden, levels = c("Fatality","Dangerous Occurrence","First Aid / Injury","Near Miss"))
  
  eda_10 <- data_eda_10 %>% group_by(Tipe_Insiden) %>% 
    summarise("Total_Incident" = length(Department)) %>% 
    ungroup() %>% 
    mutate(label10 = glue("Tipe Insiden: {Tipe_Insiden}
                          Total Incident: {Total_Incident}"))
  
  plot10 <- ggplot(data = eda_10,aes(x = Tipe_Insiden, y = Total_Incident, text = label10))+
    geom_col(aes(fill = Tipe_Insiden), position = "dodge")+
    labs(title = "Categorize by Incident Type",
         x = NULL,
         y = "Number of Incident")+
    scale_y_continuous(limits = c(0,150))+
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggplotly(plot10, tooltip = "text")
})

output$plot_eda11 <- renderPlotly({
  
  data_eda_11 <- rawdata %>% mutate_at(.vars = c('Hari','Jam','Tipe_Insiden','Jenis_Alat','Lokasi','Penyebab','Akibat','Status','Shift'), as.factor)
  data_eda_11 <- data_eda_11 %>% filter(Tahun == 2022)
  data_eda_11$Tipe_Insiden <-  factor(data_eda_11$Tipe_Insiden, levels = c("Fatality","Dangerous Occurrence","First Aid / Injury","Near Miss"))
  
  eda_11 <- data_eda_11 %>% group_by(Tipe_Insiden) %>% 
    summarise("Total_Incident" = length(Department)) %>% 
    ungroup() %>% 
    mutate(label11 = glue("Tipe Insiden: {Tipe_Insiden}
                          Total Incident: {Total_Incident}"))
  
  plot11 <- ggplot(data = eda_11,aes(x = Tipe_Insiden, y = Total_Incident, text = label11))+
    geom_col(aes(fill = Tipe_Insiden), position = "dodge")+
    labs(title = "Categorize by Incident Type",
         x = NULL,
         y = "Number of Incident")+
    scale_y_continuous(limits = c(0,150))+
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggplotly(plot11, tooltip = "text")
})

output$plot_eda12 <- renderPlotly({
  
  data_eda_12 <- rawdata %>% mutate_at(.vars = c('Hari','Jam','Tipe_Insiden','Jenis_Alat','Lokasi','Penyebab','Akibat','Status','Shift'), as.factor)
  data_eda_12 <- data_eda_12 %>% filter(Tahun == 2023)
  data_eda_12$Tipe_Insiden <-  factor(data_eda_12$Tipe_Insiden, levels = c("Fatality","Dangerous Occurrence","First Aid / Injury","Near Miss"))
  
  eda_12 <- data_eda_12 %>% group_by(Tipe_Insiden) %>% 
    summarise("Total_Incident" = length(Department)) %>% 
    ungroup() %>% 
    mutate(label12 = glue("Tipe Insiden: {Tipe_Insiden}
                          Total Incident: {Total_Incident}"))
  
  plot12 <- ggplot(data = eda_12,aes(x = Tipe_Insiden, y = Total_Incident, text = label12))+
    geom_col(aes(fill = Tipe_Insiden), position = "dodge")+
    labs(title = "Categorize by Incident Type",
         x = NULL,
         y = "Number of Incident")+
    scale_y_continuous(limits = c(0,150))+
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 30, hjust = 1))
  
  ggplotly(plot12, tooltip = "text")
})

output$plot_eda_hour1 <- renderPlotly({
  
  data_eda_hour1 <- rawdata %>% mutate_at(.vars = c('Hari','Tipe_Insiden','Jenis_Alat','Lokasi','Penyebab','Akibat','Status','Shift'), as.factor)
  data_eda_hour1$Jam <- as.numeric(data_eda_hour1$Jam)
  
  eda_hour1 <- data_eda_hour1 %>% group_by(Jam) %>% 
    summarise("Total_Incident" = length(Department)) %>% 
    ungroup() %>% 
    arrange(Jam) %>% 
    mutate(label_hour1 = glue("{Jam}:00 - {Jam}:59
                              Total Incident: {Total_Incident}"))
  
  plot_hour1 <- ggplot(data = eda_hour1,aes(x = Jam, y = Total_Incident, text = label_hour1))+
    
    geom_col(aes(fill = Total_Incident))+
    scale_fill_gradient(high = "black", low = "red")+
    labs(title = "Categorize by Hour",
         x = NULL,
         y = "Number of Incident")+
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
  
  ggplotly(plot_hour1, tooltip = "text")
})

output$forecast_number <- renderUI({
  forecast_m <- readRDS("data_input/forecast_result.rds")
  numbering <- match(input$input_sel, forecast_m$ds)
  
  if(round(forecast_m$yhat[numbering])>=8){warna = "red"}
  else {(warna ="green")}
  
  if(round(forecast_m$yhat[numbering])>=8){gambar = "exclamation"}
  else {(gambar ="magnifying-glass")}
  
  infoBox(width = 12, "Forecast Number", round(forecast_m$yhat[numbering]), icon = icon(gambar), color = warna)
  
})

output$alert_text <- renderUI({
  forecast_m <- readRDS("data_input/forecast_result.rds")
  numbering <- match(input$input_sel, forecast_m$ds)
  low_risk = "Low risk doesn't mean no risk. Keep safety a priority and stay alert for any potential incidents."
  high_risk = "High-risk situations demand heightened awareness. Stay focused and report any potential hazards without delay."
  
  if(round(forecast_m$yhat[numbering])>=8){text = high_risk}
  else {(text = low_risk)}
  
  div(style = "text-align:justify",
      p(h4(tags$b("ALERT"))),
      p(text))
  
})

output$plot_act_pred <- renderPlotly({
  hasil_prediksi <- read_excel("data_input/prediction.xlsx")
  hasil_melt <- melt(hasil_prediksi, id.vars = "ds", variable.name = "series") %>% 
    filter(value != 0) %>% 
    mutate(label_act_pred = glue("{series} 
                                  {ds}
                                  {value}"))
  
  plot_act_pred <- ggplot(hasil_melt, aes(x = ds, y = value)) +
    geom_line(aes(colour = series))+
    geom_point(aes(text = label_act_pred),col="black", size = 0.5)+
    labs(title = "Time Series Forecasting
(PROPHET)",
x = NULL,
y = "Number of Incident")+
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
  
  ggplotly(plot_act_pred, tooltip = "text")
})

}