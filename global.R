library(shiny)
library(shinydashboard)
library(htmltools)

# use for data preparation
library(readxl)                     
library(dplyr)
library(padr)
library(lubridate)
library(reshape2)

# use for graphic chart
library(ggplot2)
library(plotly)
library(glue)
library(zoo)
library(ggcorrplot)
library(dygraphs)

# use for wordcloud
library(stringr)
library(tm)
library(SnowballC)
library(wordcloud)

# model
library(prophet)

#
library(MLmetrics)

#
library(xgboost)
library(tidymodels)
library(modeltime)
library(tidyverse)
library(lubridate)
library(timetk)

# Settingan Agar tidak muncul numeric value
options(scipen = 9999)

# read data
rawdata <- read_excel("data_input/RAW_DATA2.xlsx")

# storage data
fatality <- rawdata %>% 
  filter(rawdata$Tipe_Insiden == "Fatality")

danger <- rawdata %>% 
  filter(rawdata$Tipe_Insiden == "Dangerous Occurrence")

injury <- rawdata %>% 
  filter(rawdata$Tipe_Insiden == "First Aid / Injury")

nearmiss <- rawdata %>% 
  filter(rawdata$Tipe_Insiden == "Near Miss")

# wordcloud
wordcloud_fatality <- rawdata %>% filter(Tipe_Insiden == "Fatality") %>% 
  select(Deskripsi_Kejadian)

wordcloud_injury <- rawdata %>% filter(Tipe_Insiden == "First Aid / Injury") %>% 
  select(Deskripsi_Kejadian)

wordcloud_dangerous <- rawdata %>% filter(Tipe_Insiden == "Dangerous Occurrence") %>% 
  select(Deskripsi_Kejadian)

wordcloud_nearmiss <- rawdata %>% filter(Tipe_Insiden == "Near Miss") %>% 
  select(Deskripsi_Kejadian)

stopword_ind <- read.csv("data_input/stopwordbahasa.csv")

#converting dataframe into list
list_stopword_ind <- list()
for(i in 1:ncol(stopword_ind)) 
{list_stopword_ind[[i]] <- stopword_ind[ , i]} 


