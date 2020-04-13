library(tidyverse)
library(maps)
library(ggthemes)
library(gganimate)
library(ggmap) #load the library
library(sf)
library(rnaturalearth)
library(viridis)
library(formattable)
library(leaflet)
library(lubridate)
library(nCov2019)
library(tsibble)
library(fable)
library(gapminder)
library(plotly)
library(readxl)

tidy_corona <- function(df) {
  df %>%
    pivot_longer(cols = starts_with("X"), names_to = "date") %>%
    mutate(date = str_replace(date, "X", ""),
           date = mdy(date))
}


API_SP_POP_TOTL_DS2_en_excel_v2_887218 <- read_excel("input/API_SP.POP.TOTL_DS2_en_excel_v2_887218.xls")
population_total<-API_SP_POP_TOTL_DS2_en_excel_v2_887218 %>% 
  select(`Data Source`,"...63") %>% 
  rename(Country.Region=`Data Source`,population="...63" ) %>% 
  filter(Country.Region!="Country Name", Country.Region!="Last Updated Date") %>% 
  mutate(population=as.numeric(population)) %>% 
  drop_na()

new_cases<-function(today, df,type) {
  title<-sprintf("Top 15 Countries with most new %s cases of covid-19 on %s \n",type, today)
  df_change<-df %>% 
    group_by(Country.Region) %>% 
    summarize(change=sum(value[date==today])-sum(value[date==yesterday]), changepercentage=(sum(value[date==today])-sum(value[date==yesterday]))*100/sum(value[date==yesterday])) %>% 
    drop_na() %>% 
    arrange(desc(change)) %>% 
    head(n=15) 
  
  ggplot(df_change,aes(x=fct_reorder(Country.Region,change), y=change, fill=change)) +
    geom_text(aes(label = change),hjust = -0.1)+
    labs(title = title, y="Number of Cases", x="Country", fill=" New Cases last 24h")+
    geom_col() + 
    coord_flip()+
    scale_fill_viridis()+
    ylim(0, max(df_change$change)*1.1)
}


new_cases_pop<-function(today, df,type) {
  title<-sprintf("Top 15 Countries with most new %s by %s \n",type, today)
  df_change<-df %>% 
    left_join(population_total) %>%
    group_by(Country.Region) %>% 
    summarize(change=(sum(value[date==today])-sum(value[date==yesterday]))*1000000/mean(population)) %>%
    drop_na() %>% 
    arrange(desc(change)) %>% 
    head(n=15)
  
    ggplot(df_change,aes(x=fct_reorder(Country.Region,change), y=change, fill=change)) + 
    geom_col() + 
    coord_flip()+
    geom_text(aes(label = round(change),hjust = -0.1))+
    scale_fill_viridis()+
    labs(title = title, y="Number of New cases pr 1000000", x="Country",fill = "Numeber of\n infected pr\n100 0000:")+
    ylim(0, max(df_change$change)*1.1)
    
}

