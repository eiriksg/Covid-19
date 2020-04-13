source("utils.R")

# Download dataset from GitHub
ncovConfirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
ncovDeaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
ncovRecovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

tomorrow<-  format(Sys.Date(), "%Y-%m-%d")
today<-  format(Sys.Date()-1, "%Y-%m-%d")
yesterday<-  format(Sys.Date()-2, "%Y-%m-%d")

#Convert dataset to Tidy
ncovConfirmedTidy <- tidy_corona(ncovConfirmed)
ncovDeathsTidy <- tidy_corona(ncovDeaths)
ncovRecoveredTidy <- tidy_corona(ncovRecovered)

# Rank for Confirmed Cases
countries<-ncovDeathsTidy %>% 
  group_by(Country.Region) %>% 
  summarize(total=sum(value)) %>% 
  arrange(desc(total)) %>% 
  head(n=20)

countries_total_raw<-"https://redutv-api.vg.no/corona/v1/world/reports?"%>% jsonlite::fromJSON() %>% simplify() 
countries_total<-as_tibble(countries_total_raw$data)

countries_total %>% 
  summarize(total_confirmed=sum(confirmed),total_fatalities=sum(deaths)) %>% 
  rename("Total Confirmed"=total_confirmed, " Total Fatalities"=total_fatalities)

# # Total confirmed, dead and recovered
# ncovConfirmedTidy %>% 
#   rename(conf=value) %>% 