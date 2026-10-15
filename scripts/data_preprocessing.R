# Load required libraries
install.packages("lubridate")
library(tidyverse)
library(lubridate)

# Read the raw dataset
aqi_raw <- read.csv("C:/Users/ubhim/OneDrive/Desktop/AQI_Dashboard/data/raw/city_day.csv")

# View first few rows
head(aqi_raw)

# Count missing values per column
colSums(is.na(aqi_raw))

aqi_raw$Date <- as.Date(aqi_raw$Date, format="%Y-%m-%d")
aqi_raw$PM2.5 <- as.numeric(aqi_raw$PM2.5)

aqi_raw$AQI_Category <- cut(aqi_raw$AQI,
                            breaks=c(0,50,100,200,300,500),
                            labels=c("Good","Moderate","Poor","Very Poor","Severe"),
                            right=FALSE)
aqi_daily <- aqi_raw %>%
  group_by(City, Date) %>%
  summarise(Avg_AQI = mean(AQI, na.rm=TRUE))


# Check missing values per column
colSums(is.na(aqi_raw))
aqi_clean <- aqi_raw %>%
  filter(!is.na(AQI))
aqi_clean <- aqi_clean %>%
  group_by(City) %>%
  mutate(
    PM2.5 = ifelse(is.na(PM2.5), mean(PM2.5, na.rm=TRUE), PM2.5),
    PM10  = ifelse(is.na(PM10), mean(PM10, na.rm=TRUE), PM10),
    NO2   = ifelse(is.na(NO2), mean(NO2, na.rm=TRUE), NO2),
    SO2   = ifelse(is.na(SO2), mean(SO2, na.rm=TRUE), SO2),
    CO    = ifelse(is.na(CO), mean(CO, na.rm=TRUE), CO),
    O3    = ifelse(is.na(O3), mean(O3, na.rm=TRUE), O3)
  ) %>%
  ungroup()


# Create processed folder if it doesn't exist
if(!dir.exists("data/processed")) dir.create("data/processed")

# Save cleaned data
write.csv(aqi_clean, "data/processed/aqi_clean.csv", row.names = FALSE)



