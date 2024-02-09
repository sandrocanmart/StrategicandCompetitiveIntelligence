library(udpipe)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidytext)
#install.packages("dplyr",dependencies=T)
#install.packages("tidytext")

udmodel <- udpipe_download_model(language = "english")

udmodel <- udpipe_load_model(udmodel$file)

patents1 = read_delim(
  "datasets/aerial_vehicle_trends.csv",
  delim=",")

patents2 = read_delim(
  "datasets/pesticide_box_trends.csv",
  delim=",")

patents3 = read_delim(
  "datasets/liquid_medicine_trends.csv",
  delim=",")

patents4 = read_delim(
  "datasets/water_tank_trends.csv",
  delim=",")

patents5 = read_delim(
  "datasets/control_system_trends.csv",
  delim=",")

yearly_trend <- function(df) {
  df$year <- as.numeric(substr(df$date, 0, 4))
  trends_sum <- df %>%
    group_by(year) %>%
    summarize(freq = sum(freq)) %>%
    arrange(year)
  trends_sum
}

s1 <- yearly_trend(patents1)
s2 <- yearly_trend(patents2)
s3 <- yearly_trend(patents3)
s4 <- yearly_trend(patents4)
s5 <- yearly_trend(patents5)


TOYs <- data.frame(year = 2010:2022) %>%
  merge(y=s1, by="year", all.x=TRUE) %>%
  mutate(`aerial vehicle` = freq) %>%
  subset(select = -c(freq)) %>%


  merge(y=s2, by="year", all.x=TRUE) %>%
  mutate(`pesticide box` = freq) %>%
  subset(select = -c(freq)) %>%
  
  merge(y=s3, by="year", all.x=TRUE) %>%
  mutate(`liquid medicine` = freq) %>%
  subset(select = -c(freq)) %>%
  
  merge(y=s4, by="year", all.x=TRUE) %>%
  mutate(`water tank` = freq) %>%
  subset(select = -c(freq)) %>%
  
  merge(y=s5, by="year", all.x=TRUE) %>%
  mutate(`control system` = freq) %>%
  subset(select = -c(freq)) %>%
  
  reshape2::melt(id.var = "year")


ggplot(TOYs, aes(x=year, y=log(value), color=variable)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Frequency", 
       title = "N-Grams trends on Google") +
  xlim(2010, 2022)

