rm(list=ls())

library(tidyverse)
library(zoo)
setwd("Documents/RStudioWD")

# Set the path to the folder containing the CSV files
path <- "/Users/Charlie/Documents/RStudioWD/daylioExports"

# Get a list of all CSV files in the folder
files <- list.files(path = path, pattern = "*.csv")

# Sort the files by date and get the most recent one
file <- sort(files, decreasing = TRUE)[1]

# Read the CSV file
daylioMessy <- read.csv(file.path(path, file))
#daylioMessy[daylioMessy$activities == " "] <- NA

#daylioMessy <- read_csv(file.path(path,'daylio_export_2021_04_24.csv'))


df <- daylioMessy %>%
  select(-c(date,note_title,note,weekday)) %>%
  arrange(full_date) %>%
  mutate(moodVal = mood) %>%
  mutate(moodVal = str_replace(moodVal,"awful",'-2')) %>%
  mutate(moodVal = str_replace(moodVal,"bad",'-1')) %>%
  mutate(moodVal = str_replace(moodVal,"meh",'0')) %>%
  mutate(moodVal = str_replace(moodVal,"good",'1')) %>%
  mutate(moodVal = str_replace(moodVal,"rad",'2')) %>%
  mutate(moodVal = as.integer(moodVal)) %>%
  mutate(mood = as.factor(mood)) %>%
  separate_rows(activities, sep = ' \\| ') %>%
  mutate(activities = str_trim(activities, side = c("both"))) %>%
  mutate(val = 1) %>%
  pivot_wider(names_from = activities,
                     values_from = val,
                     values_fill = 0) %>%
  select(-c("NA"))

  dfDayGroup <- df %>%
    group_by(full_date) %>%
    summarise(moodSummary = mean(moodVal)) %>%
    mutate(seven_avg= rollmean(moodSummary, 7,
                               align="left", 
                               fill=0))

  ggplot(df, aes(x = full_date, y = moodVal)) + 
    geom_point(aes(colour=moodVal))
    
  ggplot(dfDayGroup, aes(x = full_date, y = moodSummary)) + 
    geom_point(aes(colour=moodSummary)) +
    geom_line(aes(y = seven_avg), 
              color = "blue", 
              size = .75)