library(tidyverse)
library(readr)
Video_Games_Sales <- read_csv("data/Video_Games_Sales_as_at_22_Dec_2016.csv")

View(Video_Games_Sales)
head(Video_Games_Sales)

glimpse(Video_Games_Sales)
str(Video_Games_Sales)

colnames(Video_Games_Sales)
length(Video_Games_Sales)

dim(Video_Games_Sales)

gamesales2010_2016 <- filter(Video_Games_Sales, Year_of_Release >= 2010)
nrow(gamesales2010_2016)

gamesales2010_2016 <- filter(gamesales2010_2016, Year_of_Release != "N/A")
nrow(gamesales2010_2016)
