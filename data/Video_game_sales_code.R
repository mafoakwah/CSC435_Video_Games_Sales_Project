install.packages("tinytex")



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

colnames(gamesales2010_2016)

gamesalesNA <- filter(gamesales2010_2016, Year_of_Release == "N/A")
gamesales2010_2016 <- filter(gamesales2010_2016, Year_of_Release != "N/A")
nrow(gamesales2010_2016)
view(gamesalesNA)
#platform NA sales
platform_NA_Sales <- gamesales2010_2016 %>%
  group_by(Platform) %>%
  summarize(NA_total = sum(NA_Sales)) %>%
  arrange(NA_total)
view(platform_NA_Sales)

#platform Global sales
platform_glbl_sales <- gamesales2010_2016 %>%
  group_by(Platform) %>%
  summarize(glbl_total = sum(Global_Sales)) %>%
  arrange(glbl_total)
view(platform_glbl_sales)

#category NA sales
cat_na_sales <- gamesales2010_2016 %>%
  group_by(Genre) %>%
  summarise(cat_na_total = sum(NA_Sales)) %>%
  arrange(cat_na_total)
view(cat_na_sales)

#category Global sales
cat_glbl_sales <-gamesales2010_2016 %>%
  group_by(Genre) %>%
  summarise(cat_glbl_total = sum(Global_Sales)) %>%
  arrange(cat_glbl_total)
view(cat_glbl_sales)


#developer global sales
dev_global <- gamesales2010_2016 %>%
  filter(Developer != "N/A") %>%
  group_by(Developer) %>%
  summarise(global_total = sum(Global_Sales)) %>%
  arrange(-global_total) %>%
  slice_head(n = 5)
view(dev_global)

#developer profit based on genre
dev_genre_global <- gamesales2010_2016 %>%
  filter(Developer != "N/A") %>%
  filter(Genre != "N/A") %>%
  group_by(Genre, Developer) %>%
  summarise(devcat_total = sum(Global_Sales)) %>%
  arrange(-devcat_total)
view(dev_genre_global)



######
dev_gen <- gamesales2010_2016 %>%
  filter(Developer != "N/A") %>%
  group_by(Genre, Developer) %>%
  summarise(gencat_total = sum(Global_Sales))

view(dev_genre_globalSale)





