---
title: "Notes"
output: html_document
date: "2023-03-18"



## 1. Scenerio
            
Suppose their is an investor who wants to invest in a gaming platform. However before the       investor invests, they want to know which gaming platform between the years 2010 to 2016 has the best NA, EU, and Global sales. The investor also wants to know what video game category produces the most income between 2010 and 2016 while for each video game platform so that they can better invest their money to better profit themselves in the future.The investor also wants to see what developers create more profitable games and the video game categories those developers mainly develop. The investor wants an indepth analyst on the video game platforms, video game categories, and developers before they invest their money. 

##2. Questions

  * Which video game platform made the most NA Sales from 2010 to 2016?
  * Which video game platform made the most Global Sales from 2010 to      2016?
  * Which video game category had the highest NA sales?
  * Which video game category made the highest Global sales?
  * Which video game developer has the highest Global video game sales?
  * Which video game genre is the top three developers most profitable in?


##3.Preparation
  The data we are using was uploaded to Kaggle by SID_TWR. SID_TWR stated that this dataset was      scraped from VGChartz and Metacritic.

3.1 Load Tidyverse package

```#install.packages("Tidyverse")
  library(tidyverse)
```

```{r}
library(readr) 
```


3.2 Import dataset  

```{r}
Video_Games_Sales <- read_csv("data/Video_Games_Sales_as_at_22_Dec_2016.csv")
```

3.3 Preview dataset
```{r}
View(Video_Games_Sales)
```

```{r}
head(Video_Games_Sales)
```


```{r}
glimpse(Video_Games_Sales)
```
```{r}
str(Video_Games_Sales)
```

```{r}
colnames(Video_Games_Sales)
```
```{r}
length(Video_Games_Sales)
```

```{r}
dim(Video_Games_Sales)
```

##4. Data Cleaning

We will be using cleaning data using data transformations.

4.1 Since we are focusing on the video game sales between 2010 to 2016 we will shrink the data set by filtering it.

```{r}
gamesales2010_2016 <- filter(Video_Games_Sales, Year_of_Release >= 2010)
nrow(gamesales2010_2016)
```

4.2 We will now filter all rows where the Year_of_Release is unknown.

```{r}
gamesales2010_2016 <- filter(gamesales2010_2016, Year_of_Release != "N/A")
nrow(gamesales2010_2016)
```

##5. Analyzing 

Now we are going to answer the questions that were mentioned in section 2:
  * Which video game platform made the most NA Sales from 2010 to 2016?
  * Which video game platform made the most Global Sales from 2010 to      2016?
  * Which video game category had the highest NA sales?
  * Which video game category made the highest Global sales?
  * Which top 5 video game developers have the highest Global video game sales?
  * Which video game genre is the top three developers most profitable in

5.1 To answer the first question we have to calculate the NA revenue of each platform.
```platform_NA_Sales <- gamesales2010_2016 %>%
  group_by(Platform) %>%
  summarize(NA_total = sum(NA_Sales)) %>%
  arrange(NA_total)
  view(platform_NA_Sales)
  ```

5.2 Secondly, we calculate the global revenue for each platform.
```platform_glbl_sales <- gamesales2010_2016 %>%
  group_by(Platform) %>%
  summarize(glbl_total = sum(Global_Sales)) %>%
  arrange(glbl_total)
view(platform_glbl_sales)
```

5.3 Then, we calculate the NA revenue based on categories.
```cat_na_sales <- gamesales2010_2016 %>%
  group_by(Genre) %>%
  summarise(cat_na_total = sum(NA_Sales)) %>%
  arrange(cat_na_total)
view(cat_na_sales)```

5.4 Now, we calculate the Global revenue based on categories.
```cat_glbl_sales <-gamesales2010_2016 %>%
  group_by(Genre) %>%
  summarise(cat_glbl_total = sum(Global_Sales)) %>%
  arrange(cat_glbl_total)
view(cat_glbl_sales)
```

5.5 After, we want to calculate the global revenue based on the developer.

```dev_global <- gamesales2010_2016 %>%
  filter(Developer != "N/A") %>%
  group_by(Developer) %>%
  summarise(global_total = sum(Global_Sales)) %>%
  arrange(-global_total) %>%
  slice_head(n = 5)
view(dev_global)
```

5.6 Finally, we want to find what video game categories are mainly developed by the top three developers and how the revenue in these categories differ.

