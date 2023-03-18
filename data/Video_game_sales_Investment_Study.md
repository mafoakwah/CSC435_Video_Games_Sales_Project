---
title: "Notes"
output: html_document
date: "2023-03-18"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

## 1. Scenerio
            
Suppose their is an investor who wants to invest in a gaming platform. However before the       investor invests, they want to know which gaming platform between the years 2010 to 2016 has the best NA, EU, and Global sales. The investor also wants to know what video game category produces the most income between 2010 and 2016 while for each video game platform so that they can better invest their money to better profit themselves in the future.The investor also wants to see what developers create more profitable games and the video game categories those developers mainly develop. The investor wants an indepth analyst on the video game platforms, video game categories, and developers before they invest their money. 

##2. Ask

  * What are the two best video game platforms to invest in?
  * What video game category should the investor invest in?
  * What video game developer had the most video game sales?


##3.Preparation
  The data we are using was uploaded to Kaggle by SID_TWR. SID_TWR stated that this dataset was      scraped from VGChartz and Metacritic.

#3.1 Load Tidyverse package

```{r}
#install.packages("Tidyverse")
  library(tidyverse)
```

```{r}
library(readr) 
```


#3.2 Import dataset  

```{r}
Video_Games_Sales <- read_csv("data/Video_Games_Sales_as_at_22_Dec_2016.csv")
```

#3.3 Preview dataset
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

#4.1 Since we are focusing on the video game sales between 2010 to 2016 we will shrink the data set by filtering it.

```{r}
gamesales2010_2016 <- filter(Video_Games_Sales, Year_of_Release >= 2010)
nrow(gamesales2010_2016)
```

#4.2 We will now filter all rows where the Year_of_Release is unknown.

```{r}
gamesales2010_2016 <- filter(gamesales2010_2016, Year_of_Release != "N/A")
nrow(gamesales2010_2016)
```


