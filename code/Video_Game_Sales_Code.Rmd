---
output:
  word_document: default
  html_document: default
  pdf_document: default
---
library(formatR)
library(tidyverse) #Used to load tidyverse package into R.
library(readr) #Used to load readr package in R to use to read CSV file.

#Loading the data set
Video_Games_Sales <- read_csv("C:/Users/Matthew Afoakwah/OneDrive/Documents/Bport/Bport Spring 2023/CSC435 Data Analytics/Project/CSC435_Video_Games_Sales_Project/data/Video_Games_Sales_as_at_22_Dec_2016.csv")

#head() function to get a preview of the dataset.
head(Video_Games_Sales)

#shows a glimpse() of the data set along with the attributes.
glimpse(Video_Games_Sales)

#Shows the name of the columns in the Video_Games_Sales data set.
colnames(Video_Games_Sales) 

#dim() function to find the length of the data set.
dim(Video_Games_Sales)

#sapply() function to apply the class function on each attribute of our data set. 
sapply(Video_Games_Sales, class) 

#filter() funtion to filter out all years less then 2010 and greater then 2016.
gamesales2010_2016 <- filter(Video_Games_Sales, Year_of_Release >= 2010 )
gamesales2010_2016 <- filter(gamesales2010_2016, Year_of_Release <= 2016)

#The filter() function removes all values that are "N/A" in the Year_of_Release column
gamesales2010_2016 <- filter(gamesales2010_2016, Year_of_Release != "N/A")

#The duplicated() function finds all rows where there are repeated occurences. Then we use the ! to filter them out from the graph.
gamesales2010_2016 <- gamesales2010_2016[!duplicated(gamesales2010_2016), ]

#Create a new variable outliers, then use boxplot.stats() to assign outliers from Global sales to outliers.
boxplot(gamesales2010_2016$Global_Sales, main = "Global Sales BoxPlot")
outliers <- boxplot.stats(gamesales2010_2016$Global_Sales)$out

#Create a new variable outliersNA, then use boxplot.stats() to assign our outliers from NA sales to outliersNA.
boxplot(gamesales2010_2016$NA_Sales, main = "NA Sales BoxPlot")
outliersNA <- boxplot.stats(gamesales2010_2016$NA_Sales)$out

#unique function lists the unique characters in the platform column.
unique(gamesales2010_2016$Platform) 

#We use recode() to change the values of certain attributes to new attributes such as Nintendo, PlayStation, and Xbox.
gamesales2010_2016$Platform <- recode(gamesales2010_2016$Platform, "X360" = "Xbox", "PS3" = "PlayStation", "DS" = "Nintendo", "PS4" = "PlayStation", "3DS" = "Nintendo", "Wii" = "Nintendo", "XOne" = "Xbox", "WiiU" = "Nintendo", "PSP" = "PlayStation", "PSV" = "PlayStation", "PS2" = "PlayStation")

#We use unique()to list the unique attributes in the platform column.
unique(gamesales2010_2016$Platform)


#We create a new data thar has the sum of NA sales for each platform.
platform_NA_Sales <- gamesales2010_2016 %>% 
#We use the group_by() to group all the platforms in the platform_NA_Sales together.  
  group_by(Platform) %>% 
#We use the summarize() to summarize the sum of NA_sales for each different platform.
  summarize(NA_total = sum(NA_Sales)) %>%  
#We arrange the data set by decreasing order according to the NA_total.
  arrange(desc(NA_total)) 
#We use the head() function to preview the new data set.
  head(platform_NA_Sales) 
 
  
#We create a new data thar has the sum of NA and Global sales for each platform in each year.
platform_SalesbyYear <- gamesales2010_2016 %>% 
#We use the group_by() to group all the Platform and Year_of_Release together.
  group_by(Platform, Year_of_Release) %>% 
#We use the summarize() to summarize the sum of NA_sales for each different platform, and year.
  summarize(NA_total = sum(NA_Sales), glbl_total = sum(Global_Sales)) %>%  
#We arrange the data set by decreasing order according to the glbl_total.
  arrange(desc(glbl_total)) 
#We use the head() function to preview the new data set.
  head(platform_SalesbyYear)
  
#We move the gamesales2010_2016 data into a new data set then pipe the command.
platform_glbl_sales <- gamesales2010_2016 %>% 
#We use the group_by() to group all the platforms together.
  group_by(Platform) %>%
#We use the summarize function to summarize the sum of Global_sales for each different platform.
  summarize(glbl_total = sum(Global_Sales)) %>% 
#We arrange the data set by decreasing order according to the glbl_total.  
  arrange(desc(glbl_total)) 
#We use the head() function to preview the table.
head(platform_glbl_sales) 

#We use the unique to search for any irregular values in the categories column.
unique(gamesales2010_2016$Genre) 

#We move the gamesales2010_2016 data into a new data set.
genre_na_sales <- gamesales2010_2016 %>% 
#We use the group_by() to group all the Genres together.
  group_by(Genre) %>%  
#We use summarize() to summarize the sum of na_sales for each different genre.
  summarise(genre_na_total = sum(NA_Sales)) %>%  
#We arrange the data set by decreasing order according to the genre_na_total.
  arrange(desc(genre_na_total))
#We use the head() function to preview the table.
head(genre_na_sales) 

#We move the gamesales2010_2016 data into a new data set.
genre_SalesbyYear <- gamesales2010_2016 %>%
#We use the group_by() to group all the Genre and Year_of_Release together.
  group_by(Genre, Year_of_Release) %>%
#We use the summarize() to summarize the sum of NA_sales for each different Genre, and year.  
  summarize(NA_total = sum(NA_Sales), glbl_total = sum(Global_Sales)) %>%
#We arrange the data set by decreasing order according to the glbl_total.
  arrange(desc(glbl_total))
#We use the head() function to preview the new data set.
head(genre_SalesbyYear)

#We move the gamesales2010_2016 data into a new data set then pipe the command.
genre_glbl_sales <-gamesales2010_2016 %>% 
#We use group_by() to group all the Genres in the genre_glbl_sales together.
  group_by(Genre) %>%  
#We use summarize() to summarize the sum of Global_sales for each different genre.
  summarise(genre_glbl_total = sum(Global_Sales)) %>%  
#We arrange the data set by decreasing order according to the genre_na_total.
  arrange(desc(genre_glbl_total))
#We use the head() function to preview the table.
head(genre_glbl_sales)

#We use unique() to take all unique values in the developer column then sort them. 
unique_dev <- sort(unique(gamesales2010_2016$Developer)) 

#We use the recode() function from the dplyr package from our tidyverse package in order change our variables.
gamesales2010_2016$Developer <- recode(gamesales2010_2016$Developer,"1C: Maddox Games" ="1C Company", "1C:Ino-Co" = "1C Company","2K Australia" = "2K Games", "2K Czech" = "2K Games", "2K Sports" = "2K Games", "2K Marin" = "2K Games", "2K Play" = "2K Games", "505 Games, Sarbakan Inc." = "505 Games", "Activision, Behaviour Interactive" = "Activision", "Activision, FreeStyleGames" = "Activision", "Ambrella, The Pokemon Company" = "Ambrella", "Armature Studio, comcept" = "Armature Studio", "Artificial Mind and Movement, EA Redwood Shores" = "Artificial Mind and Movement", "Atari, Atari SA" = "Atari", "Atari, Slightly Mad Studios, Atari SA" = "Atari", "Atlus, Dingo Inc." = "Atlus", "Atomic Planet Entertainment" = "Atomic Games", "Avalanche Software" = "Avalanche Studios", "Bandai Namco Games, Artdink" = "Bandai Namco Games", "Beenox, Other Ocean Interactive" = "Beenox", "Big Blue Bubble Inc., Scholastic, Inc." = "Big Blue Bubble Inc.", "Big Blue Bubble Inc., Scholastic, Inc." = "Big Blue Bubble Inc.", "Blitz Games Studios" = "Blitz Games", "Blue Byte, Related Designs" = "Blue Byte", "Bungie Software, Bungie" = "Bungie", "Capcom Vancouver" = "Capcom", "Capcom, Pipeworks Software, Inc." = "Capcom")

gamesales2010_2016$Developer <- recode(gamesales2010_2016$Developer, "Capcom, QLOC" = "Capcom", "Climax Entertainment" = "Climax Studios", "Climax Group" = "Climax Studios", "Climax Group, Climax Studios" = "Climax Studios", "Codemasters Birmingham" = "Codemasters", "Compile Heart, GCREST" = "Compile Heart", "Crave, DTP Entertainment" = "Crave", "Crystal Dynamics, Nixxes Software" = "Crystal Dynamics", "Cyanide, Cyanide Studios" = "Cyanide", "CyberConnect2, Racjin" = "CyberConnect2", "CyberPlanet Interactive Public Co., Ltd., Maximum Family Games" = "CyberPlanet Interactive Public Co., Ltd.", "Deep Silver Dambuster Studios" = "Deep Silver", "Deep Silver, Keen Games" = "Deep Silver", "Dimps Corporation, Dream Execution" = "Dimps Corporation", "Dimps Corporation, SCE Japan Studio" = "Dimps Corporation", "Disney Interactive Studios, Land Ho!" = "Disney Interactive Studios", "EA Black Box" = "EA Games", "EA Bright Light" = "EA Games", "EA Canada" = "EA Games", "EA Canada, EA Vancouver" = "EA Games" , "EA DICE" = "EA Games", "EA DICE, Danger Close" = "EA Games")
#"EA Sports" = "EA Games"

gamesales2010_2016$Developer <- recode(gamesales2010_2016$Developer, "EA Sports" = "EA Games", "EA Montreal" = "EA Games", "EA Redwood Shores" = "EA Games", "EA Sports, EA Canada" = "EA Games", "EA Sports, EA Vancouver" = "EA Games", "EA Tiburon" = "EA Games", "Eidos Montreal, Nixxes Software" = "Eidos Montreal", "Engine Software, Re-Logic" = "Engine Software", "Epic Games, People Can Fly" = "Epic Games", "Farsight Studios, Crave" = "Farsight Studios", "Gaijin Entertainment" = "Gaijin Games", "Gearbox Software, 3D Realms" = "Gearbox Software", "Gearbox Software, WayForward" = "Gearbox Software", "Guerilla Cambridge" = "Guerrilla Cambridge", "Guerilla" = "Guerilla Cambridge")


gamesales2010_2016$Developer <- recode(gamesales2010_2016$Developer,"Harmonix Music Systems, Demiurge" = "Harmonix Music Systems", "Headup Games, Crenetic Studios" = "Headup Games", "Konami Computer Entertainment Hawaii" = "Konami", "Marvelous AQL" = "Marvelous Inc.", "Marvelous Entertainment" = "Marvelous Inc.", "Midway Studios - Austin" = "Midway", "Monolith Soft" = "Monolith Productions", "Monolith Soft, Banpresto" = "Monolith Productions")

gamesales2010_2016$Developer <- recode(gamesales2010_2016$Developer,"Namco Bandai Games America, Namco Bandai Games" = "Namco Bandai Games", "Namco Bandai Games, Bandai Namco Games" = "Namco Bandai Games", "Namco Bandai Games, Cellius" = "Namco Bandai Games", "Namco Bandai Games, Monkey Bar Games" = "Namco Bandai Games", "NATSUME ATARI Inc." = "Natsume", "Nintendo EAD Tokyo" = "Nintendo", "Nintendo, Camelot Software Planning" = "Nintendo", "Nintendo, Headstrong Games" = "Nintendo", "Nintendo, Intelligent Systems" = "Nintendo", "Nintendo, Nd Cube" = "Nintendo", "Nintendo, Nintendo Software Technology" = "Nintendo","Nintendo, Spike Chunsoft" = "Nintendo", "Paradox Development Studio" = "Paradox Interactive")


gamesales2010_2016$Developer <- recode(gamesales2010_2016$Developer,"PLAYGROUND, Playground Games" = "Playground Games", "Retro Studios, Entertainment Analysis & Development Division" = "Retro Studios", "Rockstar Leeds" = "Rockstar Studios", "Rockstar North" = "Rockstar Studios", "Rockstar San Diego" = "Rockstar Studios", "Sanzaru Games, Sanzaru Games, Inc." = "Sanzaru Games", "SCE Japan Studio, comcept" = "SCE Studio", "SCE Santa Monica" = "SCE Studio", "SCE Studio Cambridge" = "SCE Studio", "SCE Japan Studio" = "SCE Studio", "SCEA San Diego Studios" = "SCEA","SCEA, Zindagi Games" = "SCEA", "SCEE London Studio" = "SCEE")

gamesales2010_2016$Developer <- recode(gamesales2010_2016$Developer, "Sega Studios San Francisco" = "Sega", "Sega Toys" = "Sega", "Sega, Dimps Corporation" = "Sega", "Sega, French-Bread" = "Sega", "Sega, Sonic Team" = "Sega", "Snapdragon" = "Snap Dragon Games", "Sonic Team" = "Sega", "Sony Bend" = "Sony Interactive Entertainment", "Sony Online Entertainment" = "Sony Interactive Entertainment", "Spike Chunsoft" = "Spike", "Spike Chunsoft Co. Ltd., Spike Chunsoft" = "Spike","Tecmo" = "Tecmo Koei Games", "Tecmo Koei Canada" = "Tecmo Koei Games", "THQ Australia" = "THQ", "THQ Digital Studio Phoenix" = "THQ", "Ubisoft Casablanca" = "Ubisoft", "Ubisoft Milan" = "Ubisoft", "Ubisoft Montpellier" = "Ubisoft", "Ubisoft Montreal" = "Ubisoft", "Ubisoft Osaka" = "Ubisoft", "Ubisoft Paris" = "Ubisoft", "Ubisoft Paris, Ubisoft Montpellier" = "Ubisoft", "Ubisoft Quebec" = "Ubisoft", "Ubisoft Reflections" = "Ubisoft", "Ubisoft Reflections, Ivory Tower" = "Ubisoft", "Ubisoft Romania" = "Ubisoft", "Ubisoft Sofia" = "Ubisoft", "Ubisoft Toronto" = "Ubisoft", "Ubisoft Vancouver" = "Ubisoft", "Ubisoft, FunHouse" = "Ubisoft", "Ubisoft, Ludia Inc." = "Ubisoft", "Ubisoft, Ubisoft Montreal" = "Ubisoft") 


dev_global <- gamesales2010_2016 %>% 
#We want to filter out any unknown Developers as there are so few NA developers that will affect our variables.
  filter(Developer != "N/A") %>% 
#We will then group the columns according to the value in the developer columns.
  group_by(Developer) %>% 
#We sum up the total global sales and NA sales of each developers games earned.
  summarise(global_total = sum(Global_Sales)) %>% 
#We then arrange each row in descending order based on the global_total.
  arrange(desc(global_total)) %>% 
#We use the slice_head() function to take ONLY the top five most profitable developers.
  slice_head(n = 5) 
#We preview the data set.
head(dev_global) 


#We create a table which shows video games where the developers are the top five developers.
developer_genre_sale <- gamesales2010_2016 %>%
  filter(Developer %in% c("EA Games", "Ubisoft", "Nintendo", "Rockstar Studios", "Treyarch"))
 #We use this to preview the data set.
  head(developer_genre_sale)
  

#We use is.na() to determine if any values in Genre is N/A.
#Then use sum() to count the exact number of N/A values.
sum(is.na(developer_genre_sale$Genre))


#We store the developer_genre_sale data into df.
 df <- developer_genre_sale %>% 
#We then group by Developer and Genre.
  group_by(Developer,Genre) %>%  
#We summarise the NA and Global total for each category based on developer.
  summarise(developer_glbl_total = sum(Global_Sales), developer_NA_total = sum(NA_Sales)) %>% 
#We arrange the order in alphabetical order using the Developer column.
  arrange(Developer)
#We preview the data set.
head(df)


#GRAPHS 

#This code produces a graph in which it displays the sum of all Platforms in the 2010 to 2016 range.
ggplot(data = platform_NA_Sales, aes(x = reorder(Platform, NA_total), y = NA_total)) + geom_bar(stat = "identity") + labs(title = "NA sales by Platform", x = "Platform", y = "NA Sales") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#This code creates a bar graph that breaks up the total amount of NA sales for each of the different years of each different Platform.
ggplot(platform_SalesbyYear, aes(x = Year_of_Release, y = NA_total, fill = Platform)) + geom_bar(stat = "identity") + labs(title = "Total NA sales by Video Game Platform (2010-2016)", x = "Year", y = "NA Sales\n(In millions)")

#This code generates a lollipop graph in which it displays the total global sales of each platform in 2010 to 2016.
ggplot(platform_glbl_sales , aes(x = Platform, y = glbl_total, color = Platform)) + geom_point(stat = "identity") + geom_segment(aes( x = Platform, xend = Platform, y = 0, yend = glbl_total )) + labs(title = "Global Sales by Platform", x = "Platform", y = "Global Sales\n(in millions)")

#This code generates pie charts which shows the difference in sales between each platform from 2010 to 2016.
ggplot(data = platform_SalesbyYear, aes(x = 0, y = glbl_total, fill = Platform)) + geom_col(position = "fill") + facet_wrap(~Year_of_Release) + coord_polar(theta = "y") + theme_void() + labs(title = "Global Sales by Platform")

#This code generates a line graph that shows the rate of increase/decrease of each Platform from 2010 to 2016.
ggplot(data = platform_SalesbyYear, aes(x = Year_of_Release, y = glbl_total, color = Platform)) +
 geom_point() +
 geom_line(aes(group = Platform)) + labs(title = "Global Video Game Sales (2010-2016)", x = "Year", y = "Global Sales\n(in millions)")
 
#This code creates a bar graph that displays the overall sum of sales of each genre between 2010 to 2016.
ggplot(data = genre_na_sales, aes(x = reorder(Genre,genre_na_total), y = genre_na_total, fill = Genre)) + geom_bar(stat = "identity") + labs(title = "NA sales by Genre", x = "Genre", y = "NA Sales") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#We convert our numeric values in genre_na_sales into a percentage of the entire sum and store it into pie_labels.
pie_labels <- paste0(round(100 *genre_na_sales$genre_na_total/sum(genre_na_sales$genre_na_total), 2), "%")
#This code creates a pie graph, then displays the difference in NA sales each genre made out of 100% of the total sales made between 2010 to 2016
ggplot(genre_na_sales, aes(x = "", y = genre_na_total, fill = Genre)) + geom_col(color = "black") + geom_text(aes(label = pie_labels), position = position_stack(vjust = 0.5)) + coord_polar(theta = "y") + labs( y = "NA Sales", title = "NA Sales by Genre")

#This code creates a line graph that displays the change of NA sales based on each genre over the past 6 years.
ggplot(data = genre_SalesbyYear, aes(x = Year_of_Release, y = NA_total, color = Genre)) +
 geom_point() +
 geom_line(aes(group = Genre)) + labs(title = "NA Video Game Sales (2010-2016)", x = "Year", y = "NA Sales\n(in millions)")
 
#This code generates a bar graph that displays the total Global sales of each video game genre between 2010 to 2016.
ggplot(data = genre_glbl_sales, aes(x = Genre, y = genre_glbl_total, fill = Genre)) + geom_bar(stat = "identity") + labs(title = "Global Sales by Genre", x = "Genre", y = "Global Sales\n(In millions)") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#We convert our numerical values into percentages, then store into pvgsg.
PVGSG <- genre_glbl_sales %>% mutate(percentage=paste0(round(genre_glbl_total/sum(genre_glbl_total)*100, 2), "%")) 
PVGSG <- PVGSG %>% arrange(desc(genre_glbl_total))
#This code displays a stacked bar graph with a couple of functions at play.
ggplot(PVGSG, aes(fill = Genre, y = genre_glbl_total, x = "")) + geom_bar(position = "fill", stat = "identity")

#We convert our numeric values in genre_glbl_sales into a percentage of the entire sum and store it into pie_labels2.
pie_labels2 <- paste0(round(100*genre_glbl_sales$genre_glbl_total/sum(genre_glbl_sales$genre_glbl_total), 2), "%")
#This code creates a pie chart in which it displays the sum of global sale of video games between 2010 to 2016 in a percentage of the total sales made by genre.
ggplot(genre_glbl_sales, aes(x = "", y = genre_glbl_total, fill = Genre)) + geom_col(color = "black") + geom_text(aes(label = pie_labels2), position = position_stack(vjust = 0.5)) + coord_polar(theta = "y")

#This line creates a line chart that shows the decrease/increase of gloabl video game sales each year.
ggplot(data = genre_SalesbyYear, aes(x = Year_of_Release, y = glbl_total, color = Genre)) +
 geom_point() +
 geom_line(aes(group = Genre)) + labs(title = "Global Video Game Sales (2010-2016)", x = "Year", y = "Global Sales\n(in millions)")

#Overall, this code generates a bar chart that ranks the top 5 video game developers by their total global sales across all regions.
ggplot(dev_global, aes(x = reorder(Developer, global_total), y = global_total, fill = Developer)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Top 5 Developer Global Sales", x = "Developer", y = "Global Sales\n(In millions)")

#
#we convert our numeric values in genre_glbl_sales into a percentage of the entire sum and store it into pie_labels2.
pie_labels3 <- paste0(round(100*dev_global$global_total/sum(dev_global$global_total), 2), "%")
#Overall, this code generates a pie chart that ranks the top 5 video game developers by their total global sales across all regions.
ggplot(dev_global, aes(x = "", y = global_total, fill = Developer)) + geom_col(stat = "identity", width = 1, color = "black") + coord_polar(theta = "y") + theme_void() + geom_text(aes(label = pie_labels3), position = position_stack(vjust = 0.5)) + labs(title = " Top 5 Developer Sales")

#Overall, this code generates a stacked bar chart that shows the contribution of each video game genre to the global sales of the top 5 video game developers.
ggplot(df, aes( x = Developer, y = developer_glbl_total, fill = Genre)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(title = "Top 5 Developer Sales by Genre", x = "Developer", y = "Global Game Sales\n(In millions)")

#Overall, this code generates a polar bar chart that shows the relative contribution of each video game genre to the global sales of the top 5 video game developers.
ggplot(data = df, aes(x = 0, y = developer_glbl_total, fill = Genre)) + geom_col(position = "fill") + facet_wrap(~Developer) + coord_polar(theta = "y") + theme_void() + labs(title = "Top 5 Developer sales by Genre")

#Overall this code creates a scatter plot that shows how most categories are similar in sales.
ggplot(data = df, aes(x = Genre, y = developer_glbl_total, colour = Developer)) +
  geom_point() +
  labs(title = "Top 5 Developer sales by Genre", x = "Genre", y = "Global Game Sales\n(In millions)") + coord_flip()
  
  