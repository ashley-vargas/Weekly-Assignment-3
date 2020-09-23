---
title: 'Weekly Exercises #3'
author: "Ashley Vargas"
output: 
  html_document:
    keep_md: TRUE
    toc: TRUE
    toc_float: TRUE
    df_print: paged
    code_download: true
---


```{r setup, include=FALSE}
#knitr::opts_chunk$set(echo = TRUE, error=TRUE, message=FALSE, warning=FALSE)
```

```{r libraries}
library(tidyverse)     # for graphing and data cleaning
library(googlesheets4) # for reading googlesheet data
library(lubridate)     # for date manipulation
library(ggthemes)      # for even more plotting themes
library(geofacet)      # for special faceting with US map layout
gs4_deauth()           # To not have to authorize each time you knit.
theme_set(theme_minimal())       # My favorite ggplot() theme :)
```

```{r data}
#Lisa's garden data
garden_harvest <- read_sheet("https://docs.google.com/spreadsheets/d/1DekSazCzKqPS2jnGhKue7tLxRU3GVL1oxi-4bEM5IWw/edit?usp=sharing") %>% 
  mutate(date = ymd(date))

# Seeds/plants (and other garden supply) costs
supply_costs <- read_sheet("https://docs.google.com/spreadsheets/d/1dPVHwZgR9BxpigbHLnA0U99TtVHHQtUzNB9UR0wvb7o/edit?usp=sharing",
  col_types = "ccccnn")

# Planting dates and locations
plant_date_loc <- read_sheet("https://docs.google.com/spreadsheets/d/11YH0NtXQTncQbUse5wOsTtLSKAiNogjUA21jnX5Pnl4/edit?usp=sharing",
  col_types = "cccnDlc")%>% 
  mutate(date = ymd(date))

# Tidy Tuesday data
kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')
```

## Setting up on GitHub!

Before starting your assignment, you need to get yourself set up on GitHub and make sure GitHub is connected to R Studio. To do that, you should read the instruction (through the "Cloning a repo" section) and watch the video [here](https://github.com/llendway/github_for_collaboration/blob/master/github_for_collaboration.md). Then, do the following (if you get stuck on a step, don't worry, I will help! You can always get started on the homework and we can figure out the GitHub piece later):

* Create a repository on GitHub, giving it a nice name so you know it is for the 3rd weekly exercise assignment (follow the instructions in the document/video).  
* Copy the repo name so you can clone it to your computer. In R Studio, go to file --> New project --> Version control --> Git and follow the instructions from the document/video.  
* Download the code from this document and save it in the repository folder/project on your computer.  
* In R Studio, you should then see the .Rmd file in the upper right corner in the Git tab (along with the .Rproj file and probably .gitignore).  
* Check all the boxes of the files in the Git tab and choose commit.  
* In the commit window, write a commit message, something like "Initial upload" would be appropriate, and commit the files.  
* Either click the green up arrow in the commit window or close the commit window and click the green up arrow in the Git tab to push your changes to GitHub.  
* Refresh your GitHub page (online) and make sure the new documents have been pushed out.  
* Back in R Studio, knit the .Rmd file. When you do that, you should have two (as long as you didn't make any changes to the .Rmd file, in which case you might have three) files show up in the Git tab - an .html file and an .md file. The .md file is something we haven't seen before and is here because I included `keep_md: TRUE` in the YAML heading. The .md file is a markdown (NOT R Markdown) file that is an interim step to creating the html file. They are displayed fairly nicely in GitHub, so we want to keep it and look at it there. Click the boxes next to these two files, commit changes (remember to include a commit message), and push them (green up arrow).  
* As you work through your homework, save and commit often, push changes occasionally (maybe after you feel finished with an exercise?), and go check to see what the .md file looks like on GitHub.  
* If you have issues, let me know! This is new to many of you and may not be intuitive at first. But, I promise, you'll get the hang of it! 



## Instructions

* Put your name at the top of the document. 

* **For ALL graphs, you should include appropriate labels.** 

* Feel free to change the default theme, which I currently have set to `theme_minimal()`. 

* Use good coding practice. Read the short sections on good code with [pipes](https://style.tidyverse.org/pipes.html) and [ggplot2](https://style.tidyverse.org/ggplot2.html). **This is part of your grade!**

* When you are finished with ALL the exercises, uncomment the options at the top so your document looks nicer. Don't do it before then, or else you might miss some important warnings and messages.


## Warm-up exercises with garden data

These exercises will reiterate what you learned in the "Expanding the data wrangling toolkit" tutorial. If you haven't gone through the tutorial yet, you should do that first.

  1. Summarize the `garden_harvest` data to find the total harvest weight in pounds for each vegetable and day of week. Display the results so that the vegetables are rows but the days of the week are columns.


```{r}
garden_harvest %>% 
  mutate(day_of_week = wday(date, label = TRUE)) %>% 
  group_by(vegetable, day_of_week) %>% 
  summarize(total_weight_lb = sum(weight)*0.00220462) %>% 
  arrange(day_of_week, vegetable) %>% 
  pivot_wider(names_from = vegetable,
              values_from = total_weight_lb,
              values_fill = 0)
   
```

  2. Summarize the `garden_harvest` data to find the total harvest in pound for each vegetable variety and then try adding the `plot` variable from the `plant_date_loc` table. This will not turn out perfectly. What is the problem? How might you fix it?

```{r}
garden_harvest %>% 
  group_by(vegetable, variety) %>% 
  mutate(wt_lbs = weight* 0.00220462) %>% 
  summarize(daily_wt_lbs = sum(wt_lbs)) %>% 
  left_join(plant_date_loc,
            by = "variety")

```

This table shows two different vegetable categories when there should only be one category considering they provide the same data. I would keep the vegetable.x category simply because the vegetable.y category has a few missing vegetable names. To ensure that there is only one vegetable category, I would do a semi-join since this only includes rows with matching values in both tables and only keep columns from the first dataframe, eliminating the need for both a vegetable.x and vegetable.y row. 


  3. I would like to understand how much money I "saved" by gardening, for each vegetable type. Describe how I could use the `garden_harvest` and `supply_cost` datasets, along with data from somewhere like [this](https://products.wholefoodsmarket.com/search?sort=relevance&store=10542) to answer this question. You can answer this in words, referencing various join functions. You don't need R code but could provide some if it's helpful.
  
  
First I need to find the total number of each variety, and store that in a column. I would do this in a new dataset with the columns being vegetable variety, and total count of variety

Next, I would divide the total counts for each vegetable variety by its associating cost per packet; semi_joining the dataset we just created with the supply_cost on the variety name column. This will give me a dataset that contains each variety's total count and cost per vegetable.

Then I would join with data from Whole Foods, again on the variety name column, which gives me a single dataset with both homegrown and store bought prices to compare and aggregate as need be to find out which is cheaper and by how much

Note: This is all done with vegetable variety, not vegetable type. So, it's not lettuce but Crispy Colors Duo for example. This is all done with the assumption, that there is one variety packet of each kind. 
  
  
  
 
  
  

  4. Subset the data to tomatoes. Reorder the tomato varieties from smallest to largest first harvest date. Create a barplot of total harvest in pounds for each variety, in the new order.

```{r}
garden_harvest %>% 
  filter(vegetable  %in% c("tomatoes")) %>% 
  mutate(wt_lbs = weight*0.00220462) %>% 
  ggplot(aes(y = fct_infreq(variety))) +
  geom_bar() +
  labs(title = "Tomato Varieties",
       x = "Count",
       y = "Variety")

```

  5. In the `garden_harvest` data, create two new variables: one that makes the varieties lowercase and another that finds the length of the variety name. Arrange the data by vegetable and length of variety name (smallest to largest), with one row for each vegetable variety. HINT: use `str_to_lower()`, `str_length()`, and `distinct()`.
  
```{r}
garden_harvest %>% 
  mutate(variety_lowercase = str_to_lower(variety)) %>% 
  mutate(variety_length = str_length(variety)) %>% 
  arrange(vegetable, variety_length) %>% 
  distinct(vegetable,variety_lowercase, variety_length) 
 

```

  6. In the `garden_harvest` data, find all distinct vegetable varieties that have "er" or "ar" in their name. HINT: `str_detect()` with an "or" statement (use the | for "or") and `distinct()`.

```{r}
garden_harvest %>% 
  distinct(variety) %>% 
  mutate(has_r = str_detect(variety, "er|ar"))
    

```


## Bicycle-Use Patterns

In this activity, you'll examine some factors that may influence the use of bicycles in a bike-renting program.  The data come from Washington, DC and cover the last quarter of 2014.

<center>

![A typical Capital Bikeshare station. This one is at Florida and California, next to Pleasant Pops.](https://www.macalester.edu/~dshuman1/data/112/bike_station.jpg){300px}


![One of the vans used to redistribute bicycles to different stations.](https://www.macalester.edu/~dshuman1/data/112/bike_van.jpg){300px}

</center>

Two data tables are available:

- `Trips` contains records of individual rentals
- `Stations` gives the locations of the bike rental stations

Here is the code to read in the data. We do this a little differently than usualy, which is why it is included here rather than at the top of this file. To avoid repeatedly re-reading the files, start the data import chunk with `{r cache = TRUE}` rather than the usual `{r}`.

```{r cache=TRUE}
data_site <- 
  "https://www.macalester.edu/~dshuman1/data/112/2014-Q4-Trips-History-Data.rds" 
Trips <- readRDS(gzcon(url(data_site)))
Stations<-read_csv("http://www.macalester.edu/~dshuman1/data/112/DC-Stations.csv")
```

**NOTE:** The `Trips` data table is a random subset of 10,000 trips from the full quarterly data. Start with this small data table to develop your analysis commands. **When you have this working well, you should access the full data set of more than 600,000 events by removing `-Small` from the name of the `data_site`.**

### Temporal patterns

It's natural to expect that bikes are rented more at some times of day, some days of the week, some months of the year than others. The variable `sdate` gives the time (including the date) that the rental started. Make the following plots and interpret them:

  7. A density plot, which is a smoothed out histogram, of the events versus `sdate`. Use `geom_density()`.
  
```{r}
Trips %>% 
ggplot(aes(x=sdate)) +
  geom_density() + labs(title = "Bike Rentals Over Months",
      y= "Density",  
      x = "Month")

```
 
This graph shows that indeed, some bikes are rented more in certain months The density within each graph varies even within the month. However, overall this graph demonstrates that the bike rentals decrease from October to January. This can be attributed to the winter since it is more unlikely that someone will be riding a bike during the winter months.  
 
  
  8. A density plot of the events versus time of day.  You can use `mutate()` with `lubridate`'s  `hour()` and `minute()` functions to extract the hour of the day and minute within the hour from `sdate`. Hint: A minute is 1/60 of an hour, so create a variable where 3:30 is 3.5 and 3:45 is 3.75.
  
```{r}
Trips %>% 
  mutate(time_of_day = hour(sdate) + (minute(sdate)/60)) %>% 
  ggplot(aes(x = time_of_day)) +
  geom_density() + 
  labs(title = "Bike Rentals Over Time",
      y= "Density",  
      x = "Time of Day")

```
 
This density plot has a bimodal distribution. As we can see, bike rentals are quite slow at the beginning and end of this graph. The peak times for bike rentals are around 8 and 17. This may be attributed to the fact that these are the times in the day where people are going and getting out of school and work which can require people to rent a bike to get home . 
 
 
 
 
  
  9. A bar graph of the events versus day of the week. Put day on the y-axis.
  
```{r}
Trips %>% 
  mutate(day_of_week = wday(sdate, label = TRUE)) %>% 
  ggplot(aes(y = day_of_week)) +
  geom_bar() + 
  labs(title = "Bike Rentals Over Day of the Week",
      x= "Count",  
      y = "Day of the Week")

```
  
This bar graph shows how bike rentals are pretty much evenly distributed throughout the week. There isn't a day that had a dramatically lower bike rental count. However, Saturdays and Sundays do have the lowest count of bike rentals, which can possibly attributed to the fact that people do not need bikes to get to school or work on the weekends.   
  
  
  
  10. Facet your graph from exercise 8. by day of the week. Is there a pattern?
  
```{r}
Trips %>% 
  mutate(day_of_week = wday(sdate, label = TRUE)) %>% 
  mutate(time_of_day = hour(sdate) + (minute(sdate)/60)) %>% 
  ggplot(aes(x = time_of_day)) +
  geom_density() +
  facet_wrap(~ day_of_week) +
 labs(title = "Bike Rentals Over Day of the Week",
      x= "Time of Day",  
      y = "Density")

```
 
Yes. This graph shows the pattern that I described in the previous graph more clearly. We can see that on Monday-Friday, the distributions are nearly identical. On the other hand, there are fewer bike rentals on the weekend. Saturdays and Sundays have a similar density graph to each other. On the weekdays, there appears to be a bimodal distribution, while on the weekends, there is a normal distribution. 
 
 
 
  
The variable `client` describes whether the renter is a regular user (level `Registered`) or has not joined the bike-rental organization (`Causal`). The next set of exercises investigate whether these two different categories of users show different rental behavior and how `client` interacts with the patterns you found in the previous exercises. Repeat the graphic from Exercise \@ref(exr:exr-temp) (d) with the following changes:

  11. Change the graph from exercise 10 to set the `fill` aesthetic for `geom_density()` to the `client` variable. You should also set `alpha = .5` for transparency and `color=NA` to suppress the outline of the density function.
  
```{r}
Trips %>% 
  mutate(day_of_week = wday(sdate, label = TRUE)) %>% 
  mutate(time_of_day = hour(sdate) + (minute(sdate)/60)) %>% 
  ggplot(aes(x = time_of_day, fill = client)) +
  geom_density(color=NA, alpha = 0.5) +
  facet_wrap(~ day_of_week)  +
  labs(title = "Bike Rentals Over Time of Day",
      x= "Time of Day",  
      y = "Density")
```

This graph shows us the differences between casual and registered riders. We can compare the density in bike rentals by casual and registered riders and compare them at different times of the day and throughout each day of the week. Unlike the previous graph, which only showed the distribution among different days and times, this graph now includes the client variable to allow for a comparison between clients. Yet, we can still see similar features of the previous graph: patterns in the days of the week and times. 






  12. Change the previous graph by adding the argument `position = position_stack()` to `geom_density()`. In your opinion, is this better or worse in terms of telling a story? What are the advantages/disadvantages of each?
  
```{r}

Trips %>% 
  mutate(day_of_week = wday(sdate, label = TRUE)) %>% 
  mutate(time_of_day = hour(sdate) + (minute(sdate)/60)) %>% 
  ggplot(aes(x = time_of_day, fill = client)) +
  geom_density(color=NA, alpha = 0.5, position = position_stack()) +
  facet_wrap(~ day_of_week) +
  labs(title = "Bike Rentals Over Time of Day",
      x= "Time of Day",  
      y = "Density")


```


I believe this graph is slightly worse at telling the story because this graph makes it harder for me to compare casual versus registered riders. In the second graph, we can the data a bit clearer simply because the graphs are not overlapping. Yet, I am able visualize the data better through the first graph better because I can see the differences in distribution for each rider more clearly. The second graphs make the distribution look very similar and makes it difficult for me to analyze the differences.
  
  
  
  13. Add a new variable to the dataset called `weekend` which will be "weekend" if the day is Saturday or Sunday and  "weekday" otherwise (HINT: use the `ifelse()` function and the `wday()` function from `lubridate`). Then, update the graph from the previous problem by faceting on the new `weekend` variable. 
  
```{r}
Trips %>% 
  mutate(day_of_week = wday(sdate, label = TRUE)) %>% 
  mutate(time_of_day = hour(sdate) + (minute(sdate)/60)) %>% 
  mutate(weekend = ifelse(day_of_week %in% c("Sat", "Sun"), "weekend", "weekday")) %>% 
  ggplot(aes(x = time_of_day, fill = client)) +
  geom_density( color="NA", alpha = .5) +
  facet_wrap(~weekend) +
   labs(title = "Bike Rentals Over Time of Day",
      x= "Time of Day",  
      y = "Density")
```
 

This graph shows us a comparison between casual and registered riders depending on the weekend or weekday. On the weekends, both riders have a similar normal distribution, while on the weekdays, there are evident differences in their bike rides.The distribution for registered riders during the week is bimodal, while there is a normal distribution for casual riders. I like this graph because it allows me to clearly see the differences in clients by day without much confusion considering weekdays and weekends are made into just two graphs. 

  14. Change the graph from the previous problem to facet on `client` and fill with `weekday`. What information does this graph tell you that the previous didn't? Is one graph better than the other?
  
```{r}
Trips %>% 
  mutate(day_of_week = wday(sdate, label = TRUE)) %>% 
  mutate(time_of_day = hour(sdate) + (minute(sdate)/60)) %>% 
  mutate(weekday = ifelse(day_of_week %in% c("Mon", "Tues", "Wed", "Thurs", "Fri"), 
        "weekday", "weekend")) %>% 
  ggplot(aes(x = time_of_day, fill = weekday)) +
  geom_density(color="NA", alpha = .5) +
  facet_wrap(~client) +
   labs(title = "Bike Rentals Over Time of Day",
      x= "Time of Day",  
      y = "Density")

```
  
  
This graph shows me a better comparison of casual versus registered riders. For instance, I am able to see that casual riders have a similar activity on weekends and weekdays which is not as visible in the previous graph. The spikes in the density graph for the registered riders are more evident in this graph. I do not believe one graph is better than the other, I simply think it depends on what information you are looking for. This graph is best if you are comparing between weekend and weekday bike rides, rather than comparing casual and registered riders directly to each other. 
  
  
  
### Spatial patterns

  15. Use the latitude and longitude variables in `Stations` to make a visualization of the total number of departures from each station in the `Trips` data. Use either color or size to show the variation in number of departures. We will improve this plot next week when we learn about maps!
  
```{r}
stations1 <- Trips %>% 
  left_join(Stations,
        by = c( "sstation" = "name")) %>% 
        group_by(long, lat) %>% 
        summarise(total_departures = n(),
        proportions_casual = mean(client == "Casual")) 
  
stations1 %>%         
        ggplot(aes(x = long, y=lat, color = total_departures)) +
        geom_point() +
        scale_color_gradient(low = "lightblue", high = "blue") +
         labs(title = "Total number of Departures From Each Station",
      x= "Longitude",  
      y = "Latitude")

```
  
  
Through this graph, we can see how the total departures number is mainly in the 400 category with very few total departures in the larger categories of 800-1200. Most points are cluttered closely together, with a few scattered on their own.



  16. Only 14.4% of the trips in our data are carried out by casual users. Create a plot that shows which area(s) have stations with a much higher percentage of departures by casual users. What patterns do you notice? (Again, we'll improve this next week when we learn about maps).
  
  
```{r}
stations1 %>% 
        ggplot(aes(x = long, y=lat, color = proportions_casual)) +
        geom_point() +
        scale_color_gradient(low = "lightblue", high = "blue") +
        labs(title = "Proportions of Departures in Stations By Casual Users",
        x= "Longitude",  
        y = "Latitude")
```


As indicated, this graph shows us that the proportions of casual users in various stations is very low, all mostly within the 0.0-0.2 categories. Like the previous graph, most points are cluttered very closely together, with a few scattered on the top left corner. 

  
### Spatiotemporal patterns

  17. Make a table with the ten station-date combinations (e.g., 14th & V St., 2014-10-14) with the highest number of departures, sorted from most departures to fewest. Save this to a new dataset and print out the dataset. Hint: `as_date(sdate)` converts `sdate` from date-time format to date format. 
  
```{r}
Trips2 <- Trips %>% 
  mutate(trip_date = as_date(sdate)) %>% 
  group_by(trip_date, sstation) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(n = 10)

Trips2
```
  
  18. Use a join operation to make a table with only those trips whose departures match those top ten station-date combinations from the previous part.
  
```{r}
Trips %>% 
   mutate(trip_date = as_date(sdate)) %>% 
   inner_join(Trips2, 
            by = c("sstation", "trip_date"))

```
  
  19. Build on the code from the previous problem (ie. copy that code below and then %>% into the next step.) and group the trips by client type and day of the week (use the name, not the number). Find the proportion of trips by day within each client type (ie. the proportions for all 7 days within each client type add up to 1). Display your results so day of week is a column and there is a column for each client type. Interpret your results.
  
    
```{r}
Trips %>% 
   mutate(trip_date = as_date(sdate)) %>% 
   inner_join(Trips2, 
            by = c("sstation", "trip_date")) %>% 
  mutate(day_week = wday(sdate, label = TRUE)) %>%
  group_by(day_week, client) %>% 
  summarise(rider_count = n()) %>% 
  mutate(total_prop = rider_count/sum(rider_count)) %>% 
  pivot_wider(id_cols = day_week,
              names_from = client,
              values_from = total_prop)


```

This table shows us the proportion of casual versus registered riders during each day of the week. Casual drivers only use bike rentals at a higher proportion than registered riders on the weekends. This would make sense because the weekends are for more leisure activities which would might prompt an individual to rent a bike spontaneously. On every single weekday there is a drastic difference between the number of casual versus registered riders; registered riders are the ones using the bike rentals at much higher proportions. 
  

**DID YOU REMEMBER TO GO BACK AND CHANGE THIS SET OF EXERCISES TO THE LARGER DATASET? IF NOT, DO THAT NOW.**

## GitHub link

  20. Below, provide a link to your GitHub page with this set of Weekly Exercises. Specifically, if the name of the file is 03_exercises.Rmd, provide a link to the 03_exercises.md file, which is the one that will be most readable on GitHub.

## Challenge problem! 

This problem uses the data from the Tidy Tuesday competition this week, `kids`. If you need to refresh your memory on the data, read about it [here](https://github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-09-15/readme.md). 

  21. In this exercise, you are going to try to replicate the graph below, created by Georgios Karamanis. I'm sure you can find the exact code on GitHub somewhere, but **DON'T DO THAT!** You will only be graded for putting an effort into this problem. So, give it a try and see how far you can get without doing too much googling. HINT: use `facet_geo()`. The graphic won't load below since it came from a location on my computer. So, you'll have to reference the original html on the moodle page to see it.
  
```{r fig.width=15, fig.height=10}
kids %>% 
  group_by(state) %>%
  filter(variable %in% "lib") %>% 
  ggplot(aes(x = year, y = inf_adj_perchild)) +
  geom_line(color = "white") +
  theme(plot.background = element_rect(fill = "lightsteelblue4")) +
  facet_geo(~state, scales = "free") +
  labs(title = "Change in public spending on libraries from 1997 to 2016", 
       subtitle = "Dollars spend per child, adjusted for inflation",
       caption = "Source: Urban Institute, Table: Georgios Karamanis") +
        theme(plot.title = element_text(hjust = 0.5, size = 25)) +
        theme(plot.subtitle = element_text(hjust = 0.5, size = 20))
```