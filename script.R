# R PROJECT - Dan and Quentin - Perceived usefulness of work by employees: The Case of European Union
# Countries for the Year 2015 with France as the reference Country

rm(list=ls())

library("ggplot2")
library("eurostat")
library("dplyr")
library("prettyR")
library("tidyverse")
library("lubridate")
library("prettyR")
library("Epi")
library("e1071")


# OBJECTIVE: FROM A DATABASE CONTAINING A LOT OF INFORMATION,
# WE NEED TO EXTRACT A SUBSET WITH 2 VARIABLES (AS REQUESTED IN THE INSTRUCTION)
# THAT MAXIMIZES THE INFORMATION CONTAINED IN THE INITIAL DATABASE. TO DO THIS,
# WE WILL FOCUS ON THE YEAR 2015 WITH FRANCE AS THE COMPARISON COUNTRY."



# PART 1: DATABASE AND DATA ARRANGEMENT
# We want to work on usefulness work perception, focusing particularly on EU countries. 
# To do this, we are looking for a database on Eurostat that is compatible with our work:"

# Information search  
search_eurostat("useful work") %>%   #We use the term useful work for the idea of 
                                     #usefulness work perception by employees
select(title, code)

# Database import
base = get_eurostat("qoe_ewcs_7b3")


# We will arrange our database by selecting only 
# people aged 25 to 64, regardless of gender.

base = base %>%
  filter(age == "Y25-64",
         sex %in% c("M","F"))

# Then we select only the year 2015 and rename the variables.
base = base %>%
  mutate(time = year(time)) %>%
  filter(time =="2015") %>%
  select(time, geo, sex, values) %>%
  rename(years = time, countries = geo, gender = sex, useful_work = values)


# Finally, we remove the 'year' variable as it contains no information; 
# our analysis is only focused on the year 2015
base = base %>%
  select(-years)




# Finally, here is our main database containing 3 variables:
# The indicator variable "gender," which currently takes the value "M" or "F,"
# and the variable "useful_work," which ranges from 0 to 100, indicating
# the percentage of useful work / perceived usefulness of work  by country. The variable "countries" 
# lists the different countries for which observations are made.

head(base)


# To avoid biasing our analysis, we will remove the observations 
# related to the EU average for the relevant period

unique(base$countries) # remove of "EU27_2020" et "EU28"
which(base$countries == "EU28") # Removing rows 12 and 42
which(base$countries == "EU27_2020") # Removing rows 11 and 41

base = base[-c(11, 12, 41, 42), ] 



# Finally, we will calculate the average useful work % 
# from our database, first by gender (across all EU countries), 
# and then by country (regardless of gender).

# Average useful work by gender (average of the 28 EU countries in 2015)

mean_gender = base %>%
  group_by(gender) %>%
  summarize(mean(useful_work, na.rm =T))

# Average useful work by country (without gender differentiation)

mean_countries = base %>%
  group_by(countries) %>%
  summarize( mean = mean(useful_work, na.rm = T))

# Now we want to plot an map of Europe colorized showing which countries 
# have people in employment who believe their work is meaningful/useful.

install.packages("sf")
install.packages("cowplot")
install.packages("ggthemes")


library(tidyverse)
library(eurostat)
library(leaflet)
library(sf)
library(scales)
library(cowplot)
library(ggthemes)







# We integrate a 'shapefile' that will provide us with the map of Europe.

get_eurostat_geospatial(resolution = 10, 
                        nuts_level = 0, 
                        year = 2016)



# We download EU countries shapfiles

Map_EU <- get_eurostat_geospatial(resolution = 10, 
                                 nuts_level = 0, 
                                 year = 2016)


Map_EU %>% 
  ggplot() +
  geom_sf()


# We create a dataframe that assigns a code to each country.

EU28 <- eu_countries %>%
  select(geo = code, name) %>% 
  rename(countries = geo)

# We Include the United Kingdom (UK) in EU28
EU28 <- rbind(EU28, data.frame(countries = "UK", name = "United Kingdom"))




# We create a dataframe with the 28 countries that we can display as a map/shapefile
# We join Eu28 and EU_EU by countries (countries taht take NUTS_ID value )

Map_EU28 <- Map_EU %>% 
  select(countries = NUTS_ID, geometry) %>% 
  inner_join(EU28, by = "countries") %>% 
  arrange(countries) %>% 
  st_as_sf()


# We zoom on Europe by using scal_ command to crop the Map 

Map_EU28 %>% 
  ggplot() +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void()


# We want a representation of mean_countries (the variables countries and mean)
# on the map of Europe. We merge it with 'Map_EU28' using the countries variable.

mean_countries_map <- mean_countries %>% 
  select(countries, mean) %>% 
  inner_join(Map_EU28, by = "countries") %>% 
  st_as_sf()

mean_countries_map%>% 
  ggplot(aes(fill = mean)) +
  geom_sf() +                   
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void()


mean_countries_map%>% 
  ggplot(aes(fill = mean)) +
  geom_sf() +
  scale_fill_continuous(
    type = "viridis",
    name = "% useful work",                    # title of the legend
    guide = guide_colorbar(
      direction = "vertical",             # vertical colorbar
      title.position = "top",             # The title displayed at the top
      label.position = "right",           # labels displayed at the right side
      barwidth = unit(0.4, "cm"),         # width of the colorbar
      barheight = unit(5, "cm"),          # height of the colorbar
      ticks = TRUE,                       # ticks are displayed
    )
  ) + 
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  theme_void()+
  labs(
    title = "Perceived usefulness of work by employees",
    subtitle = "As a percentage in various European countries (EU28) in 2015",
    caption = "Data: Eurostat qoe_ewcs_7b3"
  ) +
  theme(
    legend.position = c(1.2, 0.40)
  )



# Now, we want to represent the disparities in the perceived usefulness of work 
# by employees in the form of horizontal bars for the 28 countries.

# We start by first assigning each country code to its full name.

full_name <- c("AT" = "Austria", "BE" = "Belgium", "BG" = "Bulgaria", "CY" = "Cyprus", "CZ" = "Czech Republic",
               "DE" = "Germany", "DK" = "Denmark", "EE" = "Estonia", "EL" = "Greece", "ES" = "Spain",
               "FI" = "Finland", "FR" = "France", "HR" = "Croatia", "HU" = "Hungary", "IE" = "Ireland",
               "IT" = "Italy", "LT" = "Lithuania", "LU" = "Luxembourg", "LV" = "Latvia", "MT" = "Malta",
               "NL" = "Netherlands", "PL" = "Poland", "PT" = "Portugal", "RO" = "Romania", "SE" = "Sweden",
               "SI" = "Slovenia", "SK" = "Slovakia", "UK" = "United Kingdom")



# We add the full_name variable into the mean_countries dataframe
mean_countries <- mean_countries %>% mutate(full_name = full_name[countries])






#Now we can plot a bar/horizontal histogramm of perceived usefulness of work/useful work per country

mean_countries_bar <- mean_countries %>% 
  ggplot(aes(x = reorder(full_name, mean), y = mean, fill = mean)) +  # The "reorder" function allows reordering in descending order.
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # blue gradient with scale_ function
  labs(title = "Perceived usefulness of work by employees",
       subtitle = "In EU28 (including UK)",
       x = "Countries",
       y = "% useful work") + 
  theme_bw() + 
  theme(legend.title = element_blank()) +  # removes the title of the legend in a plot.
  coord_flip()                             #  Arrange the bars horizontally.
mean_countries_bar




# PART 2: DESCRIPTIVE STATISTICS AND GLOBAL TRENDS

str(base) 

# First, we will transform the "gender" variable into a binary variable, 
# taking the value 0 for "F" and 1 for "H". 
# Then, we will factorize the "countries" variable. 

base$gender = factor(base$gender, levels= c("F","M"), labels = c(0,1))
base$countries = factor(base$countries)

str(base) # The transformation has been successfully performed, and we can see 
#the term "factor" next to the two variables, indicating they have been factorized.

# Let's focus on the sample size: 

prop.table(table(base$gender))*100 # as many men as women
unique(base$countries) # the 28 EU countries in 2015 (including the UK)

# Let's focus on the variable useful_work : 

summary(base$useful_work)


# the average useful work rate in the EU in 2015 was 86%, 
# with a median close to 86.45%. The interquartile range is 7.3%. 
# The mode is reached at a useful work level of 82.4%. Since the mode is less than the median, 
# the distribution of useful work rates is left-skewed.

skewness(base$useful_work) 

# The negative skewness value indicates a slight leftward skew 
# in the distribution of useful work. 
# Let's verify this graphically:

hist(base$useful_work)


qqnorm(base$useful_work);qqline(base$useful_work) 
# By excluding outliers and extreme values, we can say that the "useful_work" 
# variable follows a normal distribution. This condition is important
# for performing the following statistical tests. 


# PART 3: COMPARISON AND INFERENCE
# A)
# We will focus on the difference in useful work rates between genders 
# for the 28 EU countries (including the UK because we are in 2015).

mean_gender 
# The average useful work rate is 87% for women and 85% for men. 
# Despite a 2-percentage point difference, there doesn't 
# appear to be a gender-specific effect on useful work on average.

# We can calculate the relative risk and odds ratio, but to do that, we need 
# to binarize the useful_work variable. 
# Since the average is 86%, all values below this average will be assigned 0 ("low usefulness"), 
# and all values greater than or equal to the median will be assigned 1 ("high usefulness").

# NOTE: Useful work/perceived usefulness of work is a subjective measure, and there is no objective
# threshold for this indicator. The value of 86% has been set arbitrarily.


base = base %>%
  mutate (useful_work.b = ifelse(base$useful_work<86, 0,1))


table(base$useful_work.b) # 26 countries are affected by "low usefulness.".

# Let's calculate the relative risk and odds ratio: 
table(base$useful_work.b,base$gender, deparse.level = 2)
twoby2(base$gender, base$useful_work.b)

# With these results, we can say that 32% of women are dissatisfied at work, while 60% 
# of men are. Being a man increases the risk of job dissatisfaction by 2 times, 
# or in other words, women are 2 times less likely to be dissatisfied at work (relative risk = 0.52).

# Considering that job not useful is not rare, the odds ratio of 0.3065 is not interpretable 

# Note that the p-value is 6%, making it challenging to statistically validate
# these two ratios. Let's verify this with a t-test.


# We have previously seen that the average useful work between men and 
# women differs by 2 percentage points. Let's perform a test of means using 
# the Student's t-test. To use this test, the variance (or standard deviation) 
# of the two groups should be roughly similar, and the useful_work variable 
# should be normally distributed (which was validated earlier).

aggregate(useful_work~gender, base, sd) # the difference is 0.5 points: we accept.
# to perform the t-test for means.. 

# H0 ==> M(useful_work/H) == M(useful_work/F)
# H1 ==> M(useful_work/H) != M(useful_work/F)

t.test(base$useful_work~base$gender, var.equal = T)
# The p-value is 13.46%, and we conclude that there is not a statistically 
# significant difference between the two means. 

# CONCLUSION 1: CONTRARY TO WHAT WE MIGHT HAVE EXPECTED, THERE IS NO 
# STATISTICALLY SIGNIFICANT DIFFERENCE IN AVERAGE USEFUL WORK % OR PERCEIVED USEFULNESS OF WORK
# BETWEEN MEN AND WOMEN.




# We'll choose the 10 countries statistically significant . 
# We introduce the gender (m/f) within the countries to observe 
# the difference in perceived usefulness of work (useful work rate/%) based on gender and by country."





gender_countries = base %>%
  group_by(countries) %>%
  summarize( mean = mean(useful_work, na.rm = T))



gender_countries = subset(base, countries %in% c("FR", "DK", "EL", "HR", "LT", "MT", "PL", "SE", "SK","UK"), 
                          select=c(countries, useful_work, gender))

gender_countries <- gender_countries %>%
  mutate(
    full_name = case_when(
      countries == "FR" ~ "France",
      countries == "DK" ~ "Denmark",
      countries == "EL" ~ "Greece",
      countries == "HR" ~ "Croatia",
      countries == "LT" ~ "Lithuania",
      countries == "MT" ~ "Malta",
      countries == "PL" ~ "Poland",
      countries == "SE" ~ "Sweden",
      countries == "SK" ~ "Slovakia",
      countries == "UK" ~ "United Kingdom",
      TRUE ~ NA_character_  # Manage the default (not found) case
    )
    
  )



gender_countries %>% 
  ggplot(aes(x = gender, y = useful_work, fill = gender)) +
  geom_col(stat = "identity") +
  facet_wrap(~full_name) +
  labs(
    title = "Perceived usefulness of work by Country and Gender",
    subtitle = "In 10 European countries",
    x = "Gender",
    y = "meaningful work %"
  ) +
  theme_bw()        #theme_bw is function that applies a simple black and white theme to a plot with a white background, black lines, and tex





# Regarding the average useful work rate between countries:

describe(mean_countries$mean) 
# The average usful work rate across all EU countries in 2015 was 86% 
# with a standard deviation of 4.75 

min(mean_countries$mean)
max(mean_countries$mean)
# Poland has the lowest useful work rate (75.3%), 
# while Malta has the highest rate at 95.75%.



# B) 
# Let's focus particularly on the average useful work of countries compared to France

base$countries = relevel(base$countries, ref = "FR")
# The variable 'countries' is an indicator variable
# and we take France as the reference." 

model = lm(useful_work~countries, base)
summary(model)
# We will select only the coefficients that are statistically significant 
# at a 5% threshold: 'DK,' 'EL,' 'HR,' 'LT,' 'MT,' 'PL,' 'SE,' 'SK,' and 'UK' 

# Let's check the overall significance of the "countries" variable : 

drop1(model, .~., test = "F")
# This variable is statistically significant with a p-value much lower than 0.0001%.
# So there is indeed a differentiated effect of the country on the average useful work %.

# CONCLUSION 2: Even for countries with relatively similar economies,
# the useful work rate in 2015 varies statistically within the EU.


# In line with the two conclusions we have outlined, we can, on the one hand, 
# nremove gender, which has no effect on perceived usefulness of work (useful work %), and, on the other hand, 
# select 8 countries (with France as the reference) that have statistically 
# significant useful work rates.


general_useful = subset(base, countries %in% c("FR", "DK", "EL", "HR", "LT", "MT", "PL", "SE", "SK","UK"), 
                       select=c(countries, useful_work))

general_useful = general_useful %>%
  group_by(countries) %>%
  summarize(mean = mean(useful_work))


# This final dataset, titled "general_useful," is a subset of our original 
# database that maximizes the information available. Therefore, 
# the goal of this work has been achieved. 



# CONCLUSION: READING of OUR FINAL DATAFRAME

# Create several graphs from the "general_useful" dataframe and 
# explain the main trends in this dataframe

library(countrycode)
library(ggplot2)


general_useful <- general_useful %>%
  mutate(
    full_name = case_when(
      countries == "FR" ~ "France",
      countries == "DK" ~ "Denmark",
      countries == "EL" ~ "Greece",
      countries == "HR" ~ "Croatia",
      countries == "LT" ~ "Lithuania",
      countries == "MT" ~ "Malta",
      countries == "PL" ~ "Poland",
      countries == "SE" ~ "Sweden",
      countries == "SK" ~ "Slovakia",
      countries == "UK" ~ "United Kingdom",
      TRUE ~ NA_character_  
    )
    
  )


# Here I created a horizontal bar graph, sorted in descending order, 
# with Satisfaction % on the x-axis and the different countries on the y-axis

general_useful_bar <- general_useful %>% 
  ggplot(aes(x = reorder(full_name, mean), y = mean, fill = mean)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Blue gradient
  labs(title = "Perceived usefulness of work by employees and Country",
       subtitle = "In 10 European Countries (including UK)",
       x = "Countries",
       y = "% useful work",
       caption = "Data: Eurostat qoe_ewcs_7b3") +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  coord_flip()
general_useful_bar







































