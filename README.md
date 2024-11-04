# Project 2
- This was a project completed from Gapminder Data in an Advanced R Course
- Gapminder World Health data was used in this project to examine Life Expectancy,
  trends in GDP per capita as well as Infant Mortality across countries and continents.
- Population rank charts was also discussed based on GDP per capita and infant mortality rates.







# Introduction





Global health is defined as an area of study, practice and research focused on developing overall equity in health for people all around the world [@Rutgers2022]. More specifically, global health interventions are directed to vulnerable populations in an effort to minimize health disparities especially among low-resource countries [@DukeUniversity2022]. Over the years, global data concerning health and economic trends have been captured through online databases such as Gapminder. Gapminder prides itself as an "independent Swedish foundation with no political, religious, or economic affiliations" [@Gapminder2006]. The organization's objective is to detect misconceptions concerning global trends by presenting factual data about countries, in order to educate the masses and eradicate misinformation that exists in society. Gapminder World Health data will be used in this paper to examine `Life Expectancy`, trends in `GDP per capita` as well as `Infant Mortality` across countries and continents. Population rank charts will also be discussed based on GDP per capita and infant mortality rates. This project is based off curated data from the Gapminder website including predictive values for certain variables.





# Methods





GapMinder World Health data was curated from raw data into a more complete dataset than the initial 'dslabs' download in Project 1. A total of 6 datasets were combined in this project using the tidyverse package. The curated dataset contains statistics for 230 countries examined across 10 variables between 1799 to 2099.

-   **Hypothesis 1:** `Life Expectancy` increases as `GDP per capita` increases. Data from the year 2011 (the most recent year with complete data) was used to create a bubble chart in @fig-bubble-chart by plotting GDP per capita (x) versus life expectancy in years (y) for each continent represented.

-   **Hypothesis 2:** `Life Expectancy` increases over time as `GDP per capita` increases. A bubble chart was also created depicting the life expectancy of countries at 10 year increasing increments in @fig-bubble-chart-time.

-   **Hypothesis 3:** `GDP per capita` increases over time in more economically stable countries.A growth charts was plotted in @fig-gdppc showing changes in GDP per capita for selected countries.

-   **Hypothesis 4:** `Infant mortality` decreases over time as the country develops. In @fig-inf-mort, infant mortality rates are plotted against time in years.

-   **Hypothesis 5:** Countries with larger populations will have lower GDP per capita. `GDP per capita` values were ranked according to population for the top 10 countries in @fig-pop-rank-gdp.

-   **Hypothesis 6:** Countries that are less densely populated, have higher infant mortality rates. The top 10 countries with the highest infant mortality rates were ranked and distinguish by their populations in @fig-inf-rank. The viridis package was used in hypothesis 5 & 6 testing to accurately distinguish variations in `GDP per capita` and `Population`.

-   **Hypothesis 7:** In @fig-bubble-chart-years-time A greater change in `Life Expectancy` is seen over `GDP per capita` in a larger time scale compared to Figure 2.

-   **Hypothesis 8:** Countries with higher `Infant mortality` have lower `GDP per capita` rates than those with lower `Infant mortality` values. The results are depicted in @fig-infmort-gdp.





# Results





## Merging of data




```{r data-downloads}
#| results: hide


#install.packages("tidyverse")
# Contains color palette for bar plot
#install.packages("viridis")
#Contains original Gapminder dataset from Project 1
# install.packages("dslabs")


library(dslabs)
library(tidyverse)
library(viridis)

# CSV file was downloaded via Canvas for "Continent/Region" file. 
country_regions_table_20220628 <- read_csv("raw_data/country_regions_table_20220628.csv", na = c("N/A", ""))

# CSV file is downloaded via Gapminder website for "Population,total".
population_total <- read_csv("raw_data/population_total.csv")

# CSV file is downloaded via Gapminder website for "Babies per woman (total fertility)".
children_per_woman_total_fertility <- read_csv("raw_data/children_per_woman_total_fertility.csv")

# CSV file is downloaded via Gapminder website for "Life expectancy (years)".
life_expectancy_years <- read_csv("raw_data/life_expectancy_years.csv")

# CSV file is downloaded via Gapminder website for "Infant mortality (rate per 1000 births)".
inf_mort_rate <- read_csv("raw_data/infant_mortality_rate_per_1000_births.csv")

# CSV file is downloaded via Gapminder website for "Total GDP(PPP$, inflation-adjusted)".
total_gdp_ppp_inf_adj <- read_csv("raw_data/total_gdp_ppp_inflation_adjusted.csv")

```

```{r country-reg-setup}
#| results: hide

# Gives a snapshot of variables
# glimpse(country_regions_table_20220628)

# Country & Regions data stored in this environment
country_reg <- as_tibble(country_regions_table_20220628)
country_reg2 <- country_reg |>
  # selects only data variables required
  select(country,continent,region)
  
```

```{r total-fertility-setup}
#| results: hide

# Fertility(total) data stored in this environment
total_fertility <- as_tibble(children_per_woman_total_fertility) %>%
  # Performed data tidying 
  pivot_longer(!country,
  names_to = "year",
  values_to = "Fertility")

# Gives a snapshot of variables
# glimpse(total_fertility)

```

```{r pop-total-setup}
#| results: hide

# Used to visualize dataset
# glimpse(population_total)

# Population data is contained in this environment
pop_total <- as_tibble(population_total) %>% 
  # Performed data tidying 
  pivot_longer(!country,
  names_to = "year",
  values_to = "Population")

# str(pop_total)

# Used to examine unique values & characters in this variable
# unique(pop_total)


```

```{r inf-mort-rate-setup}
#| results: hide


# Infant mortality data stored in this environment
infant_mort <- as_tibble(inf_mort_rate) %>% 
  pivot_longer(!country,
  names_to = "year",
  values_to = "infant_mortality") 

# Gives a snapshot of variables
# glimpse(infant_mort)

```

```{r life-exp-setup}
#| results: hide


# Used to visualize dataset
# glimpse(life_expectancy_years)

life_expec <- as_tibble(life_expectancy_years) %>% 
  # Performed data tidying
  pivot_longer(!country,
  names_to = "year",
  values_to = "life_expectancy")

# glimpse(life_expec)
```

```{r gdp-adj-setup}
#| results: hide


# Used to visualize dataset
# glimpse(total_gdp_ppp_inf_adj)

# GDP data stored in this environment
gdp_adj <- as_tibble(total_gdp_ppp_inf_adj) %>% 
  # Performed data tidying
  pivot_longer(!country,
  names_to = "year",
  values_to = "GDP")

# glimpse(gdp_adj)

```

```{r joining}

# Full_join() used to combine 'country/region' data with 'life expectancy'
# data because of the predictive values it contained (1799-2099)
datafull <- full_join(country_reg2,life_expec)
# Gives a snapshot of variables
# glimpse (datafull)

# Full_join() used to combine 'datafull' with 'infant mortality'
# data because of the predictive values it contains (1799-2099)
datafull_1 <- full_join(datafull,infant_mort)
# Gives a snapshot of variables
# datafull_1

# Left_join() used to combine 'datafull_1' with 'total fertility'
# data to match previous data set with respective values
dataleft <- left_join(datafull_1,total_fertility)
# Gives a snapshot of variables
#glimpse(dataleft)

# Left_join() used to combine 'dataleft' with 'total population' 
# data to match previous data set with the respective values
dataleft_1 <- left_join(dataleft,pop_total)
# Gives a snapshot of variables
# glimpse(dataleft_1)
# Used to determine structure of variables in df after joining
# str(dataleft_1)

# Left_join function used to combine 'dataleft_1' with 'GDP'
# data to match previous data set with respective values
dataleft_2 <- left_join(dataleft_1,gdp_adj)
# Used to determine structure of variables in df after joining
# str(dataleft_2)


# Reorganize data columns according to 'dslabs' package

dataleft_2 <- dataleft_2 %>% 
  relocate(infant_mortality, .before = life_expectancy) %>% 
  relocate(continent, .after = `GDP`) %>% 
  relocate(region, .after = continent)
# Used to determine structure of variables in df after reorganizing

# str(dataleft_2)

```

```{r computed-gdppc}
#| results: hide

# Used to examine unique values in 'Population' variable
# unique(dataleft_2$Population)


# Used to identify class of 'Population' variable
# class(dataleft_2$Population)

# Extracts characters from 'Population' variable and
# separates them into this environment
pop.char <-  
  str_extract(dataleft_2$Population,"[aA-zZ]+")


# Extracts digits from 'Population' variable
# digits from population variable separated into this environment
pop.num <- 
  str_extract(dataleft_2$Population,"[[:digit:]]+\\.*[[:digit:]]*")


# Combines population digits and character data frames into one environment
pop_values <- data.frame(pop.num, pop.char)


# Converts 'pop.num' (digit column) from character into numeric values
pop_values$pop.num <- as.numeric(pop_values$pop.num)
# Used to determine structure of variables
# str(pop_values)


######### Repeat steps for GDP  #########

# Examines unique values for 'GDP'
# unique(dataleft_2$GDP)

# Identifies class of 'GDP' variable
# class(dataleft_2$GDP)

# Extracts character values from 'GDP' variable
# and separates them into this environment
gdp.char <- 
  str_extract(dataleft_2$GDP,"[aA-zZ]+")
# str(gdp.char)

# Extracts digits from 'GDP' variable and 
# separates them into this environment
gdp.num <- 
  str_extract(dataleft_2$GDP,"[[:digit:]]+\\.*[[:digit:]]*")
# str(gdp.num)

# Combine 'GDP' character & numeric data into one environment
gdp_values <- data.frame(gdp.num, gdp.char)
# str(gdp_values)

# Converts gdp.num to numeric class
gdp_values$gdp.num <- as.numeric(gdp_values$gdp.num)
# str(gdp_values)

# Checks for unique characters
# unique(pop.char)

# Converts character values to numeric for population data
pop_category <- mutate(pop_values, category = case_when(
  pop.char == "M" ~ 1e6,
  pop.char == "B" ~ 1e9,
  pop.char == 'k' ~ 1e3
))
# glimpse(pop_category)

# Creates new column of the product of 'gdp_final'
pop_data <- pop_category %>% 
mutate(pop_final = pop.num*category) %>%  select(pop_final)

# Gives snapshort of pop_data to check variables
# glimpse(pop_data)


######### repeat steps for 'GDP' #########

# Reports unique values
# unique(gdp.char)

# Converts 'GDP' character values to numeric based on exponentials
gdp_category <- mutate(gdp_values, category_gdp = case_when(
  gdp.char == "M" ~ 1e6,
  gdp.char == "B" ~ 1e9,
  gdp.char == 'TR' ~ 1e12
))
gdp_data <- gdp_category %>% 
# creates new column with the product of two numeric variables
  mutate(gdp_final = gdp.num*category_gdp)

gdp_final <- gdp_data %>% 
  select(gdp_final)

# str(gdp_final)
```

```{r joining-2}
#| results: hide

# Joins country region table and population numeric table
gdp_per_cap <- data.frame(dataleft_2$country, dataleft_2$year, 
                           pop_data$pop_final, gdp_data$gdp_final)
# str(gdp_per_cap)

# Computed 'GDP per capita' variable
gdp_per_capita <- gdp_per_cap %>% 
  mutate(gdppc = gdp_data.gdp_final/ pop_data.pop_final)

gdp_per_capita %>%  
  select(dataleft_2.country, dataleft_2.year,gdppc)


# str(gdp_per_capita)

```

```{r joining-final}
#| results: hide

# glimpse(dataleft_2)

# Left join() used to match computed GDP per capita to original 'dataleft_2' 
# table computed 
data_table_full <- left_join(dataleft_2, gdp_per_capita, 
                            by = c("country" = "dataleft_2.country", 
                                   "year" = "dataleft_2.year"))


# str(data_table_full)

# Removes unnecessary computed variables
data_table_full2 <- data_table_full[,-c(6,7)] %>% 
  # Set number of decimal places
  mutate_if(is.numeric, round, digits = 2) %>% 
  relocate(continent, .after = `gdppc`) %>% 
  relocate(region, .after = continent) 

# Renames columns based on gapminder data
colnames(data_table_full2)[8] = "gdpPerCap"
colnames(data_table_full2)[7] = "gdp"
colnames(data_table_full2)[6] = "population"


glimpse(data_table_full2)
```

```{r dslabs}
#| results: hide

# Visualization of 'dslabs' package for Gapminder data
# glimpse(gapminder)


```

```{r write-csv}

# Contains newly computed dataset based on path 
write.csv(data_table_full2, "clean_data/dataset_full.csv")


```

```{r tibble}
#| results: hide


full_table_df <-                   
  as_tibble(data_table_full2)
# Prints full tibble
# print(full_table_df)

```




## Sanity Checks Summary




When compared to the 'dslabs' gapminder package the data is quite different. To begin, the computed data table has 61,322 rows while dslabs only reported 10,545. The computed data table reports from 1799 up to 2099 (which also indicates predictive data), but dslabs only contains data up to 2012. The populations are also listed in numeric forms in dslabs where as they were listed as character (and then reassigned as numeric) in project 2.




## Bubble Chart for 2011




```{r fig-bubble-chart}
#| fig-cap: 'Life Expectancy trends by GDP per capita in 2011.'
#| warning: false


bubble_chart_2011 <-
  ggplot(data = filter(full_table_df, year == 2011)
  ) +
  aes(
    x =gdpPerCap,
    y = life_expectancy,
    size = population,
    color = continent
  ) +
  labs(
    x = "GDP per capita",
    y = "Life expectancy",
    title = "Direct positive relationship between Life Expectancy & Income",
    subtitle = "among countries in 2011",
    caption = "Source: Computed Gapminder Dataset"
  ) +
  # Logged scale to 10 to improve appearance
  # Include the code for numbers
  # Limits of x axis obtained from summary() function
  scale_x_log10(limits = c(110, 262069)) +
  scale_y_log10(
    # Limits of y axis obtained from summary() function
    limits = c(45, 85)
  ) +
  geom_point(alpha = 0.5)

bubble_chart_2011

```


@fig-bubble-chart depicts the relationship between `GDP per capita` and `Life expectancy` among five continents in 2011. GDP per capita (x axis, log10 units) was plotted against life expectancy (y axis). The bubble size depicts respective country populations. The figure displays a positive relationship between `GDP per capita` and `Life Expectancy`. This supports **Hypothesis 1** which states that as `GDP per capita` increases, `Life Expectancy` also increases. This trend can be confirmed for most continents including Europe, Asia, Americas and Oceania. However, Africa appears to be an exception to the trend. As GDP per capita increases on the continent, life expectancy fluctuates between 50 to 80 years with very few exceptions. Many african countries around \$10000 in `GDP per capita` still have a life expectancy below 70 years. However, some exceptions can be seen where African countries have moved up in life expectancy up to 75 years. Countries in the Americas or Europe at the similar GDP leves, appear to have a life expectancy above 70 years. Asian countries appear to have a wider range of life expectancy with some below 65 and others higher than 75 years.

The World Bank defines `GDP per capita` as the sum of gross value added by resident producers in a country's entire economy divided by the mid-year population [@WorldBank2022]. `Life Expectancy` at birth indicates the general mortality level of a population according to the World Health Organization [@WHO2021]. Both these variables being discussed in this paper are indirect indicators of a country's quality of life in terms of income/wealth and physical & mental health. @fig-bubble-chart highlighted the positive relationship that exists between `GDP per capita` and `Life Expectancy` in accordance with Hypothesis 1. Though this was the case for many countries in Europe, Asia, Oceania and the Americas, many African countries did not follow this positive trend. In fact, though `GDP per capita` increased in some of the African countries, the `Life Expectancy` remained fairly at the same level. In an article written by Daniel Borjas for the 'Borgen project', the author highlights many health issues that have plagued the continent over the 21st century. One of the most devastating health crisis to the continent is the HIV/AIDS epidemic. This epidemic has ravaged many lives due to lack of resources; both educational and health related [@Borjas2017] . Although access to antiretroviral therapy (ART) has been made readily available worldwide, there are several barriers to access that exist in alot of these African countries. Other vector-borne infectious diseases such as malaria are also widely rampant in many African countries. The global malaria burden is disproportionately carried by African countries up to 95-96% [@WorldHealthOrganization2022]. Given these statistics, many of these countries had difficulties improving their life expectancy. Despite increased `GDP per capita`, poor governance and human resource issues are said to account for the hindered integration of health services to these nations [@Oleribe2019].




## Bubble Chart over Time




```{r fig-bubble-chart-time}
#| fig-cap: "Life expectancy and GDP per capita have a positive relationship"
#| fig-subcap: "Years: 1980-2010"
#| warning: false


bubble_chart_years <-
  # Plotted the full_table_df data for 4 separate years
  ggplot(data = filter(full_table_df, year %in% c(1980, 1990, 2000, 2010))) +
  aes(
    x = gdpPerCap,
    y = life_expectancy,
    size = population,
    color = continent
  ) +
  labs(
    x = "GDP Per Capita",
    y = "Life Expectancy",
    title = "Life Expectancy by GDP per capita",
    subtitle = "among countries in 1980,1990,2000 and 2010",
    caption = "Source: Computed Gapminder Dataset",
  ) +
  # The range of GDP per capita was between 50 and 62000 
  # using summary() function 
  scale_x_log10(limits = c(500, 262069)) +
  # The range of life expectancy was between 10 and 84 using 
  # summary() function
  ylim(40, 85) +
  # Apply facets to create a separate plot for each year
  facet_wrap(~year) +
  geom_point(alpha = 0.5)
  

bubble_chart_years

```


@fig-bubble-chart-time depicts the relationship between `Life Expectancy` and `GDP per capita` between the 1980 to 2010 time period. As shown in the previous figure, `GDP per capita` was plotted on the x axis (log10 units) and `Life Expectancy` on the y axis. Country population determines bubble size, and color by continent. The figure above shows a positive relationship between `GDP per capita` and `Life Expectancy` over the time period 1980 to 2010 at 10 year increments. This observation is congruent with **Hypothesis 2**. As the 'x' values increase, 'y' increases accordingly. This is seen specifically in Asia, Europe, Oceania, and the Americas. However, there are countries that do not support this claim. As seen in previous analysis of Figure 1, many African countries with higher GDP per capita statistics over time, still maintain a low level of life expectancy. In 1980, `Life expectancy` for all countries was below 80 years. However by 2010, many countries were either closely approaching, reached or even surpassed the 80 year life expectancy. Additionally, the overall gradient of the curve decreases over time, transitioning from a steeper slope in 1980 to a flattening one by 2010.

@fig-bubble-chart-time further highlights the trends established in the previous figure. However, one can infer that as the years go by, more countries are transitioning to higher `Life Expectancy` levels, causing the gradient of the curve to be reduced as it flattens. One observation made is that some Asian countries had high `GDP per capita` values in 1980 compared to 2010. This can be accounted for by the increasing population that developed over time causing the `GDP per capita` value to decrease as its denominator expanded. The transition in many African countries to higher `GDP per capita` levels between 1980 to 2010 without much increase in `Life Expectancy` can also be seen and accounted for with explanations given above. Many Asian, European and some Western countries appear to dominate `GDP per capita` and `Life Expectancy`. This pattern can be attributed to their well -developed economic systems, and highly trained health care personnel. Resources such as food and proper housing are also more readily available and the overall quality of life is assumed to be greater.




## The Growth Charts




### GDP per capita



```{r fig-gdppc}
#| fig.cap: 'Trends in GDP per capita for five countries.'
#| warning: false

my_countries <- 
  c("China", "Russia", "United States", "India", "Norway")

full_table_df$year <- as.numeric(full_table_df$year)

gdp_growth_chart <-
  # Plotted full_table_df data for 4 separate countries
  ggplot(data = filter(full_table_df, country %in% my_countries)
  ) +
  aes(
    x = year,
    y = gdpPerCap,
    color = country
  ) +
  labs(
    x = "Year",
    y = "GDP Per Capita",
    title = "Trends in GDP Per Capita",
    subtitle = "for China, Russia, US, India & Norway",
    caption = "Source: Gapminder Computed Table"
  ) +
  scale_y_log10() +
  scale_x_log10(
    # Limits set based on computed dataset min & max values 
    limits = c(1799, 2022)
  ) +
  geom_line(size = 1)
  

gdp_growth_chart
```


@fig-gdppc illustrates the relationship between `GDP per capita` and time in years for five countries, namely China, India, Norway, Russia and the United States (US). This figure is a visual representation of changes and trends in `GDP per capita` (y axis) by `Year` (x axis) from 1799 to 2022. A general positive relationship can be seen between `GDP per capita` and `Year`. This positive association appears to be greater in the US and Norway (more stable economies) compared to China, India and Russia (less stable economies during this time period). This supports the claim in **Hypothesis 3** that GDP per capita increases over time in more economically stable countries. The flat lines start off much higher in the US, Russia and Norway (more stable economies at the time) and then gradually increase over time. Similarly, India and China curves also start off flat and show a sharp increase towards the start of the 21 cenury. Despite these overall trends, many dips in the curves occur throughout this time period (explained below). A significant decrease in `GDP per capita` is seen between 1910-1925, and 1990-2000 for Russia. China also experienced decreases in `GDP per capita` between 1930 to 1970.

@fig-gdppc visualizes trends in `GDP per capita` from 1799 to 2022 for the US, Norway, Russia, India and China. The US started off with the highest `GDP per capita` among the five countries in 1799. A major drop in GDP is seen between 1925-1950, which is accounted for by World War II and the Great Depression which ended in 1945. During this time period many other countries like China, Russia and Norway were involved in the war (whether directly or indirectly) resulting in GDP per capita decreases; as shown in their trendlines. The US was eventually surpassed by Norway around 1981.

Norway's economic system became transformed in the late 1960s when oil was discovered. Petroleum production eventually began in 1971 with major discoveries following [@MinistryofPetroleumandEnergy2020]. Over the next decade the country transitioned from a fishing economy to a billion dollar oil-based economy creating many jobs and driving the country's GDP per capita upward [@Haroon2022]. Between 2007-2009, both the US and Norway display a downward trend in their `GDP per capita`. This seemingly small change on this graph can be explained by the "Great Recession" which had a tremendous economic, social and mental effect at the population and individual level in the US. The Federal Reserve described the economic decline as 'modest' at first but eventually sharply decreasing in 2008 where GDP fell by 4.3%. Historians marked this as the worst recession since World War II [@FederalReserveHistory2013]. In Norway, this decline (knowingly caused by the Great Recession) was attributed to the "International Financial Crisis" which affected most countries in Europe. Both the economies, however, were able to pick up within 1-2 years after.

In the early 1800s, Russia ranked higher than Norway in `GDP per capita` until about 1860. Russia as we know it today has had a long history of war. From 1799 to 1880, Russia experiences a general increase in GDP. However, in 1881 the leader Czar Alexander II is also assassinated and the turmoil is depicted by this drop in GDP around that time. From 1914, a decline is also seen as a result of World War I where Russia went to war with Austria-Hungary for Serbia. In 1917 an even greater decline is seen as a result of the Civil War in Russia which also led to the formation of the Soviet Union. Throughout 1925 to 1950, the country experienced major turmoil with other historic eents like the Great Purge (1934) and World War II (ending in 1945). Despite steady increases in GDP after this period, Russia faces another great decline with the dissolution of the Soviet Union in 1991. The country was also affected a few years earlier by the Chernobyl disaster of 1986.

Fragile State Indexes for India, China and Russia are much higher (60-80) compared to US (46.6) and Norway(15.6). India, China and Russia all had extremely low `GDP per capita` levels compared to the US and Norway. Despite it's poorly designed tax system, low `GDP per capita` in India has been mostly related to its extremely large population. The `GDP per capita` in China before 1980 was below \$200, hence its seeming emergence in 1985 and beyond. For many years China did not engage in the foreign market and remained isolated from the global economy. They have also been familiar with wars like the Chinese Revolution of 1911, World War II and the Civil War in 1947-1949. During these years the country divided into the PRC and ROC, where the ROC fled to Taiwan leading to the formation of the Republic of China as a country (Taiwan). The string of war and tension lead to much economic instability. Coupled with it's large population this worked in their disfavor. The increasing trend however can be linked to innovation, trade liberalization and economic reform in China [@CongressionalResearchService2019].



### Infant Mortality



```{r fig-inf-mort}
#| fig.cap: 'Infant mortality trends for five countries.'
#| warning: false


infant_growth_chart <-
  # Plotted gapminder_df data for 4 separate countries
  ggplot(data = filter(full_table_df, country %in% my_countries)
  ) +
  aes(
    x = year,
    y = infant_mortality,
    color = country
  ) +
  labs(
    x = "Year",
    y = "Infant Mortality",
    title = "Infant Mortality Rates",
    subtitle = "for China, Russia, US, India & Norway",
    caption = "Source: Gapminder Computed Data",
  ) +
  scale_x_log10(
    limits = c(1840, 2022)
  ) +
  geom_line(size = 1)


infant_growth_chart
```


@fig-inf-mort illustrates the relationship between `Infant Mortality` and time in `Years` between 1799 to 2022. The `Year` was plotted on the x axis against `Infant Mortality` on the y axis, with the each country's line distinguished by color. A negative relationship was established between `Infant Mortality` and time in `Years`. This observation is consistent with **Hypothesis 4** and therefore supports the claim that `Infant Mortality` decreases over time as the country develops. @fig-inf-mort shows that `Infant Mortality` dropped significantly in Russia India and China. On the other hand, countries like the US and Norway with already lower infant mortality rates continued to decline steadily over 21st century to minimal values. Norway maintained the lowest `Infant Mortality` rate and China the highest among all five countries. This figure also highlights a break in data for China between 1962 and 1969, as well as India (approx 1947-1952) and Russia (approx 1910-1970). Overall Norway has maintained the lowest infant mortality from 19799 to 2022 followed closely by the US. These trends will be explained below.

@fig-inf-mort depicted the negative relationship that exists between `Infant Mortality` rates and time. The Center for Disease Control defines `Infant Mortality` as the death of infants before they turn one year old [@CenterforDiseaseControlandPrevention2022]. India has been known to have high rates of infant mortality due to premature births, pneumonia and diarrheal diseases. The country became independent from Britain in 1947, the same year that the Indo-Pakistani War started. During this time there was high communal violence and death rates skyrocketed. This accounts for the increased infant mortality rates when reporting resumed in 1951. High infant mortality rates were also seen in the early 1900s as a result of the Spanish Flu pandemic from 1910.

Infant mortality rates in China started being reported in early 1950s and took a sharp increase during that time. This can be accounted for by the 'Great Leap Forward' initiative in China that failed, causing widespread starvation (The Great Chinese Famine), and death of approximately 20 million people. Data was found missing on China's infant mortality figures between 1962 to 1969. This can be related to the loss of the ROC's seat in the UN as Taiwan in 1971. For Russia, data is missing for the early 20th century (1910) to the early 1950s, due their revolutionary periods and the Second World War which caused mass instability in their country.




## Rank Chart




### Population Rank by GDP/Capita



```{r fig-pop-rank-gdp}
#| fig.cap: 'Population rank chart of GDP per capita levels in 2011.'
#| results: hide
#| warning: false

gdp_rank_chart <-
  full_table_df %>%
  filter(year == 2011) %>%
  # Arranges country population in descending order 
  arrange(desc(population)) %>%
  # Keep only the data columns of interest
  select(country, population, gdpPerCap) %>%
  # Includes the top 10 rows of output 
  head(n = 10)

##### Part 2 #####
ggplot(data = gdp_rank_chart
) +
  aes(
    x = population,
    # Reorders country on y axis based on population
    y = reorder(country, population),
    fill = gdpPerCap
  ) +
  labs(
    x = "Population",
    y = "Country",
    title = "Population Rank Charts",
    subtitle = "for GDP per capita in 2011",
    caption = "Source: Gapminder computed dataset",
  ) +
  scale_fill_continuous(name = "GDP per capita") +
  # Color palette for bar plot
  scale_fill_viridis(option = "D") +
  geom_bar(stat = "identity") 
  
gdp_rank_chart

```

@fig-pop-rank-gdp displays the relationship between `Population` and `GDP per capita` for 10 countries in 2011. These countries are listed in descending order of population. `GDP per capita` levels are represented by the respective fill colors with yellow being the highest and purple the lowest. `Population` is plotted on the x axis and `Country` on the y axis. This rank chart depicts China as having the highest population followed closely by India, while Japan has the 10th largest in rank. Inversely, among the top 10 countries, the US appears to have the highest `GDP per capita` followed by the Japan. Other countries like India, Indonesia, Pakistan, Nigeria and Bangladesh and China also have high populations but are on the lower end of GDP. These observations don't provide enough evidence to support the claim in **Hypothesis 5** stating that countries with higher populations have lower `GDP per capita`. This is because high population countries like the US and Japan have been able to maintain relatively high `GDP per capita`.

In @fig-pop-rank-gdp, the bar plot shows the relationship between `Population` and `GDP per capita`. The US maintained a high `GDP per capita` with an equally high population. However, compared to India and China, the US population is significantly smaller, although in the top 10 population countries. Russia also appears to have a relatively medium range `GDP Per capita` when compared than other countries in this set, followed by Brazil.



### Infant Mortality by Population



```{r fig-inf-rank}
#| fig.cap: 'Rank chart of the top 15 countries by infant mortality in 2011.'
#| results: hide
#| warning: false


inf_mort_rank_chart <-
  full_table_df %>%
  filter(year == 2011) %>%
  # Arranges output in order of descending infant mortality
  arrange(desc(infant_mortality)) %>%
  # Keep only the data columns of interest
  select(country, population, infant_mortality) %>%
  # Selects the top 15 rows of output
  head(n = 15)

##### Part 2 #####
ggplot(data = inf_mort_rank_chart
  ) +
  aes(
    x = infant_mortality,
    # Reorders country on y axis based on infant mortality
    y = reorder(country, infant_mortality),
    fill = population
  ) +
  labs(
    x = "Infant Mortality",
    y = "Country",
    title = "Infant Mortality Ranking",
    subtitle = "by population in 2011",
    caption = "Source: Gapminder computed data",
  ) +
  scale_fill_continuous(name = "Population") +
  scale_fill_viridis(option = "D") +
  geom_bar(stat = "identity")

inf_mort_rank_chart
```


@fig-inf-rank outlines `Infant Mortality` in descending order for the top 15 countries in 2011. The fill color gradient represents the population size for that year, with yellow indicating the highest population and purple the lowest. `Infant Mortality` is displayed on the x axis and `Country` on the y axis. The top three countries showing the highest infant mortality rates are Angola, Sierra Leone and Central African Republic. The lowest infant mortality levels in this rank are Benin, Mauritania and Pakistan. The highest population is Pakistan followed closely by Nigeria and then Democratic Republic of Congo. The lower end populations in this top 15 rank are listed as Sierra Leone, Central African Rebuplic, Equitorial Guinea, Losotho, Guinea-Bissau and Mauritania; all African countries. From these observations there is not sufficient evidence to support the claim made in **Hypothesis 6**; countries that are less densely populated, have higher infant mortality rates. For 2011, many of the countries with very high infant mortality rates were on the lowest end of the population scale like Angola, Sierra Leone and Central African Republic, but still in the top 15.Exceptions can be seen to this hypothesis. For example, Nigeria with an infant mortality rate over 75 is on the high end of the population spectrum. Pakistan is also in the 15 countries for `Infant mortality` with the highest population counts. The statistics for Nigeria and Pakistan are alarming, and should spark public health intervention efforts. Although this contradicts **Hypothesis 6**, one must also take into account that the rank includes the top 15 countries with the highest infant mortality rates in 2011. Hence, these countries towards the end of the @fig-inf-rank are still considered 'high' with respect to the other 185 countries.

@fig-inf-rank depicts successfully depicts the relationship between `Infant Mortality` and `Population`. . The figure highlights key exceptions to the hypothesis for Democratic Republic of Congo, Nigeria, and Pakistan (all in the top 15) which have significantly higher populations. This is very concerning as it infers that a significant proportion of infant lives are being lost in those countries.




## Bubble Chart - Larger Time Frame




```{r fig-bubble-chart-years-time}
#| fig-cap: "Life expectancy and GDP per capita have a positive relationship"
#| fig-subcap: "Years: 1860-2010"
#| warning: false


bubble_chart_years2 <-
  # Plotted the full_table_df data for 4 separate years
  ggplot(data = filter(full_table_df, year %in% c(1860, 1910, 1960, 2010))) +
  aes(
    x = gdpPerCap,
    y = life_expectancy,
    size = population,
    color = continent
  ) +
  labs(
    x = "GDP Per Capita",
    y = "Life Expectancy",
    title = "Positive relationship between life Expectancy & GDP per capita",
    subtitle = "among countries in 1860,1910,1960 and 2010",
    caption = "Source: Computed Gapminder Dataset",
  ) +
  # The range of GDP per capita was between 500 and 262069 
  # using summary() function 
  scale_x_log10(limits = c(500, 262069)) +
  # The range of life expectancy was between 40 and 85 using 
  # summary() function
  ylim(40, 85) +
  # Apply facets to create a separate plot for each year
  facet_wrap(~year) +
  geom_point(alpha = 0.5)
  

bubble_chart_years2

```


@fig-bubble-chart-years-time displays the change in `Life expectancy` (y axis) with `GDP per capita`(x axis) over a larger time frame than Figure 2. The graph supports the claim made in **Hypothesis 6** which suggests greater changes being displayed over time between the two variables. In 1860, there were little to no records of `Life expectancy` and `GDP per capita`, only in a few European countries. By 1910, more European countries reported these variables with slight increases being observed. By this year, one African country is also observed. 50 years later, the plot fills up with multiple countries being displayed, and an increasing positive relationship between the two variables, similar to the trend in Figure 2. In 2010, a general upward shift can be seen in the curve with life expectancy increasing significantly above 70 years for many countries. A left-ward shift has also taken place in 2010 with increases in `GDP per capita`.




## Infant Mortality & GDP per Capita




```{r fig-infmort-gdp}
#| fig-cap: "Infant mortality and GDP per capita "
#| fig-subcap: "Year: 2011"
#| warning: false


infmort_gdp <-
  # Plotted the full_table_df data for 4 separate years
  ggplot(data = filter(full_table_df, year == 2011)
  ) +
  aes(
    x = gdpPerCap,
    y = infant_mortality,
    size = population,
    color = continent
  ) +
  labs(
    x = "GDP Per Capita",
    y = "Infant mortality",
    title = "Negative relationship between infant mortality and GDP per capita",
    subtitle = "among countries in 2011",
    caption = "Source: Computed Gapminder Dataset",
  ) +
  # The range of GDP per capita was between 50 and 62000 
  # using summary() function 
  scale_x_log10(limits = c(500, 150000)) +
  # The range of infant mortality was between 0 and 200 using 
  # summary() function
  ylim(0, 150) +
  geom_point(alpha = 0.5)
  

infmort_gdp
```


@fig-infmort-gdp depicts the relationship between `Infant mortality` (y axis) and `GDP per capita` (x axis) for 2011. From the graph, one can infer that countries with higher GDP per capita values have slightly lower infant mortality rates, compared to low GDP per capita values with slightly high infant mortality rates. This observation supports the claim in **Hypothesis 8**. Countries with higher GDP per capita values are more likely to have better access to healthcare. This will lead to the positive outcome of lower infant mortality rates. @fig-infmort-gdp also highlights the fact that many countries in Africa on the lower `GDP per capita` end of the scale, had the highest infant mortality rates for 2011.





# Conclusion





In conclusion, a global positive trend can be seen in the relationship between `GDP per capita` and `Life Expectancy` for most countries and continents. However, exceptions do exist among many African countries. This can be attributed to political instability, war, poor health resources and economic hardship. Post colonial effects also play a major part in these statistics, with many of these countries grappling to find identity and stability midst a fast changing world. These countries are proven to have the lowest `GDP per capita` and `Life Expectancy`as well as some of the highest `Infant Mortality` rates. On the other hand, some Asian countries like Japan and China have been able to increase their `GDP per capita` and `Life Expectancy` while decreasing their `Infant Mortality` rates respectively. Countries ravaged by war and political instability such as Russia, China and India have struggled in the past to maintain low `Infant mortality` rates, but many have been able to reduce this outcome in the 21st century. Public health interventions should be geared toward reducing these health disparities in disadvantaged countries (for example on the African continent) in order to improve the population's quality of life. Based on final assessments in Hypothesis 8, reduced infant mortality can also be an indicator and predictor of increased `GDP per capita`.





# References












