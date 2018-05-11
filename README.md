| Table of Contents    | (\#TidyTuesday)                            |
|----------------------|--------------------------------------------|
| [Week 1](#april-3):  | College tuition data                       |
| [Week 2](#april-10): | NFL salaries by position                   |
| [Week 3](#april-16): | Causes of death across the world           |
| [Week 4](#april-23): | Salary differences by gender in Australia  |
| [Week 5](#april-30): | American Community Survey                  |
| [Week 6](#may-7):    | Coffee chains across the USA and the World |

------------------------------------------------------------------------

### April 3

### College tuition data

``` r
library(xlsx)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)
```

#### load data, check data

``` r
usa_avg_tuition <- read.xlsx(file = "april_3_week_1/us_avg_tuition.xlsx", sheetName = "Table 5")

glimpse(usa_avg_tuition)
```

    ## Observations: 50
    ## Variables: 13
    ## $ State       <fct> Alabama, Alaska, Arizona, Arkansas, California, Co...
    ## $ X2004.05    <dbl> 5682.838, 4328.281, 5138.495, 5772.302, 5285.921, ...
    ## $ X2005.06    <dbl> 5840.550, 4632.623, 5415.516, 6082.379, 5527.881, ...
    ## $ X2006.07    <dbl> 5753.496, 4918.501, 5481.419, 6231.977, 5334.826, ...
    ## $ X..2007.08. <dbl> 6008.169, 5069.822, 5681.638, 6414.900, 5672.472, ...
    ## $ X2008.09    <dbl> 6475.092, 5075.482, 6058.464, 6416.503, 5897.888, ...
    ## $ X2009.10    <dbl> 7188.954, 5454.607, 7263.204, 6627.092, 7258.771, ...
    ## $ X2010.11    <dbl> 8071.134, 5759.153, 8839.605, 6900.912, 8193.739, ...
    ## $ X2011.12    <dbl> 8451.902, 5762.421, 9966.716, 7028.991, 9436.426, ...
    ## $ X2012.13    <dbl> 9098.069, 6026.143, 10133.503, 7286.580, 9360.574,...
    ## $ X2013.14    <dbl> 9358.929, 6012.445, 10296.200, 7408.495, 9274.193,...
    ## $ X2014.15    <dbl> 9496.084, 6148.808, 10413.844, 7606.410, 9186.824,...
    ## $ X2015.16    <dbl> 9751.101, 6571.340, 10646.278, 7867.297, 9269.844,...

``` r
colnames(usa_avg_tuition)
```

    ##  [1] "State"       "X2004.05"    "X2005.06"    "X2006.07"    "X..2007.08."
    ##  [6] "X2008.09"    "X2009.10"    "X2010.11"    "X2011.12"    "X2012.13"   
    ## [11] "X2013.14"    "X2014.15"    "X2015.16"

#### deal with annoying column names ==&gt; regex time!

``` r
colnames(usa_avg_tuition) <- colnames(usa_avg_tuition) %>% 
  str_replace_all("X", "") %>% 
  str_replace("\\.", "-20")

# manually change 2007-2008 ...

usa_avg_tuition <- usa_avg_tuition %>% rename(`2007-2008` = `-20.2007.08.`)
```

#### gather to correct format + create "rank" column

``` r
usa_avg_tuition <- usa_avg_tuition %>% gather(key = "year", value = "tuition", -State)

usa_avg_tuition <- usa_avg_tuition %>% 
  arrange(year, desc(tuition)) %>% 
  group_by(year) %>% 
  mutate(rank = dense_rank(desc(tuition)))


usa_avg_tuition <- usa_avg_tuition %>% 
  mutate(state = as.character(State))
```

#### `pull()` out the top 10 states for start year and end year ==&gt; assign color to them.

``` r
top_states <- usa_avg_tuition %>% 
  filter(year %in% c("2004-2005", "2015-2016") & rank %in% c(1:10)) %>% 
  pull(state) %>% 
  unique()

top_states
```

    ##  [1] "Vermont"        "Pennsylvania"   "Ohio"           "New Hampshire" 
    ##  [5] "New Jersey"     "Massachusetts"  "Maryland"       "Delaware"      
    ##  [9] "South Carolina" "Illinois"       "Michigan"       "Virginia"

``` r
usa_avg_tuition <- usa_avg_tuition %>% 
  mutate(top_tuition = state %in% top_states,
         tuition = round(tuition, digits = 2))

colors = c(
  Vermont = "#EE2C2C",          # red
  Pennsylvania = "lightgreen",        # dark blue
  Ohio = "#00441b",          # green
  `New Hampshire` = "#4a1486",        # purple
  `New Jersey` = "#636363",        # dark grey
  Massachusetts = "#fd8d3c",    # orange
  Maryland = "#000000",          # black
  Delaware = "blue",
  `South Carolina` = "brown",
  Illinois = "pink",
  Michigan = "yellow",
  Virginia = "violet"
)
```

#### bump chart theme

``` r
library(extrafont)

theme_tuition <-  
  theme(text = element_text(family = "Arial Narrow", color = "#444444", face = "bold"),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(angle = 0, vjust = 0.5, margin = margin(r = 15)),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 35, vjust = 1.3, hjust = 1.1,
                                   margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 8)),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        legend.position = "none")
```

#### the final bump chart

``` r
usa_avg_tuition %>% 
  filter(top_tuition == TRUE) %>% 
  ggplot(aes(year, rank, group = state)) +
  geom_line(aes(color = state), size = 1.6, alpha = 0.75) +
  geom_point(aes(color = state), size = 3, alpha = 1) +
  scale_y_reverse(breaks = 1:10) +         # show only top 10!
  geom_text(data = usa_avg_tuition %>% filter(year == "2004-2005"),
            aes(label = state, x = -0.3), 
            fontface = "bold", color = "black", size = 3.5) +
  geom_label(data = usa_avg_tuition %>% filter(year == "2004-2005"),
            aes(label = paste("$", tuition)), nudge_x = -1.3, nudge_y = -0.4, 
            fontface = "bold", color = "black", size = 2.5) +
  geom_text(data = usa_avg_tuition %>% filter(year == "2015-2016"),
            aes(label = state, x = 13.25), 
            fontface = "bold", color = "black", size = 3.5) +
  geom_label(data = usa_avg_tuition %>% filter(year == "2015-2016"),
             aes(label = paste("$", tuition)), nudge_x = 1.3, nudge_y = -0.4, 
             fontface = "bold", color = "black", size = 2.5) +
  coord_cartesian(ylim = c(1, 10.3), xlim = c(-0.9, 14)) +
  theme_tuition +
  scale_color_manual(values = colors) +
  labs(x = "Year", 
       y = "Rank\n&\nTuition",
       title = "Average College Tuition Rankings in the United States",
       subtitle = "By State, 2004-2005 to 2015-2016",
       caption = "By: Ryo Nakagawara (@R_by_Ryo) \n Source: https://onlinembapage.com/average-tuition-and-educational-attainment-in-the-united-states/\n#TidyTuesday")
```

<img src="readme_tidytuesday_files/figure-markdown_github/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

``` r
# ALL states
library(viridis)

usa_avg_tuition %>% 
  ggplot(aes(year, rank, group = state)) +
  geom_line(aes(color = state), size = 1.6, alpha = 0.75) +
  geom_point(aes(color = state), size = 3, alpha = 1) +
  #scale_color_viridis(option = "A") +
  scale_y_reverse(breaks = 1:50) +         # show only top 10!
  geom_text(data = usa_avg_tuition %>% filter(year == "2004-2005"),
            aes(label = state, x = -0.3), 
            fontface = "bold", color = "black", size = 2.7) +
  geom_label(data = usa_avg_tuition %>% filter(year == "2004-2005"),
            aes(label = paste("$", tuition)), nudge_x = -1.3, nudge_y = -0.4, 
            fontface = "bold", color = "black", size = 1.5) +
  geom_text(data = usa_avg_tuition %>% filter(year == "2015-2016"),
            aes(label = state, x = 13.25), 
            fontface = "bold", color = "black", size = 2.7) +
  geom_label(data = usa_avg_tuition %>% filter(year == "2015-2016"),
             aes(label = paste("$", tuition)), nudge_x = 1.3, nudge_y = -0.4, 
             fontface = "bold", color = "black", size = 1.5) +
  coord_cartesian(ylim = c(1, 51), xlim = c(-0.9, 14)) +
  theme_tuition +
  labs(x = "Year", 
       y = "Rank\n&\nTuition",
       title = "Average College Tuition Rankings in the United States",
       subtitle = "By State, 2004-2005 to 2015-2016",
       caption = "By: Ryo Nakagawara (@R_by_Ryo) \n Source: https://onlinembapage.com/average-tuition-and-educational-attainment-in-the-united-states/\n#TidyTuesday") +
  theme(axis.text.y = element_text(size = 6.5))
```

<img src="readme_tidytuesday_files/figure-markdown_github/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

### April 10

### NFL salaries by position

(did not participate this week)

### April 16

### Causes of death across the world

### load data

``` r
library(dplyr)
library(tidyr)
library(ggplot2)
library(xlsx)

## Load file
df <- read.csv(file = "../tidy_tuesday/april_16_week_3/df.csv")

## Easily clean names
df <- df %>% janitor::clean_names() 
```

``` r
## Gather cause of deaths under single column 
df_gather <- df %>% 
  gather(key = "cause_of_death", value = "percentages", -country, -country_code, -year) 

## Use stringr functions to make cleaned names work on plot, leave HIV/AIDS for now...
library(stringr)

df_gather <- df_gather %>% 
  mutate(cause_of_death = stringr::str_replace_all(cause_of_death, "_", " "),
         cause_of_death = stringr::str_to_title(cause_of_death),
         cause_of_death = case_when(
           cause_of_death == "Hiv Aids" ~ "HIV/AIDS",
           TRUE ~ cause_of_death
         )) 
```

### Grab the "region name" from UNlocations dataset for easy subsetting! Ex. Eastern Europe, Western Africa, etc.

``` r
library(wpp2015)

data("UNlocations")

UNlocations <- UNlocations %>% select(ends_with("name"))

df2 <- df_gather %>% 
  inner_join(UNlocations, by = c("country" = "name"))

## Only look at countries in the "Eastern Africa" region
east_afr <- df2 %>% 
  filter(reg_name == "Eastern Africa")
```

Unfortunately geom\_boxplot() does not support "frame" for gganimate...
-----------------------------------------------------------------------

Therefore, create plot for each year with purrr then use ImageMagick or other to compile into gif/video!
--------------------------------------------------------------------------------------------------------

``` r
library(purrr)

nested_east_afr <- east_afr %>% 
  mutate(percentages = percentages/100) %>%
  group_by(year) %>% 
  nest() %>% 
  mutate(plot = map2(data, year, 
                     ~ggplot(data = .x, aes(reorder(cause_of_death, percentages), percentages)) +
                       geom_boxplot(fill = "white", color = "darkred", outlier.color = "black") +
                       scale_y_continuous(breaks = scales::pretty_breaks(10), 
                                          limits = c(0, 1),
                                          labels = scales::percent,
                                          expand = c(0.02, 0)) +
                       coord_flip() +
                       labs(title = "Median Proportion of 'Cause of Death' in Countries of the Eastern Africa Region",
                            subtitle = paste0("Year: ", .y), # .y == YEAR!
                            y = "Proportion of All Deaths", x = "",
                            caption = "By: Ryo Nakagawara (@R_by_Ryo) \n Source: ourworldindata.org\n#TidyTuesday") +
                       theme_bw() +
                       theme(panel.grid.major.y = element_blank(),
                             text = element_text(family = "Arial Narrow"),
                             plot.title = element_text(size = 14, hjust = 0.5),
                             plot.subtitle = element_text(size = 12, hjust = 0.5)
                       )
  ))

glimpse(nested_east_afr)

#nested_east_afr$plot[5]
#nested_east_afr$plot[20]
#nested_east_afr$plot[13]
#nested_east_afr$plot[1]
```

#### Save each plot (per year) with ggsave!

``` r
map2(paste0("april_16_week_3/boxplots/", nested_east_afr$year, ".jpg"), nested_east_afr$plot, ggsave)
```

#### Use ImageMagick through Command Console:

![]()

#### Final GIF:

![]()

### April 23

### Salary differences by gender in Australia

### April 30

### American Community Survey

### May 7

### Coffee chains across the USA and the World

``` r
library(dplyr)
library(readxl)
library(sf)

# read-in data
starbucks_raw <- read_excel("../tidy_tuesday/may_7_week_6/week6_coffee_chains.xlsx", sheet = 1)

# clean, select cols, filter USA, summarize
starbucks_usa <- starbucks_raw %>% 
  janitor::clean_names() %>% 
  select(brand, city, state_province, country, longitude, latitude) %>% 
  filter(country == "US") %>% 
  group_by(state_province) %>% 
  summarize(count = n()) %>% 
  ungroup()

# grab geometries of USA from tigris pkg, turn into sf
states_sf <- tigris::states(cb = TRUE) %>% 
  st_as_sf() %>% 
  select(STUSPS, NAME, geometry) %>% 
  filter(!STUSPS %in% c("VI", "MP", "GU", "PR", "AS")) # filter out territories and Puerto Rico
```

    ## 
      |                                                                       
      |                                                                 |   0%
      |                                                                       
      |=                                                                |   1%
      |                                                                       
      |=                                                                |   2%
      |                                                                       
      |==                                                               |   3%
      |                                                                       
      |===                                                              |   4%
      |                                                                       
      |===                                                              |   5%
      |                                                                       
      |====                                                             |   6%
      |                                                                       
      |====                                                             |   7%
      |                                                                       
      |=====                                                            |   7%
      |                                                                       
      |=====                                                            |   8%
      |                                                                       
      |======                                                           |   9%
      |                                                                       
      |======                                                           |  10%
      |                                                                       
      |=======                                                          |  10%
      |                                                                       
      |=======                                                          |  11%
      |                                                                       
      |========                                                         |  12%
      |                                                                       
      |========                                                         |  13%
      |                                                                       
      |=========                                                        |  13%
      |                                                                       
      |=========                                                        |  14%
      |                                                                       
      |=========                                                        |  15%
      |                                                                       
      |==========                                                       |  15%
      |                                                                       
      |==========                                                       |  16%
      |                                                                       
      |===========                                                      |  16%
      |                                                                       
      |===========                                                      |  17%
      |                                                                       
      |============                                                     |  18%
      |                                                                       
      |============                                                     |  19%
      |                                                                       
      |=============                                                    |  19%
      |                                                                       
      |=============                                                    |  20%
      |                                                                       
      |=============                                                    |  21%
      |                                                                       
      |==============                                                   |  21%
      |                                                                       
      |==============                                                   |  22%
      |                                                                       
      |===============                                                  |  22%
      |                                                                       
      |===============                                                  |  23%
      |                                                                       
      |===============                                                  |  24%
      |                                                                       
      |================                                                 |  24%
      |                                                                       
      |================                                                 |  25%
      |                                                                       
      |=================                                                |  25%
      |                                                                       
      |=================                                                |  26%
      |                                                                       
      |=================                                                |  27%
      |                                                                       
      |==================                                               |  27%
      |                                                                       
      |==================                                               |  28%
      |                                                                       
      |===================                                              |  29%
      |                                                                       
      |===================                                              |  30%
      |                                                                       
      |====================                                             |  30%
      |                                                                       
      |====================                                             |  31%
      |                                                                       
      |====================                                             |  32%
      |                                                                       
      |=====================                                            |  32%
      |                                                                       
      |=====================                                            |  33%
      |                                                                       
      |======================                                           |  33%
      |                                                                       
      |======================                                           |  34%
      |                                                                       
      |=======================                                          |  35%
      |                                                                       
      |=======================                                          |  36%
      |                                                                       
      |========================                                         |  36%
      |                                                                       
      |========================                                         |  37%
      |                                                                       
      |========================                                         |  38%
      |                                                                       
      |=========================                                        |  38%
      |                                                                       
      |=========================                                        |  39%
      |                                                                       
      |==========================                                       |  39%
      |                                                                       
      |==========================                                       |  40%
      |                                                                       
      |==========================                                       |  41%
      |                                                                       
      |===========================                                      |  41%
      |                                                                       
      |===========================                                      |  42%
      |                                                                       
      |============================                                     |  43%
      |                                                                       
      |============================                                     |  44%
      |                                                                       
      |=============================                                    |  44%
      |                                                                       
      |=============================                                    |  45%
      |                                                                       
      |==============================                                   |  46%
      |                                                                       
      |==============================                                   |  47%
      |                                                                       
      |===============================                                  |  47%
      |                                                                       
      |===============================                                  |  48%
      |                                                                       
      |================================                                 |  49%
      |                                                                       
      |================================                                 |  50%
      |                                                                       
      |=================================                                |  50%
      |                                                                       
      |=================================                                |  51%
      |                                                                       
      |==================================                               |  52%
      |                                                                       
      |==================================                               |  53%
      |                                                                       
      |===================================                              |  53%
      |                                                                       
      |===================================                              |  54%
      |                                                                       
      |====================================                             |  55%
      |                                                                       
      |====================================                             |  56%
      |                                                                       
      |=====================================                            |  57%
      |                                                                       
      |======================================                           |  58%
      |                                                                       
      |======================================                           |  59%
      |                                                                       
      |=======================================                          |  59%
      |                                                                       
      |=======================================                          |  60%
      |                                                                       
      |========================================                         |  61%
      |                                                                       
      |========================================                         |  62%
      |                                                                       
      |=========================================                        |  63%
      |                                                                       
      |=========================================                        |  64%
      |                                                                       
      |==========================================                       |  64%
      |                                                                       
      |==========================================                       |  65%
      |                                                                       
      |===========================================                      |  66%
      |                                                                       
      |===========================================                      |  67%
      |                                                                       
      |============================================                     |  67%
      |                                                                       
      |============================================                     |  68%
      |                                                                       
      |=============================================                    |  69%
      |                                                                       
      |=============================================                    |  70%
      |                                                                       
      |==============================================                   |  70%
      |                                                                       
      |==============================================                   |  71%
      |                                                                       
      |===============================================                  |  72%
      |                                                                       
      |===============================================                  |  73%
      |                                                                       
      |================================================                 |  73%
      |                                                                       
      |================================================                 |  74%
      |                                                                       
      |=================================================                |  75%
      |                                                                       
      |=================================================                |  76%
      |                                                                       
      |==================================================               |  76%
      |                                                                       
      |==================================================               |  77%
      |                                                                       
      |==================================================               |  78%
      |                                                                       
      |===================================================              |  78%
      |                                                                       
      |===================================================              |  79%
      |                                                                       
      |====================================================             |  79%
      |                                                                       
      |====================================================             |  80%
      |                                                                       
      |====================================================             |  81%
      |                                                                       
      |=====================================================            |  81%
      |                                                                       
      |=====================================================            |  82%
      |                                                                       
      |======================================================           |  82%
      |                                                                       
      |======================================================           |  83%
      |                                                                       
      |======================================================           |  84%
      |                                                                       
      |=======================================================          |  84%
      |                                                                       
      |=======================================================          |  85%
      |                                                                       
      |========================================================         |  85%
      |                                                                       
      |========================================================         |  86%
      |                                                                       
      |========================================================         |  87%
      |                                                                       
      |=========================================================        |  87%
      |                                                                       
      |=========================================================        |  88%
      |                                                                       
      |==========================================================       |  88%
      |                                                                       
      |==========================================================       |  89%
      |                                                                       
      |==========================================================       |  90%
      |                                                                       
      |===========================================================      |  90%
      |                                                                       
      |===========================================================      |  91%
      |                                                                       
      |============================================================     |  92%
      |                                                                       
      |============================================================     |  93%
      |                                                                       
      |=============================================================    |  93%
      |                                                                       
      |=============================================================    |  94%
      |                                                                       
      |=============================================================    |  95%
      |                                                                       
      |==============================================================   |  95%
      |                                                                       
      |==============================================================   |  96%
      |                                                                       
      |===============================================================  |  96%
      |                                                                       
      |===============================================================  |  97%
      |                                                                       
      |================================================================ |  98%
      |                                                                       
      |================================================================ |  99%
      |                                                                       
      |=================================================================| 100%

``` r
# join with starbucks data
starbucks_sf <- states_sf %>% left_join(starbucks_usa, by = c("STUSPS" = "state_province"))

# remove Alaska and Hawaii... try to rescale and put them back in another time
starbucks_sf2 <- starbucks_sf %>% filter(!NAME %in% c("Alaska", "Hawaii"))

# change crs to one nc_cartogram() expects (sf compatibility for cartogram pkg coming soon i think...)
starb_cartogram <- st_transform(starbucks_sf2, crs = 2163)

# change back into sp object for nc_cartogram()
starb_sp <- as(starb_cartogram, "Spatial")

# construct non-contiguous area cartogram, area weighed by "count" var 
library(cartogram)
starb_cartogram <- nc_cartogram(starb_sp, weight = "count") # k = 1 is default

# Sequential single hue color palette :: http://colorbrewer2.org/#type=sequential&scheme=Greens&n=5
greenpal <- c('#edf8e9','#bae4b3','#74c476','#31a354','#006d2c')


# add "kmeans" process for better class intervals for the colors
# (California is still in their own category but best I can do with such big outliers)
# Texas has x2 the amount of Starbucks as NY, WA, and FL but size relative only to CA?
# can also use hclust, kmeans, quantile, jenks to varying success
# legend.reverse for HIGH values on TOP, slight sepia to offset white glare?
# fiddle with inner.margins to fit legend and small title
library(tmap)

starbucks_cartogram <- tm_shape(starb_cartogram) + 
  tm_borders("grey10") +
  tm_fill(title = "", "count", 
          palette = greenpal, 
          style = "kmeans",
          legend.reverse = TRUE) +
  tm_layout(inner.margins = c(.04,.02, .08, .02),
            main.title = "Number of Starbucks across the United States",
            title = "(Source: https://www.kaggle.com/starbucks/store-locations)\n(@R_by_Ryo, #TidyTuesday)",
            title.position = c("center", "top"), title.size = 0.9,
            fontfamily = "Garamond", fontface = "bold",
            legend.text.size = 0.85, 
            sepia.intensity = 0.2)

starbucks_cartogram
```

![](readme_tidytuesday_files/figure-markdown_github/unnamed-chunk-14-1.png)

``` r
save_tmap(starbucks_cartogram, "starbucks_cartogram.png")
```
