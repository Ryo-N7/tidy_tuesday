library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
#library(xlsx)
library(forcats)

#df <- read.xlsx(file = "../tidy_tuesday/april_16_week_3/global_mortality.xlsx", 
#                sheetName = "share-of-deaths-by-cause-2016 (")

#save(df, file = "data.Rda")
#write.csv(df, file = "../tidy_tuesday/april_16_week_3/df.csv", row.names = FALSE)

df <- read.csv(file = "../tidy_tuesday/april_16_week_3/df.csv")

glimpse(df)

df <- df %>% janitor::clean_names() 

glimpse(df)

# data is in --PERCENTAGES-- for each year.

df %>% 
  group_by(country) %>% 
  arrange(desc(cancers))

df %>% 
  filter(year == 2015, country == "Kenya") %>% 
  glimpse()


df_gather <- df %>% 
  gather(key = "cause_of_death", value = "percentages", -country, -country_code, -year) 

df_gather %>% pull(country_code) %>% unique()
# 194 countries and NA and OWID_WRL (World)

df_gather %>% glimpse()

df_gather %>% 
  mutate(cause_of_death = fct_reorder(cause_of_death, percentages, max)) %>% 
  ggplot(aes(x = cause_of_death, y = percentages, fill = cause_of_death)) +
  geom_col(na.rm = TRUE, position = "dodge") +
  ylim(c(0, 100)) + coord_flip()



# year: 1990-2016
df_gather %>% pull(year) %>% unique()


## gganimate
library(gganimate)

anim <- df_gather %>% 
  filter(country == "Kenya") %>% 
  ggplot(aes(x = cause_of_death, y = percentages, fill = cause_of_death, frame = year)) +
  geom_point(na.rm = TRUE) +
  ylim(c(0, 80)) + 
  coord_flip() +
  theme(legend.position = "none")

gganimate(anim)

## bars

df_gather %>% 
  filter(country == "Kenya") %>% 
  group_by(year) %>% 
  arrange(desc(year), desc(percentages))

anim <- df_gather %>% 
  filter(country == "Kenya") %>% 
  ggplot(aes(x = reorder(cause_of_death, percentages), y = percentages, fill = cause_of_death, frame = year)) +
  geom_bar(stat = "identity") +
  ylim(c(0, 80)) + 
  coord_flip() +
  theme(legend.position = "none")

gganimate(anim)




# Separate diseases/disorders ---------------------------------------------

diseases_disorders <- c("tuberculosis", "respiratory_diseases", "meningitis",
                        "malaria", "lower_respiratory_infections", "liver_disease", "hiv_aids",
                        "intestinal_infectious_diseases", "kidney_disease", "hepatitis",
                        "digestive_diseases", "diarrheal_diseases", "diabetes", "cancers",
                        "cardiovascular_diseases")



# combine with UN Regional Names ------------------------------------------
library(wpp2015)
data("UNlocations")


## inner_join on country name >>> divide group by region of Africa/Asia/Euro
# region == color 
# median/mean for continent
# size of lolipop point by population rank?
#  boxplots instead?
# by continental region?

UNlocations <- UNlocations %>% select(ends_with("name"))

df2 <- df_gather %>% 
  inner_join(UNlocations, by = c("country" = "name"))

df2 %>% glimpse()

df_afr <- df2 %>% 
  filter(area_name == "Africa") %>% 
  filter(percentages != is.na(percentages)) %>% 
  mutate(avg = mean(percentages)) %>%  
  mutate(over_under = if_else(percentages - avg > 0, TRUE, FALSE))

df_afr %>% glimpse()
  
df_afr_anim <- df_afr %>% 
  filter(reg_name == "Western Africa", cause_of_death %in% diseases_disorders) %>% 
  ggplot(aes(x = percentages, y = cause_of_death, fill = country, frame = year)) +
  #geom_segment(aes(x = 0, xend = percentages, 
  #                 y = cause_of_death, yend = cause_of_death)) +
  geom_point(size = 3, shape = 21) +
  xlim(c(0, 70)) +
  scale_fill_brewer(palette = "Set3")

gganimate(df_afr_anim)

# facet_wrap regions per continent?!??!




df_jp_anim <- df2 %>% 
  filter(country == "Japan",
         cause_of_death %in% diseases_disorders) %>% 
  ggplot(aes(percentages, cause_of_death, fill = cause_of_death, frame = year)) +
  geom_point(size = 3, shape = 21) +
  xlim(c(0, 100)) +
  scale_fill_brewer(palette = "Set3") +
  theme_bw() +
  theme(legend.position = "none")
  
gganimate(df_jp_anim)


# geom_dumbbell TEST ------------------------------------------------------

library(ggalt)

df2 %>% 
  filter(country == "Japan", year %in% c(1990, 2016), cause_of_death %in% diseases_disorders) %>% 
  select(year, cause_of_death, percentages) %>% 
  spread(key = "year", value = "percentages") %>% 
  ggplot(aes(x = `1990`, xend = `2016`, y = cause_of_death, group = cause_of_death)) +
    geom_dumbbell(aes(x = `1990`, xend = `2016`, y = cause_of_death, group = cause_of_death),
      colour_x = "green", colour_xend = "black", size = 1.5, dot_guide_colour = TRUE)

health <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/health.csv")

glimpse(health)

df2 %>% 
  filter(reg_name == "Western Europe", year %in% c(1990, 2016), cause_of_death %in% diseases_disorders) %>% 
  select(country, year, cause_of_death, percentages) %>% pull(country) %>% unique()
  spread(key = "year", value = "percentages") %>% 
  ggplot(aes(y = cause_of_death, group = cause_of_death)) +
  geom_segment(aes(x = `1990`, xend = `2016`, yend = cause_of_death)) +
  geom_point(aes(x = `1990`), color = "green") +
  geom_point(aes(x = `2016`), color = "black") +
  facet_grid(~country)


df2 %>% 
  group_by(reg_name) %>% 
  summarize(n = n_distinct(country)) %>% 
  arrange(desc(n)) %>% print(n = 25)

# western europe, east asia as `country` vars.........


# boxplots gganimate TEST -------------------------------------------------



df_afr_anim <- df2 %>% 
  filter(cause_of_death %in% diseases_disorders,
         reg_name == "Western Africa") %>% 
  ggplot(aes(cause_of_death, percentages, frame = year)) +
  geom_boxplot()

library(gganimate)

gganimate(df_afr_anim)


df2 %>% 
  filter(cause_of_death %in% diseases_disorders,
         reg_name == "Western Africa",
         year == 1990) %>% 
  ggplot(aes(cause_of_death, percentages)) +
  geom_boxplot()

west_afr <- df2 %>% 
  filter(reg_name == "Western Africa")

glimpse(west_afr)

# 15 WestAfr countries
west_afr %>% pull(country) %>% unique() # n_distinct()


nort_am <- df2 %>% 
  filter(reg_name == "Northern America")

glimpse(nort_am)

# 16 diseases/disorders  non-mental >>> delete parkinsons

# no accumulation for geom_boxplot....
# do it the old fashioned way with for_loop :O
library(jpeg)


years <- c(1990:1993)

for (i in years) {
  
  #west_afr <- west_afr %>% 
  #  filter(year <= y)
  
  boxplots <- west_afr %>% 
    filter(year == i) %>% 
    ggplot(aes(cause_of_death, percentages)) +
    geom_boxplot()
  
  ggsave(file = paste0("boxplots/", i, ".jpg"), plot = boxplots, width = 6, height = 4, units = "in", dpi = 300)
  print(paste0("processings: ", i))
  
}



# nested west africa TEST -------------------------------------------------

west_afr %>% 
  group_by(year) %>% 
  nest() %>% 
  head()

library(purrr)

nested_west_afr <- west_afr %>% 
  group_by(year) %>% 
  nest() %>% 
  mutate(plot = map2(data, year, 
                     ~ggplot(data = .x, 
                             aes(reorder(cause_of_death, percentages), percentages)) + 
                       geom_boxplot() +
                       coord_flip()
                     ))

glimpse(nested_west_afr)

nested_west_afr$plot[20]

map2(paste0("april_16_week_3/boxplots/", nested_west_afr$year, ".jpg"), nested_west_afr$plot, ggsave)


# divide by regions -------------------------------------------------------

## divide by cause == smaller groups??
df2 %>% pull(cause_of_death) %>% unique()

df2 %>% 
  filter(year == 1999,
         reg_name == "Eastern Africa") %>% 
  mutate(percentages = percentages/100) %>%
  ggplot(aes(reorder(cause_of_death, percentages), percentages)) +
  geom_boxplot(fill = "white", color = "darkred", outlier.color = "black") +
  scale_y_continuous(breaks = scales::pretty_breaks(10), 
                     limits = c(0, 1),
                     labels = scales::percent,
                     expand = c(0.02, 0)) +
  coord_flip() +
  labs(title = "Median proportion of 'Cause of Death' in the Western Africa region",
       y = "Proportion of All Deaths",
       x = "Cause of death") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank())
  

# reorder by highest avg for the region?
# outlier for CONFLICT in 1997, Southern Europe == Albania... from ~0% to 10% of all deaths due to conflict
# outlier for CONFLICT in 1994, Eastern Africa == Rwanda... NA in 1993 to 82.3% of all deaths due to conflict
# outlier for CONFLICT in 1999, Eastern Africa == Eritrea  to 37% of all deaths due to conflict
glimpse(df2)


df2 %>% 
  filter(reg_name == "Southern Europe",
         cause_of_death %in% c("terrorism", "conflict"))

df2 %>% 
  filter(reg_name == "Eastern Africa",
         cause_of_death %in% c("terrorism", "conflict"),
         country == "Rwanda")



# western africa ACTUAL ---------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(xlsx)

## Load file
df <- read.csv(file = "../tidy_tuesday/april_16_week_3/df.csv")

## Easily clean names
df <- df %>% janitor::clean_names() 

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

## Grab the "region name" from UNlocations dataset for easy subsetting! Ex. Eastern Europe, Western Africa, etc.
library(wpp2015)

data("UNlocations")

UNlocations <- UNlocations %>% select(ends_with("name"))

df2 <- df_gather %>% 
  inner_join(UNlocations, by = c("country" = "name"))

## Only look at countries in the "Western Africa" region
west_afr <- df2 %>% 
  filter(reg_name == "Western Africa")

## Unfortunately geom_boxplot() does not support "frame" for gganimate...
## Therefore, create plot for each year with purrr then use ImageMagick or other to compile into gif/video!

library(purrr)

nested_west_afr <- west_afr %>% 
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
                       labs(title = "Median Proportion of 'Cause of Death' in Countries of the Western Africa Region",
                            subtitle = "Years: 1990 - 2016, (1 second = 1 year)",
                            y = "Proportion of All Deaths", x = "",
                            caption = "By: Ryo Nakagawara (@R_by_Ryo) \n Source: ourworldindata.org\n#TidyTuesday") +
                       theme_bw() +
                       theme(panel.grid.major.y = element_blank(),
                             text = element_text(family = "Arial Narrow"),
                             plot.title = element_text(size = 14, hjust = 0.5),
                             plot.subtitle = element_text(size = 10, hjust = 0.5)
                             )
  ))

glimpse(nested_west_afr)

nested_west_afr$plot[1]

## Save each plot (per year) with ggsave!
map2(paste0("april_16_week_3/boxplots/", nested_west_afr$year, ".jpg"), nested_west_afr$plot, ggsave)

# Saving 9.49 x 7.98 in image

## Futile attempts at using ImageMagick >>> gave up and just used GifMaker.me   :(

years <- c(1990:2016)

# C:/Users/Ryo Nakagawara/Documents/R_materials/tidy_tuesday/april_16_week_3/boxplots/

for (i in years) {
  
  system(paste0('magick.exe convert C:/Users/"Ryo Nakagawara"/Documents/R_materials/tidy_tuesday/april_16_week_3/boxplots/', i, '.jpg C:/Users/"Ryo Nakagawara"/Documents/R_materials/tidy_tuesday/april_16_week_3/boxplots/', i, '.jpg -geometry +305+72 -composite -pointsize 100 -font Arial -annotate +2000+1120 ', i, 'C:/Users/"Ryo Nakagawara"/Documents/R_materials/tidy_tuesday/april_16_week_3/boxplots_combined/img', i, '.jpg'))
  print(paste0("processing: ", i))
  
}

## ....
library(magick)

image_convert()




# Eastern Africa ----------------------------------------------------------

## Only look at countries in the "Eastern Africa" region
east_afr <- df2 %>% 
  filter(reg_name == "Eastern Africa")

## Unfortunately geom_boxplot() does not support "frame" for gganimate...
## Therefore, create plot for each year with purrr then use ImageMagick or other to compile into gif/video!

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

nested_east_afr$plot[5]
nested_east_afr$plot[20]
nested_east_afr$plot[13]
nested_east_afr$plot[1]

## Save each plot (per year) with ggsave!
map2(paste0("april_16_week_3/boxplots/", nested_east_afr$year, ".jpg"), nested_east_afr$plot, ggsave)

