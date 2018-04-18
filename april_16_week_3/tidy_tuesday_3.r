library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(xlsx)
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
  gather(key = "cause_of_death", value = "percentages", -country, -country_code, -year) %>% 
  glimpse()

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



diseases_disorders <- c("tuberculosis", "respiratory_diseases", "parkinson_disease", "meningitis",
                        "malaria", "lower_respiratory_infections", "liver_disease", "hiv_aids",
                        "intestinal_infectious_diseases", "kidney_disease", "hepatitis",
                        "digestive_diseases", "diarrheal_diseases", "diabetes", "cancers",
                        "cardiovascular_diseases")




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


### boxplots

# no accumulation for geom_boxplot....

df_afr_anim <- df2 %>% 
  filter(cause_of_death %in% diseases_disorders,
         reg_name == "Western Africa") %>% 
  ggplot(aes(cause_of_death, percentages, frame = year)) +
  geom_boxplot()

library(gganimate)
gganimate_save(df_afr_anim)

