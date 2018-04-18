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
  ylim(c(0, 100)) + 
  coord_flip() +
  theme(legend.position = "none")

gganimate(anim)



library(ggmap)
library(wpp2015)
data("UNlocations")


## inner_join on country name >>> divide group by region of Africa/Asia/Euro
# region == color 
# median/mean for continent
# size of lolipop point by population rank?

UNlocations <- UNlocations %>% select(ends_with("name"))

df2 <- df_gather %>% 
  inner_join(UNlocations, by = c("country" = "name"))

df2 %>% glimpse()

df2 <- df2 %>% 
  group_by(area_name) %>% 
  filter(area_name == "Africa", year == 2010) %>% 
  filter(percentages != is.na(percentages)) %>% 
  mutate(avg = mean(percentages)) %>%  
  mutate(over_under = if_else(percentages - avg > 0, TRUE, FALSE)) %>% 
  ungroup()
  
df2 %>% 
  group_by(reg_name) %>% 
  ggplot(aes(x = percentages, y = cause_of_death, color = over_under)) +
  geom_segment(aes(x = avg, xend = percentages, 
                   y = cause_of_death, yend = cause_of_death)) +
  geom_point(size = 3, shape = 5, aes(fill = df2$reg_name)) +
  geom_vline(xintercept = df2 %>% pull(avg) %>% unique()) +
  xlim(c(-10, 70)) 




