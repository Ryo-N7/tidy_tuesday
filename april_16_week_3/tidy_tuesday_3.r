library(dplyr)
library(tidyr)
library(ggplot2)
library(ggmap)
library(xlsx)

#df <- read.xlsx(file = "../tidy_tuesday/april_16_week_3/global_mortality.xlsx", 
#                sheetName = "share-of-deaths-by-cause-2016 (")

#save(df, file = "data.Rda")
#write.csv(df, file = "../tidy_tuesday/april_16_week_3/df.csv", row.names = FALSE)

df <- read.csv(file = "../tidy_tuesday/april_16_week_3/df.csv")

glimpse(df)

df <- df %>% janitor::clean_names() 

glimpse(df)

df %>% 
  group_by(country) %>% 
  add_count() %>% 
  arrange(desc(n))

