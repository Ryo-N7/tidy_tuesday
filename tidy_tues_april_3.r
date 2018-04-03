library(xlsx)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)


usa_avg_tuition <- read.xlsx(file = "us_avg_tuition.xlsx", sheetName = "Table 5")

glimpse(usa_avg_tuition)

colnames(usa_avg_tuition)


# regex ugh

colnames(usa_avg_tuition) <- colnames(usa_avg_tuition) %>% 
  str_replace_all("X", "") %>% 
  str_replace("\\.", "-20")

# manually change 2007-2008 ...

usa_avg_tuition <- usa_avg_tuition %>% rename(`2007-2008` = `-20.2007.08.`)

# spread/gather to correct format

usa_avg_tuition <- usa_avg_tuition %>% gather(key = "year", value = "tuition", -State)

usa_avg_tuition <- usa_avg_tuition %>% 
  arrange(year, desc(tuition)) %>% 
  group_by(year) %>% 
  mutate(rank = dense_rank(desc(tuition)))


usa_avg_tuition <- usa_avg_tuition %>% 
  mutate(state = as.character(State))

#usa_avg_tuition %>% 
#  group_by(year) %>% 
#  mutate(top_10 = if_else(State %in% rank[1:10], T, F))

top_states <- usa_avg_tuition %>% 
  filter(year %in% c("2004-2005", "2015-2016") & rank %in% c(1:10)) %>% 
  pull(state) %>% 
  unique()

top_states

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



# bump chart theme
library(extrafont)
theme_tuition <-  
  theme(text = element_text(family = "Arial Narrow", color = "#444444", face = "bold"),
        plot.title = element_text(size = 24, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.title.y = element_text(angle = 0, vjust = 0.5, margin = margin(r = 15)),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 35,
                                   margin = margin(t = 15, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20)),
        legend.title = element_blank(),
        legend.position = "none")

# bump chart

usa_avg_tuition %>% 
  filter(top_tuition == TRUE) %>% 
  ggplot(aes(year, rank, group = state)) +
  geom_line(aes(color = state), size = 1.6, alpha = 0.75) +
  geom_point(aes(color = state), size = 3, alpha = 1) +
  scale_y_reverse(breaks = 1:10) +         # show only top 10!
  geom_text(data = usa_avg_tuition %>% filter(year == "2004-2005"),
            aes(label = state, x = 0.5), nudge_x = -0.5, 
            fontface = "bold", color = "black", size = 4) +
  geom_label(data = usa_avg_tuition %>% filter(year == "2004-2005"),
            aes(label = paste("$", tuition)), nudge_x = -0.7, nudge_y = -0.4, 
            fontface = "bold", color = "black", size = 2.5) +
  geom_text(data = usa_avg_tuition %>% filter(year == "2015-2016"),
            aes(label = state, x = 13.5), nudge_x = 0.25, 
            fontface = "bold", color = "black", size = 4) +
  geom_label(data = usa_avg_tuition %>% filter(year == "2015-2016"),
             aes(label = paste("$", tuition)), nudge_x = 0.9, nudge_y = -0.4, 
             fontface = "bold", color = "black", size = 2.5) +
  coord_cartesian(ylim = c(1, 10.3), xlim = c(-0.5, 14)) +
  theme_tuition +
  scale_color_manual(values = colors) +
  labs(x = "Year", 
       y = "Rank\n&\nTuition ($)",
       title = "College Tuition Rankings in the United States",
       subtitle = "2004-2005 to 2015-2016")



