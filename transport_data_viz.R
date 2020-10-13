# Create time series plots for NYU capstone data

# Load data

# load libraries

library(tidyverse)
library(sf)
library(lubridate)

# load graphic styles

plotTheme <- theme(
  plot.title =element_text(size=12),
  plot.subtitle = element_text(size=8),
  plot.caption = element_text(size = 6),
  axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
  axis.text.y = element_text(size = 10),
  axis.title.y = element_text(size = 10),
  # Set the entire chart region to blank
  panel.background=element_blank(),
  plot.background=element_blank(),
  #panel.border=element_rect(colour="#F0F0F0"),
  # Format the grid
  panel.grid.major=element_line(colour="#D0D0D0",size=.75),
  axis.ticks=element_blank())

plotTheme12ptY <- theme(
  plot.title =element_text(size=16),
  plot.subtitle = element_text(size=8),
  plot.caption = element_text(size = 6),
  axis.text.x = element_text(size = 14, angle = 45, hjust = 1),
  axis.text.y = element_text(size = 14),
  axis.title.y = element_text(size = 14),
  axis.title.x = element_text(size = 14),
  # Set the entire chart region to blank
  panel.background=element_blank(),
  plot.background=element_blank(),
  #panel.border=element_rect(colour="#F0F0F0"),
  # Format the grid
  panel.grid.major=element_line(colour="#D0D0D0",size=.75),
  axis.ticks=element_blank())

mapTheme <- theme(plot.title =element_text(size=12),
                  plot.subtitle = element_text(size=8),
                  plot.caption = element_text(size = 6),
                  axis.line=element_blank(),
                  axis.text.x=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),
                  panel.background=element_blank(),
                  panel.border=element_blank(),
                  panel.grid.major=element_line(colour = 'transparent'),
                  panel.grid.minor=element_blank(),
                  legend.direction = "vertical", 
                  legend.position = "right",
                  plot.margin = margin(1, 1, 1, 1, 'cm'),
                  legend.key.height = unit(1, "cm"), legend.key.width = unit(0.2, "cm"))

palette <- c("#10142A", "#47E9B9", "#F55D60", "#71EA48", "#C148EA", "EAC148" )

# load hexbin data

hexbin2019_2020 <- read.csv("data/2020_hexbin.csv") %>%
  select(night., month, day, year, month.day,
         mta_inflow_counts, citi_inflow_counts) %>%
  rbind(., read.csv("data/2019.csv") %>%
          select(night., month, day, year, month.day,
                 mta_inflow_counts, citi_inflow_counts))%>%
  rename(night = "night.") %>%
  unite(date, c("month", "day", "year"), sep = "-", remove = FALSE) %>%
  mutate(night = ifelse(night == 0, "DAY", "NIGHT"),
         date = mdy(date),
         week = week(date)) %>%
  group_by(week, night, year) %>%
  summarize(total_mta_inflow = sum(mta_inflow_counts),
            total_citibike = sum(citi_inflow_counts))# %>%
  #mutate(year = year(date))

# Median Daily, Nightly ridership for Jan 2020

median_Ridership <- hexbin2019_2020 %>%
  filter(week < 5) %>%
  group_by(night) %>%
  summarize(median_MTA = median(total_mta_inflow),
            median_citibike = median(total_citibike))


hexbin2019_2020 %>%
  ungroup() %>%
  mutate(year = as.factor(year)) %>%
  filter(night == "NIGHT",
         week != 18) %>%
  mutate(asPctJan2020MTA = (100*(total_mta_inflow / median_Ridership$median_MTA[2])-100),
         asPctJan2020Citi = (100*(total_citibike / median_Ridership$median_citibike[2])-100)) %>%
  filter(year == "2020") %>%
  select(week, asPctJan2020MTA, asPctJan2020Citi) %>%
  gather(-week, key = "mode", value = "value") %>%
  mutate(Mode = ifelse(mode == "asPctJan2020MTA", "Subway", "Bike Share"))%>%
ggplot()+
  geom_line(aes(x = week, y = value, color = Mode), size = 1, alpha = 0.8)+
  scale_color_manual(values = palette[4:5])+
  ylab("Percentage Increase/Decrease")+
  xlab("Week")+
  geom_vline(xintercept = 12, linetype = "dotted", alpha = 0.8)+
  geom_hline(yintercept = 0)+
  geom_text(aes(x = 13, y = 40,label = "Stay-At-Home Order\nMarch, 20th"), 
            alpha = 0.5, size = 3)+
  labs(
    title = "2020 NYC Nighttime Transit Use Vs Baseline",
    subtitle = "Weekly usership as a percentage of Jan, 2020 median usership",
    caption = "Data: NYDOT, Citibike, NYU CUSP"
  )+
  plotTheme


