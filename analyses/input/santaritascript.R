# <============================================================================>
# Arizona precipitation and soil moisture visualizations
# Santa Rita Experimental Range NEON
# Temporal Ecology Lab
# Author: Justin Ngo
# <============================================================================>

library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(ggthemes)

# combining all the separate csv files into one big csv that can be selected and filtered
srer <- list.files(path="santarita_PRIPRE_30min_compiled", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

# adding year and DOY columns
datetime <- srer$endDateTime
srer <- srer %>%
  select(startDateTime,endDateTime,priPrecipBulk)%>%
  mutate(year = as.numeric(format(datetime, format ="%Y"))) %>%
  mutate(doy = yday(endDateTime))

# summarising every 30 minutes into a single doy value for broader picture of annual precipitation
# keeping srer as a separate dataframe so that if we wanted to look at intra-annual precipitation we still have that option
sreredit <- srer %>%
  type.convert(as.is=TRUE) %>%
  group_by(doy,year) %>%
  summarise(daily_precip = sum(priPrecipBulk))

# filtering by year
srer2018 <- sreredit %>%
  filter(year == "2018")
srer2019 <- sreredit %>%
  filter(year == "2019")
srer2020 <- sreredit %>%
  filter(year == "2020")
srer2021 <- sreredit %>%
  filter(year == "2021")
srer2022 <- sreredit %>%
  filter(year == "2022")

# visualizing annual trends
srer2018_plot <- srer2018 %>%
  ggplot(aes(x = doy,y = daily_precip)) + 
  geom_line(linewidth = 0.5,colour = "#366EC4") +
  geom_point(size = 2, colour = "#366EC4") +
  labs(x="Day of Year (2018)",
       y="Summed daily precipitation (mm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
srer2018_plot

srer2019_plot <- srer2019 %>%
  ggplot(aes(x = doy,y = daily_precip)) + 
  geom_line(linewidth = 0.5,colour = "#366EC4") +
  geom_point(size = 2, colour = "#366EC4") +
  labs(x="Day of Year (2019)",
       y="Summed daily precipitation (mm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
srer2019_plot

srer2020_plot <- srer2020 %>%
  ggplot(aes(x = doy,y = daily_precip)) + 
  geom_line(linewidth = 0.5,colour = "#366EC4") +
  geom_point(size = 2, colour = "#366EC4") +
  labs(x="Day of Year (2020)",
       y="Summed daily precipitation (mm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
srer2020_plot

srer2021_plot <- srer2021 %>%
  ggplot(aes(x = doy,y = daily_precip)) + 
  geom_line(linewidth = 0.5,colour = "#366EC4") +
  geom_point(size = 2, colour = "#366EC4") +
  labs(x="Day of Year (2021)",
       y="Summed daily precipitation (mm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
srer2021_plot

srer2022_plot <- srer2022 %>%
  ggplot(aes(x = doy,y = daily_precip)) + 
  geom_line(linewidth = 0.5,colour = "#366EC4") +
  geom_point(size = 2, colour = "#366EC4") + #when black/white, shapes will show through
  labs(x="Day of Year (2022)",
       y="Summed daily precipitation (mm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
srer2022_plot

# It appears that the time where rainfall is highest is somewhere between doy 175 to 300, so maybe we can focus on those using the 30min precipitation data
srerfocus2018 <- srer %>%
  filter(year == "2018") %>%
  # mutate(time = format(endDateTime,"%D:%H:%M")) %>%
  filter(between(doy,175,300))
srerfocus2019 <- srer %>%
  filter(year == "2019") %>%
  # mutate(time = format(endDateTime,"%D:%H:%M")) %>%
  filter(between(doy,175,300))
srerfocus2020 <- srer %>%
  filter(year == "2020") %>%
  # mutate(time = format(endDateTime,"%D:%H:%M")) %>%
  filter(between(doy,175,300))
srerfocus2021 <- srer %>%
  filter(year == "2021") %>%
  # mutate(time = format(endDateTime,"%D:%H:%M")) %>%
  filter(between(doy,175,300))
srerfocus2022 <- srer %>%
  filter(year == "2022") %>%
  # mutate(time = format(endDateTime,"%D:%H:%M")) %>%
  filter(between(doy,175,300))

# the commented out lines were used previously to generate a unique timestamp for each entry instead of having hours of the day repeat over and over, but then it just made the x axis impossible to read and I couldn't change the axis intervals

# ordering them by doy so that when I create a "time unit" for my x axis (instead of using endDateTime which will just show as months or the time column which will overcrowd the axis) those units will be in numerical order
sfocus18_order <- srerfocus2018 %>%
  arrange(as.character(doy))
sfocus19_order <- srerfocus2019 %>%
  arrange(as.character(doy))
sfocus20_order <- srerfocus2020 %>%
  arrange(as.character(doy))
sfocus21_order <- srerfocus2021 %>%
  arrange(as.character(doy))
sfocus22_order <- srerfocus2022 %>%
  arrange(as.character(doy))

# generating time units as timestamps
sfocus18_order$time_unit <- seq.int(nrow(sfocus18_order))
sfocus19_order$time_unit <- seq.int(nrow(sfocus19_order))
sfocus20_order$time_unit <- seq.int(nrow(sfocus20_order))
sfocus21_order$time_unit <- seq.int(nrow(sfocus21_order))
sfocus22_order$time_unit <- seq.int(nrow(sfocus22_order))

# visualizing minutes of precipitation
srer2018_focusplot <- sfocus18_order %>%
  ggplot(aes(x = time_unit,y = priPrecipBulk)) + 
  geom_line(linewidth = 0.5,colour = "#20A16F") +
  geom_point(size = 2, colour = "#20A16F") +
  labs(x="Time of Year by half-hours (2018)",
       y="Bulk precipitation (mm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
srer2018_focusplot

srer2019_focusplot <- sfocus19_order %>%
  ggplot(aes(x = time_unit,y = priPrecipBulk)) + 
  geom_line(linewidth = 0.5,colour = "#20A16F") +
  geom_point(size = 2, colour = "#20A16F") +
  labs(x="Time of Year by half-hours (2019)",
       y="Bulk precipitation (mm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
srer2019_focusplot

srer2020_focusplot <- sfocus20_order %>%
  ggplot(aes(x = time_unit,y = priPrecipBulk)) + 
  geom_line(linewidth = 0.5,colour = "#20A16F") +
  geom_point(size = 2, colour = "#20A16F") +
  labs(x="Time of Year by half-hours (2020)",
       y="Bulk precipitation (mm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
srer2020_focusplot

srer2021_focusplot <- sfocus21_order %>%
  ggplot(aes(x = time_unit,y = priPrecipBulk)) + 
  geom_line(linewidth = 0.5,colour = "#20A16F") +
  geom_point(size = 2, colour = "#20A16F") +
  labs(x="Time of Year by half-hours (2021)",
       y="Bulk precipitation (mm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
srer2021_focusplot

srer2022_focusplot <- sfocus22_order %>%
  ggplot(aes(x = time_unit,y = priPrecipBulk)) + 
  geom_line(linewidth = 0.5,colour = "#20A16F") +
  geom_point(size = 2, colour = "#20A16F") +
  labs(x="Time of Year by half-hours (2022)",
       y="Bulk precipitation (mm)") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
srer2022_focusplot

# saving plots
ggsave("SRERplots/srer2018_plot.png",plot=srer2018_plot,scale=1,dpi=600)
ggsave("SRERplots/srer2019_plot.png",plot=srer2019_plot,scale=1,dpi=600)
ggsave("SRERplots/srer2020_plot.png",plot=srer2020_plot,scale=1,dpi=600)
ggsave("SRERplots/srer2021_plot.png",plot=srer2021_plot,scale=1,dpi=600)
ggsave("SRERplots/srer2022_plot.png",plot=srer2022_plot,scale=1,dpi=600)

ggsave("SRERplots/srer2018_focusplot.png",plot=srer2018_focusplot,scale=1,dpi=600)
ggsave("SRERplots/srer2019_focusplot.png",plot=srer2019_focusplot,scale=1,dpi=600)
ggsave("SRERplots/srer2020_focusplot.png",plot=srer2020_focusplot,scale=1,dpi=600)
ggsave("SRERplots/srer2021_focusplot.png",plot=srer2021_focusplot,scale=1,dpi=600)
ggsave("SRERplots/srer2022_focusplot.png",plot=srer2022_focusplot,scale=1,dpi=600)

