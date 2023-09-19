library(tidyverse)
library(ggplot2)
library(ggthemes)

moist <- list.files(path="SRER_moisture_compiled", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

# adding year and DOY columns
datetime <- moist$endDateTime
moist <- moist %>%
  select(startDateTime,endDateTime,VSWCMean)%>%
  mutate(year = as.numeric(format(datetime, format ="%Y"))) %>%
  mutate(doy = yday(endDateTime))

# summarising every 30 minutes into a single doy value for broader picture of annual precipitation
# keeping srer as a separate dataframe so that if we wanted to look at intra-annual precipitation we still have that option
moistsum <- moist %>%
  type.convert(as.is=TRUE) %>%
  group_by(doy,year) %>%
  summarise(daily_vswc = sum(VSWCMean))

vswc18 <- moistsum %>%
  filter(year == "2018")
vswc19 <- moistsum %>%
  filter(year == "2019")
vswc20 <- moistsum %>%
  filter(year == "2020")
vswc21 <- moistsum %>%
  filter(year == "2021")
vswc22 <- moistsum %>%
  filter(year == "2022")

# visualizing annual trends
vswc18_plot <- vswc18 %>%
  ggplot(aes(x = doy,y=daily_vswc)) +
  geom_line(linewidth = 1) +
  labs(x = "Time (days)",
       y = "Volumetric soil water content") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
vswc18_plot

vswc19_plot <- vswc19 %>%
  ggplot(aes(x = doy,y=daily_vswc)) +
  geom_line(linewidth = 1) +
  labs(x = "Time (days)",
       y = "Volumetric soil water content") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
vswc19_plot

vswc20_plot <- vswc20 %>%
  ggplot(aes(x = doy,y=daily_vswc)) +
  geom_line(linewidth = 1) +
  labs(x = "Time (days)",
       y = "Volumetric soil water content") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
vswc20_plot

vswc21_plot <- vswc21 %>%
  ggplot(aes(x = doy,y=daily_vswc)) +
  geom_line(linewidth = 1) +
  labs(x = "Time (days)",
       y = "Volumetric soil water content") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
vswc21_plot

vswc22_plot <- vswc22 %>%
  ggplot(aes(x = doy,y=daily_vswc)) +
  geom_line(linewidth = 1) +
  labs(x = "Time (days)",
       y = "Volumetric soil water content") +
  theme_clean() +
  theme(axis.title = element_text(size = 12,face = "bold",),
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10))
vswc22_plot

ggsave("plots/srer_vswc_concept2018.png",plot=vswc18_plot,scale=1,dpi=600)
ggsave("plots/srer_vswc_concept2019.png",plot=vswc19_plot,scale=1,dpi=600)
ggsave("plots/srer_vswc_concept2020.png",plot=vswc20_plot,scale=1,dpi=600)
ggsave("plots/srer_vswc_concept2021.png",plot=vswc21_plot,scale=1,dpi=600)
ggsave("plots/srer_vswc_concept2022.png",plot=vswc22_plot,scale=1,dpi=600)

