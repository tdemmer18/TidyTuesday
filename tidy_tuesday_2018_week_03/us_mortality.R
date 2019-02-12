remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(readxl)
library(tidyverse)
library(ggrepel)

df <- read_excel("~/rstats/rviz/tidy_tuesday_2018_week_03/global_mortality.xlsx")

glimpse(df)

### Gather data into tidy long format
df_gathered <- df %>%
  gather(disease, percent, -country, -country_code, -year)

### Look at Diabetes in the USA, CAN, MEX
df_diabetes <- df_gathered %>%
  mutate(pct_rounded = round(percent, 2)) %>%
  na.omit() %>%
  filter(disease == "Diabetes (%)",
         country %in% c("United States", "Canada", "Mexico"))

### view head of data
head(df_diabetes)

### plot diabetes in the USA, CAN and MEX with a line graph (geom_smooth)
df_diabetes %>%
  ggplot(aes(x = year, y = percent, color = country)) +
  geom_smooth(method = 'loess', formula = 'y ~ x', se = FALSE) +
  geom_text_repel(data = subset(df_diabetes, year == "2013"), aes(label = country_code), vjust = -1) +
  geom_text_repel(data = subset(df_diabetes, year == "1990"), aes(label = pct_rounded), vjust = .4) +
  geom_text_repel(data = subset(df_diabetes, year == "2015"), aes(label = pct_rounded), vjust = 1.4) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) +
  scale_x_continuous(limits = c(1990, 2015), breaks = seq(1990, 2015, 5)) +
  theme(legend.position = "none",
        rect = element_rect(fill = "#212F3C"),#0a1926
        panel.grid = element_blank(),
        text = element_text(colour = "white"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(colour = rgb(230, 230, 230, maxColorValue = 255), size = 11),
        plot.title = element_text(hjust = 0.5)) +
  labs(x = "",
       y = "Percent",
       title = "Diabetes in North America",
       caption = "@tdemmer18 | 2019-02-02 \n TidyTuesday 2018 - Week 03")
