# load libraries
library(tidyverse)
library(extrafont) # fonts
library(gghighlight)

# load my theme ---------------------------
my_font <- "Comfortaa"
my_background <- "#f5f5f4"
my_color <- "#22211d"
my_theme <- theme(text = element_text(family = my_font, color = my_color),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  legend.background = element_rect(fill = my_background, color = NA),
                  legend.key = element_rect(fill = my_background),
                  plot.caption = element_text(size = 6))

theme_set(theme_light() + my_theme)

# load data

fed_rd <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/fed_r_d_spending.csv")
energy_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/energy_spending.csv")
climate_spend <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-12/climate_spending.csv")

View(fed_rd)
View(energy_spend)
View(climate_spend)

# Quick view of Federal R&D Spending over the years
fed_rd %>%
  #mutate(rd_budget = rd_budget/100000000) %>%
  ggplot(aes(year, rd_budget, color = department)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(seq(1975, 2015, by = 10))) +
  #gghighlight(department %in% c("DOD", "HHS", "NIH")) +
  labs(y = "",
       x = "",
       title = "federal r&d budget") +
  theme(axis.title.y=element_text(angle=90,hjust=1),
        axis.title.x=element_text(angle=0,hjust=1),
        plot.title = element_text(hjust = 0.5))


# Quick view of climate spending over the years
climate_spend %>%
  ggplot(aes(year, gcc_spending, color = department)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(seq(2000, 2017, by = 5))) +
  #gghighlight(department %in% c("DOD", "HHS", "NIH")) +
  labs(y = "",
       x = "",
       title = "climate spending") +
  theme(axis.title.y=element_text(angle=90,hjust=1),
        axis.title.x=element_text(angle=0,hjust=1),
        plot.title = element_text(hjust = 0.5))

# Quick view of energy spending over the years
energy_spend %>%
  ggplot(aes(year, energy_spending, color = department)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(seq(1997, 2018, by = 5))) +
  #gghighlight(department %in% c("DOD", "HHS", "NIH")) +
  labs(y = "",
       x = "",
       title = "energy spending") +
  theme(axis.title.y=element_text(angle=90,hjust=1),
        axis.title.x=element_text(angle=0,hjust=1),
        plot.title = element_text(hjust = 0.5))



