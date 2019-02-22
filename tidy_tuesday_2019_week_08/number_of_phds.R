library(tidyverse)


#### SET UP THEME
my_font <- "Comfortaa Bold"
my_background <- "#f5f5f4"
my_color <- "#22211d"
my_theme <- theme(text = element_text(family = my_font, color = my_color),
                  rect = element_rect(fill = my_background),
                  plot.background = element_rect(fill = my_background, color = NA),
                  panel.background = element_rect(fill = my_background, color = NA),
                  panel.border = element_blank(),
                  legend.background = element_rect(fill = my_background, color = NA),
                  legend.key = element_rect(fill = my_background),
                  plot.caption = element_text(size = 10))

theme_set(theme_light() + my_theme)

#### SET UP DATE FOR CAPTION
today <- Sys.Date()

#### READ IN DATA
phd_field <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-02-19/phd_by_field.csv")

##### TAKE A LOOK AT THE DATA AND VIEW STRUCTURE
View(phd_field)
str(phd_field)

#### GROUP BY YEAR AND MAJOR FIELD
phd_year_major <- phd_field %>%
  na.omit() %>%
  group_by(year, major_field) %>%
  summarise(total_phds_awarded = sum(n_phds)) %>%
  ungroup() %>%
  arrange(desc(total_phds_awarded))





View(phd_year_major)

#### GROUP BY YEAR AND MAJOR FIELD
phd_year_major_history <- phd_field %>%
  filter(major_field == "History") %>%
  na.omit() %>%
  group_by(year, field) %>%
  summarise(total_phds_awarded = sum(n_phds)) %>%
  ungroup() %>%
  arrange(desc(total_phds_awarded))

View(phd_year_major_history)




#### GRAPH ALL MAJORS WITH LINE CHART
phd_year_major %>%
  ggplot(aes(year, total_phds_awarded, color = major_field)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none") +
  gghighlight::gghighlight(major_field %in% c("History", "Psychology", "Biological and biomedical sciences", "Physics and astronomy")) +
  scale_x_continuous(breaks = c(seq(2008, 2017, by = 9))) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("steelblue", "#2E86C1")) +
  labs(
    title = "Total PhD's Awarded from 2008 to 2017 in the United States",
    y = "",
    x = "",
    caption = paste0("@tdemmer18 | ", today)
  ) +
  theme(plot.title = element_text(hjust = 0.5))


#### FILTER FOR HISTORY AND GRAPH WITH LINE CHART
phd_year_major %>%
  filter(major_field %in% c('History')) %>%
  ggplot(aes(year, total_phds_awarded, color = major_field)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(seq(2008, 2017, by = 1))) +
  scale_fill_manual(values = c("steelblue")) +
  labs(
    title = "History PhD's Awarded",
    y = "",
    x = "",
    caption = paste0("@tdemmer18 | ", today)
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.ticks = element_blank(),
        panel.grid = element_blank())

View(phd_year_major_history)

phd_year_major_history %>%
  ggplot(aes(year, total_phds_awarded, color = field)) +
  geom_point() +
  geom_line() +
  theme(legend.position = "none") +
  gghighlight::gghighlight(major_field %in% c("American history, United States and Canada")) +
  scale_x_continuous(breaks = c(seq(2008, 2017, by = 9))) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("steelblue", "#2E86C1")) +
  labs(
    title = "Total PhD's Awarded from 2008 to 2017 in the United States",
    y = "",
    x = "",
    caption = paste0("@tdemmer18 | ", today)
  ) +
  theme(plot.title = element_text(hjust = 0.5))















#### FILTER FOR HISTORY AND GRAPH WITH BAR CHART
phd_year_major %>%
  filter(major_field %in% c('History')) %>%
  ggplot(aes(year, total_phds_awarded, fill = major_field)) +
  geom_col() +
  geom_text(aes(label = total_phds_awarded), hjust = 2, color = "white", size = 4) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = c(seq(2008, 2017, by = 1))) +
  scale_fill_manual(values = c("steelblue")) +
  coord_flip() +
  labs(
    title = "History PhD's Awarded",
    y = "",
    x = "",
    caption = paste0("@tdemmer18 | ", today)
  ) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank())
