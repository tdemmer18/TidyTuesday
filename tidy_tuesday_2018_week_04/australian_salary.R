remove(list = ls())
options(stringsAsFactors = FALSE)
options(scipen = 999)

library(tidyverse)
library(readr)
df
df <- read_csv("https://raw.githubusercontent.com/tdemmer18/tidytuesday/master/data/2018/2018-04-23/week4_australian_salary.csv")
df_over_200k <- df %>%
  filter(average_taxable_income > 200000)


df_over_200k %>%
  ggplot(aes(reorder(occupation, average_taxable_income), average_taxable_income, fill = gender)) +
  geom_col(stat = "identity") +
  #facet_wrap(~ gender, scales = "free_y", ncol = 1) +

  coord_flip() +
  scale_y_continuous(label = scales::comma)

df_over_200k %>%
  ggplot(aes(individuals, average_taxable_income, color = gender)) +
  geom_point() +
  facet_wrap(~ gender) +
  scale_y_continuous(label = scales::comma) +
  scale_x_continuous(label = scales::comma) +
  labs(x = "Average Taxable Income",
       y = "# of Individuals") +
  theme(legend.position = "none",
        rect = element_rect(fill = "#212F3C"),#0a1926
        panel.grid = element_blank(),
        text = element_text(colour = "white"),
        panel.background = element_rect(fill = "transparent", colour = "transparent"),
        axis.text = element_text(colour = rgb(230, 230, 230, maxColorValue = 255), size = 11),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
