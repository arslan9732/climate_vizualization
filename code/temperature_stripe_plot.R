library(tidyverse)
library(glue)
library(scales)

t_data <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, t_diff = `J-D`) %>%
  drop_na()

t_plot <- t_data %>%
  ggplot(aes(x = year, y = 1, fill = t_diff)) +
  geom_tile(show.legend = FALSE) +
  scale_fill_stepsn(
    colors = c("#08306B", "white", "#67000D"),
    values = rescale(c(
      min(t_data$t_diff), 0, max(t_data$t_diff)
    )),
    n.breaks = 12
  ) +
  coord_cartesian(expand = FALSE) +
  scale_x_continuous(breaks = seq(1890, 2020, 20)) +
  labs(title = glue(
    "Global temperature change ({min(t_data$year)}-{max(t_data$year)})"
  )) +
  theme_void() +
  theme(
    plot.title = element_text(
      color = "white",
      margin = margin(b = 5, t = 10, unit = "pt"),
      hjust = 0.05
    ),
    axis.text.x =  element_text(color = "white", margin = margin(
      t = 5, b = 10, unit = "pt"
    )),
    plot.background = element_rect(fill = "black"),
  )

ggsave(
  plot = t_plot,
  file = "figures/warmings_strips.png",
  width = 8,
  height = 4.5
)