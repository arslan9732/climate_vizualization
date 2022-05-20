library(tidyverse)
library(plotly)


annual_temp <-
  read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(years = Year, t_diff = `J-D`)

t_plot <- annual_temp %>%
  ggplot(aes(x = years, y = t_diff)) +
  geom_line(aes(color = "1"), size = 0.5, show.legend = FALSE) +
  geom_point(
    fill = "white",
    aes(color = "1"),
    shape = 21,
    show.legend = TRUE
  ) +
  geom_smooth(
    se = FALSE,
    aes(color = "2"),
    size = 0.25,
    span = 0.15,
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = seq(1880, 2023, 20), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.5, 1.5), expand = c(0, 0)) +
  scale_color_manual(
    name = NULL,
    breaks = c(1, 2),
    values = c("grey", "black"),
    labels = c("Annual mean", "Lowess smoothing"),
    guide = guide_legend(override.aes = list(shape = 15, size = 5))
  ) +
  
  labs(
    x = "YEAR",
    y = "Temperature Anomaly (C)",
    title = "GLOBAL LAND-OCEAN TEMPERATURE INDEX",
    subtitle = "Data source: NASA's Goddard Institute for Space Studies (GISS). \nCredit: NASA/GISS"
  ) +
  
  theme_light() +
  theme(
    axis.ticks = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(
      margin = margin(b = 10),
      color = "red",
      face = "bold"
    ),
    plot.subtitle = element_text(size = 8, margin = margin(b = 10)),
    legend.position = c(0.15, 0.9),
    legend.title = element_text(size = 0),
    legend.key.height = unit(10, "pt"),
    legend.margin = margin(0, 0, 0, 0),
    legend.key = element_rect(color = "grey"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "figures/temperature_index_plot.svg",
  plot = t_plot,
  width = 10,
  height = 7
)
