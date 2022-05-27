library(tidyverse)
library(glue)
library(scales)

t_data <- read_csv("data/GLB.Ts+dSST.csv", skip = 1, na = "***") %>%
  select(year = Year, t_diff = `J-D`) %>%
  drop_na()

annotation <- t_data %>%
  arrange(year) %>%
  slice(1, n()) %>%
  mutate(t_diff = 0,
         x = year + c(-5, 5))

max_t_diff <- format(round(max(t_data$t_diff), 1), nsmall = 1)

t_plot <- t_data %>%
  ggplot(aes(x = year, y = t_diff, fill = t_diff)) +
  geom_col(show.legend = FALSE) +
  geom_text(data = annotation, aes(x = x, label = year), color = "white") +
  # scale_fill_gradientn(colors = c("darkblue", "white", "darkred"),
  #                      values = rescale(c(min(t_data$t_diff), 0, max(t_data$t_diff))),
  #                      limits = c(min(t_data$t_diff), max(t_data$t_diff))) +
  scale_fill_stepsn(
    colors = c("darkblue", "white", "darkred"),
    values = rescale(c(
      min(t_data$t_diff), 0, max(t_data$t_diff)
    )),
    limits = c(min(t_data$t_diff), max(t_data$t_diff)),
    n.breaks = 9
  ) +
  labs(
    title = glue(
      "Global temperature have increased by over {max_t_diff}\u00B0C since {min(t_data$year)}"
    )
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black"),
    plot.title = element_text(size = 20,
                              face = "bold",
                              color = "white")
  )

ggsave(
  plot = t_plot,
  filename = "figures/temperature_bar_plot.png",
  width = 7,
  height = 4
)
