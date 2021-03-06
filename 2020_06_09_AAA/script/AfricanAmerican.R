library(tidyverse)


firsts <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv'
  )


data <- firsts %>%
  mutate(category = as_factor(category),
         decade = year - year %% 10)




plot <- ggplot(data, aes(x = decade, fill = category)) +
  geom_bar(colour = "floralwhite") +
  annotate(
    geom = "curve",
    x = 1760,
    y = 26,
    xend = 1730,
    yend = 2,
    arrow = arrow(length = unit(2, "mm")),
    curvature = .3,
    size = 1.2,
    colour = "floralwhite"
  ) +
  annotate(geom = "text",
           x = 1761,
           y = 27,
           label = data$accomplishment[1],
           hjust = "left",
           size = 4,
           colour = "floralwhite") +
  annotate(geom = "text",
           x = 1761,
           y = 24.5,
           label = data$person[1],
           hjust = "left",
           size = 2.5,
           colour = "floralwhite")+
  annotate(
    geom = "curve",
    x = 1880,
    y = 45,
    xend = 2005,
    yend = 7,
    arrow = arrow(length = unit(2, "mm")),
    curvature = .2,
    size = 1.2,
    colour = "floralwhite"
  ) +
  annotate(geom = "text",
           x = 1880,
           y = 48,
           label = data$accomplishment[428],
           hjust = 0.65,
           size = 4,
           colour = "floralwhite") +
  annotate(geom = "text",
           x = 1880,
           y = 45.5,
           label = stringr::str_replace(data$person[428],"\\[226\\]",""),
           hjust = 4.16,
           size = 2.5,
           colour = "floralwhite")+
  labs(
    x = "",
    y = "",
    title = "African-American firsts",
    subtitle = "Number of first achievements by African-Americans by category and decade",
    caption = "Source: https://en.wikipedia.org/wiki/List_of_African-American_firsts"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = seq(1730, 2010, by = 10)) +
  theme(
    text = element_text(colour = "floralwhite"),
    line = element_line(colour = "floralwhite"),
    axis.text = element_text(colour = "floralwhite"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.line.x = element_line(colour = "floralwhite"),
    axis.ticks = element_line(),
    axis.ticks.y = element_blank(),
    legend.key.width = unit(0.7, units = "cm"),
    legend.key.height = unit(0.1, units = "cm"),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    legend.spacing = unit(0.3, units = "cm"),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "slategray", colour = "floralwhite")
  )
plot

ggsave(filename = "2020_06_09_AAA\\output\\AAA.png")
