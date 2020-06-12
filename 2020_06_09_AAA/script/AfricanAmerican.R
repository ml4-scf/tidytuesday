library(tidyverse)


firsts <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv'
  )
science <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv'
  )


data <- firsts %>%
  mutate(category = as_factor(category),
         decade = year - year %% 10)




plot <- ggplot(data, aes(x = decade, fill = category)) +
  geom_bar(colour="white")+
  labs(x = "", y= "", title = "African Americans' firsts", subtitle = "Number of first achievements by African Americans by category and decade ")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(1730, 2010, by = 10))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.line.x = element_line(colour="black"),
        axis.ticks = element_line(),
        legend.key.width = unit(0.7, units = "cm"),
        legend.key.height = unit(0.1, units = "cm"),
        legend.position = "top",
        legend.justification = "left",
        legend.text = element_text(size=8),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        # panel.background = element_rect(fill ="slategray"),
        plot.background = element_rect(fill ="slategray"))


ggsave(filename="2020_06_09_AAA\\output\\AAA.png", scale = 1.15)
