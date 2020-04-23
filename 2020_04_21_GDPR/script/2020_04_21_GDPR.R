library(tidyverse)

## Load data

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

## Transforming and Wrangling


# Get count per country
df <- gdpr_violations %>%
  group_by(name) %>%
  summarise(count=n())

## Make plots

# Number of fines per country
ggplot(df, aes(x=reorder(name, count), y=count))+
  geom_col(color="black", fill="lightblue")+
  geom_label(aes(label = count), nudge_y = 1.5, color="lightblue", label.padding = unit(0.2, "lines"))+
  coord_flip()+
  scale_x_discrete()+
  labs(title="Which country imposed the most fines?", x="",y="", caption = "Source: https://www.privacyaffairs.com/gdpr-fines/")+
  theme(
    text = element_text(color = "white", family = "mono"),
    axis.text = element_text(color = "white"),
    axis.ticks = element_line(color="white"),
    axis.line.x = element_line(color = "lightgrey"),
    axis.line.y = element_line(color = "lightgrey"),
    panel.grid.major.y = element_line(color="lightgrey", linetype = "dotted"),
    panel.background= element_rect(fill="gray20"),
    plot.background = element_rect(fill = "gray20", color = "white"),
    plot.title = element_text(face="bold", size = 18))

ggsave(filename = "output\\gdpr_plot.png", scale=1.15)
