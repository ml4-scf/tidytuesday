library(tidyverse)
library(lubridate)
library(ggthemes)

## Load data

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

## Transforming and Wrangling

# Recode missing dates to NA's
df <- gdpr_violations %>%
  mutate(date = ifelse((date=="01/01/1970"),NA, date)) %>%
  mutate(date=mdy(date))


# Highest 5 Fines by Country
plot1 <- df %>%
  group_by(name) %>%
  summarise(max_fine=max(price),count=n()) %>%
  filter(max_fine>0) %>%
  arrange(desc(max_fine))

ggplot(plot1[1:5,], aes(x=reorder(name,max_fine),y=max_fine))+
  geom_point(size=5)+
  coord_flip()+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))+ # set off scientific notation
  scale_x_discrete()+
  labs(title="Top 5 Highest GDPR Fines", x="",y="" )+
  theme_wsj()+
  theme(legend.position = c(0.8,0.3))

# Trying out themes

ggplot(plot1, aes(x=reorder(name,count),y=count))+
  geom_point(aes(size=max_fine),color="black", shape =21, fill="lightblue")+
  coord_flip()+
  scale_x_discrete()+
  scale_size_continuous(name="Fine in €")+
  labs(title="Top 5 Highest GDPR Fines", x="",y="", size="Fine in €" )+
  theme(
    axis.text = element_text(color = "white", rel(1.5)),
    axis.ticks = element_line(color="white"),
    axis.line.x = element_line(color = "lightgrey"),
    axis.line.y = element_line(color = "lightgrey"),
    panel.grid.major.y = element_line(color="lightgrey", linetype = "dotted"),
    legend.position = c(0.8,0.3),
    legend.background = element_rect(colour = "black"),
    legend.title = element_text(),
    panel.background= element_rect(fill="white"),
    plot.background = element_rect(fill = "darkblue", color = "white"),
    plot.title = element_text(color="white"))
