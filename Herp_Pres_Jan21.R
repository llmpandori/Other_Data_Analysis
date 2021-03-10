######################################
# Title: Herp Figs Quick 
# Author: LP
# Purpose: generate herp figs for MEDN meeting
#####################################

# load packages
library(tidyverse)
library(PNWColors)
library(ggrepel)

# load summary data for CHNY (total counts) and tidy
total <- read_csv("Raw Data/Herp_totals.csv")

total2 <- total %>%
    # convert from wide to long format
    pivot_longer(!Year, names_to = 'Array', values_to = 'Count') %>%
    # replace blanks with 0's
    mutate(Count = if_else(is.na(Count), 0, Count))
  
  
# make stacked bar w/ abundance across years (color by array)
total2$Array <- factor(total2$Array, levels = unique(total2$Array))

ggplot(data = total2, mapping = aes(x = Year, y = Count, fill = Array)) + 
  geom_bar(stat = 'identity') + 
  ggtitle('Orange-throated Whiptail Abundance') +
  ylab('Count') +
  theme_classic() +
  theme(text = element_text(size = 16),
        # make panels slightly farther apart
        panel.spacing = unit(2, 'lines'),
        # no background to wrap panels
        strip.background = element_blank(),
        strip.text = element_text(size = 18, color = 'black',face = 'italic'),
        # panel labels outside x axis labels
        strip.placement = 'outside',
        # tilt and adjust position of x axis labels
        axis.text.y = element_text(size = 16, color = 'black'),
        axis.text.x = element_text(size = 16, color = 'black'),
        legend.text = element_text(size = 16, color = 'black'),
        # make caption serif text to match journal style
        plot.caption = element_text(family = 'serif', 
                                    size = 16, color = 'black', hjust = 0))

ggsave('whiptail_bar_2020.png')

# other ideas?
all <- read_csv("Raw Data/Herp_all.csv")

# tidy
all2 <- all %>%
  select(SurveyYear, Array, Type, AdjSpecies, Include) %>%
  # filter for things to include?
  filter(Include == 1) %>%
  # filter for items that have years
  filter(!is.na(SurveyYear)) %>%
  group_by(SurveyYear, Type) %>%
  summarize(count = length(Type)) %>%
  filter(SurveyYear == 2020) %>%
  rename(Count = count) %>%
  # calculate proportion %>%
  mutate(Proportion = Count/261,
         ymax = cumsum(Proportion),
         ymin = c(0, head(ymax, n=-1)),
         labelPosition = (ymax + ymin)/2,
         label = paste0(Type, " (", Count, ")"))


# make pie chart of things caught in 2019?

ggplot(data = all2, mapping = aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Type)) + 
  geom_rect() + 
  geom_text(x = 4, aes(y = labelPosition, label = label), size = 4.5) +
  coord_polar(theta = 'y') + 
  xlim(c(-1,4)) +
  theme_void() +
  theme(text = element_text(size = 16),
      legend.position = 'none')

ggsave('herp_donut_2020.png')

# make bar graph of things caught in 2019
ggplot(data = all2, mapping = aes(x = Type, y = Count, fill = Type)) + 
  geom_bar(stat = 'identity') + 
  ggtitle('Abundance of Organisms Caught in 2020') +
  theme_classic() +
  theme(text = element_text(size = 14),
        # tilt and adjust position of x axis labels
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.text.x = element_text(size = 12, color = 'black'),
        legend.position = 'none')

ggsave('herp_abundance_2020.png')
