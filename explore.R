library(dplyr)
library(tidyr)
library(ggplot2)
library(xlsx)
df <- read.csv('/Users/travis/Documents/gits/Data/IED_distscores.csv', stringsAsFactors = F)
df_og <- read.xlsx('/Users/travis/Documents/gits/Data/IED_18Feb.xlsx', sheetIndex = 2)

df %>%
  separate(item_1, c('scale_1', 'item_1'), '-') %>%
  separate(item_2, c('scale_2', 'item_2'), '-') -> df

df %>%
  mutate(same_scale = scale_1==scale_2) %>%
  group_by(scale_1, same_scale) %>%
  summarise(dist_score = mean(score),
            dist_sd = sd(score)) %>%
  ggplot(aes(x=same_scale, y=dist_score, group=scale_1)) + geom_line()

df %>%
  arrange(score) %>%
  mutate(Scale = paste(scale_1, item_1, sep='-')) %>%
  left_join(df_og[,c('Scale', 'RawContent')]) %>%
  mutate(content_scale1 = RawContent) %>%
  select(scale_1:score, content_scale1) %>%
  mutate(Scale = paste(scale_2, item_2, sep='-')) %>%
  left_join(df_og[,c('Scale', 'RawContent')]) %>%
  mutate(content_scale2 = RawContent) %>%
  select(scale_1:score, content_scale1, content_scale2) -> df

df %>% 
  filter(score!=0) %>% 
  select(score, content_scale1, content_scale2) %>% 
  slice(1:5)

df %>% 
  arrange(desc(score)) %>% 
  select(score, content_scale1, content_scale2) %>% 
  slice(1:5)
