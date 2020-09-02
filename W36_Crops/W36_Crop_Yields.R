library(readr)
library(tidyverse)
library(harrypotter)
library(extrafont)
options(scipen = 999)
theme_set(theme_minimal())

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')


long_crops <- key_crop_yields %>% 
  pivot_longer(cols = 4:last_col(),
               names_to = "crop", 
               values_to = "crop_production") %>% 
  mutate(crop = str_remove_all(crop, " \\(tonnes per hectare\\)")) %>% 
  set_names(nm = names(.) %>% tolower())


turkey_long_crops <- long_crops %>%
  filter(code == "TUR") 

turkey_long_crops <- turkey_long_crops[complete.cases(turkey_long_crops),]

font_import()
loadfonts(device = "win")

turkey_long_crops %>% 
  ggplot(aes(x = year, y = crop_production)) +
  geom_point(aes(color = crop), size = 1) + 
  geom_smooth(aes(color = crop), se = FALSE, size = 1.5) +
  facet_wrap(~crop, scales = "free_y") + 
  scale_colour_hp(discrete = TRUE, option = "Hufflepuff", guide = FALSE) +
  theme(text = element_text(size = 15, family = "Georgia", color = "grey20")) +
  labs(title = "Key Crop Yields in Turkey",
       subtitle = "1961-2018",
       x = "Year",
       y = "Crop Production (tonnes per hectare)",
       caption = "Data: Our World in Data")



