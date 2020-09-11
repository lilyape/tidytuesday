library(tidyverse)
library(tidytext)
library(showtext)
options(scipen = 999)

#Get the data
friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')

main_ch <- c("Ross Geller", "Monica Geller", "Rachel Green", "Joey Tribbiani", "Phoebe Buffay", "Chandler Bing")

friends_words <- friends %>%
  subset(speaker %in% main_ch) %>%
  unnest_tokens(word, text) %>%
  count(speaker, word, sort = TRUE) %>%
  anti_join(stop_words)

#Add TFIDF
friends_tfidf <- friends_words %>%
  bind_tf_idf(word, speaker, n)

#Formatting
font_add("friends", "C:/Users/lilya/AppData/Local/Microsoft/Windows/Fonts/GABRWFFR.ttf")
showtext_auto()
palette <- c("#F6D400", "#941205", "#008F47", "#9787CD", "#3F9DD4", "#F74035", "#9A6324")

friends_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(speaker) %>%
  top_n(10) %>% 
  ungroup() %>%
  ggplot(aes(x = word, y = tf_idf, fill = speaker)) +
  geom_col(show.legend = FALSE) +
  scale_y_continuous() +
  labs(x = "Words", y = "TF-IDF",
       title = "THE ONE WITH THE MOST CHARACTERIZING WORDS",
       subtitle = "based on the TF-IDF statistic") + 
  facet_wrap(~speaker, ncol = 3, scales = "free") + 
  coord_flip() +
  theme_minimal() +
  scale_fill_manual(values = palette) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.title = element_text(family = "friends", color = "grey", size = 20,  hjust = 0.5),
        plot.subtitle = element_text(family = "friends", color = "grey", hjust = 0.5),
        panel.background = element_rect(fill = "black"),
        panel.border = element_rect(fill=NA, color="black"),
        plot.background = element_rect(fill = "black"),
        strip.text = element_text(color = "grey", family = "friends", size = 12),
        axis.title.x = element_text(color = "grey", family = "friends"),
        axis.text.x = element_text(color = "grey"),
        axis.title.y = element_text(color = "grey", family = "friends"),
        axis.text.y = element_text(color = "grey"))