source('lib/helpers.R')
cargar_paquetes('tidytext', 'gutenbergr', 'scales')

# Descargamos los libros desde Project Gutenberg
homer <- gutenberg_download(c(6130, 1727))
shakespeare <- gutenberg_download(c(100))
sophocles <- gutenberg_download(c(31))

# Limpiamos y tokenizamos
tidy_homer <- homer %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_shakespeare <- shakespeare %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_sophocles <- sophocles %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

tidy_homer %>%
  count(word, sort = TRUE)

tidy_shakespeare %>%
  count(word, sort = TRUE)

tidy_sophocles %>%
  count(word, sort = TRUE)

frequency <- bind_rows(mutate(tidy_homer, author = "Homer"),
                       mutate(tidy_shakespeare, author = "Shakespeare"),
                       mutate(tidy_sophocles, author = "Sophocles")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, Shakespeare:Sophocles)

ggplot(frequency, aes(x = proportion, y = Homer, color = abs(Homer - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Homer", x = NULL)

cor.test(data = frequency[frequency$author == "Shakespeare",], 
         ~ proportion + Homer)

cor.test(data = frequency[frequency$author == "Sophocles",], 
         ~ proportion + Homer)
