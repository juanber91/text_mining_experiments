source('lib/helpers.R')
cargar_paquetes('tidytext',  'janeaustenr')

# source('src/01_tidy_text.R')
# Agregamos las palabras al stop_words que no figuran antes
custom_stop_words <- bind_rows(data_frame(word = c("miss"), 
                                          lexicon = c("custom")), 
                               stop_words)

# Term frequency -------

# Contamos el número de veces que aparece cada palabra en cada novela
book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

# Total de palabras por novela
total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

book_words

ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")


# Zipf's law -------------
# Zipf’s law states that the frequency that a word appears is
# inversely proportional to its rank.

freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         `term frequency` = n/total)

freq_by_rank

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_abline(intercept = -0.08, slope = -1.3, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()

# La relación rango-frecuencia es negativa, pero no constante.
# Tal vez es en potencias:
rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)

lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)


# bind_tf_idf ----------
book_words <- book_words %>%
  bind_tf_idf(word, book, n)
book_words

book_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()
