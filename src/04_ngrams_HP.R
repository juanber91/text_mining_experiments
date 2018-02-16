source('lib/helpers.R')
cargar_paquetes('tidytext',  'janerowlingr', 'ggraph', 'igraph', 'widyr')

rowling_books <- function(){
  df <- read_csv('data/HP_books.csv') %>% na.omit()
  return(df)
}

# Tokenizamos por n-gramas ----------
rowling_bigrams <- rowling_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

rowling_bigrams

# bigramas más comunes
rowling_bigrams %>%
  count(bigram, sort = TRUE)

# filtramos las stop words
bigrams_separated <- rowling_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

# trigramas
rowling_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

# Analizamos los bigramas -----------

# Profesores más mencionados
bigrams_filtered %>%
  filter(word1 == "professor") %>%
  count(book, word2, sort = TRUE)

# tf-idf
bigram_tf_idf <- bigrams_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

bigram_tf_idf %>%
  mutate(word = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 3, scales = "free") +
  coord_flip()

bigram_tf_idf %>% 
  filter(tf_idf > 0.0015) %>% 
  # filter(book == 'Harry Potter and the Deathly Hallows') %>% 
  ggplot(aes(book, bigram, color = book)) +
  geom_jitter(aes(size = tf_idf), height = 0, width = 0)
  

# Bigramas y sentiment analysis -------------

# Palabras precedidas por 'not' que deberían ser negativas y no positivas
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, score, sort = TRUE) %>%
  ungroup()

not_words

not_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment score * number of occurrences") +
  coord_flip()

# otras palabras de negación
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, score, sort = TRUE) %>%
  ungroup()

negated_words %>%
  mutate(contribution = n * score) %>%
  arrange(desc(abs(contribution))) %>%
  group_by(word1) %>% 
  top_n(12, abs(contribution)) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  arrange(contribution) %>% 
  ggplot(aes(word2, n * score, fill = n * score > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by negation words") +
  ylab("Sentiment score * number of occurrences") +
  facet_wrap(~word1, scales = 'free') +
  coord_flip()


# Grafos -------
bigram_graph <- bigram_counts %>%
  filter(n > 50) %>%
  graph_from_data_frame()

bigram_graph

set.seed(2017)f

# (pimpeo)
a <- grid::arrow(type = "closed", length = unit(.15, "inches"), angle = 20)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


# Correlating pairs of words -------

# Calculamos secciones de 10 líneas
rowling_section_words <- rowling_books() %>%
  # filter(book == "Harry Potter and the Deathly Hallows") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

rowling_section_words

# count words co-occuring within sections
word_pairs <- rowling_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_pairs

# word co-occuring with 'darcy'
word_pairs %>%
  filter(item1 == "minister")

# Calculamos la phi de Pearson para medir la correlación entre palabras que 
# aparecen juntas, una sí y la otra no, o ninguna

# we need to filter for at least relatively common words first
word_cors <- rowling_section_words %>%
  group_by(word) %>%
  filter(n() >= 100) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors

# ¿Nos interesa alguna palabra en particular?
word_cors %>%
  filter(item1 == "harry")

word_cors %>%
  filter(item1 %in% c("harry", "dark", "wand", "magic")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation, fill = item1)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()


# Pairs of words in Pride and Prejudice that show at least a .15 correlation 
# of appearing within the same 10-line section
word_cors %>%
  filter(correlation > .25) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
