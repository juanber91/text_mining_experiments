source('lib/helpers.R')
cargar_paquetes('topicmodels', 'tidytext', 'gutenbergr', 'scales')

rowling_books <- function(){
  df <- read_csv('data/HP_books.csv') %>% na.omit()
  return(df)
}


custom_stop_words <-
  bind_rows(data_frame(
    word = c("miss", 'dumbledore', 'harry', 'hagrid', 'ron', 'snape',
             'hermione', 'looked', 'professor'),
    lexicon = c("custom")
  ),
  stop_words)



# Ejemplo: library heist -----------
tidy_books <- rowling_books() %>%
  group_by(book) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, book, chapter)

# split into words
by_chapter_word <- tidy_books %>%
  unnest_tokens(word, text)

# find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(custom_stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

# Casteamos a una DTM para poder correr el modelo
chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

chapters_lda <- LDA(chapters_dtm, k = 7, control = list(seed = 1234))
chapter_topics <- tidy(chapters_lda, matrix = "beta")

# Top 5 términos por tópico
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Tópicos por documento
chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma

# reorder titles in order of topic 1, topic 2, etc before plotting
chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

# ¿Qué pasó con Great Expectations?
chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

# Sólo son dos capítulos dentro del libro que no fueroncorrectamente catalogados
chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)

# bla -------
assignments <- augment(chapters_lda, data = chapters_dtm)
assignments

assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))

assignments

assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")

wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))
