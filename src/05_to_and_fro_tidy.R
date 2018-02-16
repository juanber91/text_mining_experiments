source('lib/helpers.R')
cargar_paquetes('tm', 'topicmodels', 'tidytext', 'quanteda', 'acq')

# Tidying a document-term matrix (DTM) -----------
# Cargamos la BBDD de artículos de la AP
data("AssociatedPress", package = "topicmodels")
AssociatedPress

terms <- Terms(AssociatedPress)
head(terms)

# La base no está en formato tidy, usamos la función broom::tidy para esto.
# Aparentemente no contiene stopwords.
ap_td <- tidy(AssociatedPress)
ap_td

# Podemos hacer análisis de sentimientos
ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

# Tidying a document-feature matrix (DFM) -----------
data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)
inaug_dfm

# tidyamos
inaug_td <- tidy(inaug_dfm)
inaug_td

# Podemos e.g. calcular el tf-idf
inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf

# Ahora seleccionamos algunas palabras y nos fijamos en qué tanto se utilizaron
# en el tiempo
year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))

year_term_counts %>%
  filter(term %in% c("god", "america", "children", "people",
                     "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")


# De formato tidy a DTM y DFM -------
# DTM
ap_td %>%
  cast_dtm(document, term, count)

# DFM
ap_td %>%
  cast_dfm(document, term, count)

# Matriz
ap_td %>%
  cast_sparse(document, term, count)

# Lo mismo para los libros de HP
rowling_books <- function(){
  df <- read_csv('data/HP_books.csv') %>% na.omit()
  return(df)
}

HP_dtm <- rowling_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("chapter", 
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>% 
  count(book, word) %>%
  cast_dtm(book, word, n)

HP_dtm

# Tidy a objetos corpus -----------
# Los objetos corpus (de tm) tienen metadatos (fecha, títulos, idioma, etc.)

# Artículos de Reuters
data("acq")
acq
acq[[1]]

# la función tidy incluye los metadatos como variables
acq_td <- tidy(acq)
acq_td

acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# most common words
acq_tokens %>%
  count(word, sort = TRUE)

acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))
