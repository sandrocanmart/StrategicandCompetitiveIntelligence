library(udpipe)
library(tidyverse)
library(dplyr)
library(ggplot2)


patents = read_delim(
  "RCode/patents-test.csv",
  delim = ",")


udmodel <- udpipe_download_model(language = "english")

udmodel <- udpipe_load_model(udmodel$file)
patents_tagged <- udpipe_annotate(udmodel, x = patents$Abstract, doc_id = patents$"#") %>%
  as_tibble()
patents_frame <- data.frame(patents_tagged)
years <- patents$`Publication Year`

count_plot <- function(field) {
  noun <- subset(patents_frame, upos %in% c(field))
  noun <- txt_freq(noun$token)
  noun$key <- factor(noun$key, levels = rev(noun$key))
  ggplot(data = head(noun, 20), aes(x = key, y = freq)) +
    geom_bar(stat = "identity") +
    coord_flip()
}

count_plot("NOUN")
count_plot("ADJ")
count_plot("VERB")


rake <- keywords_rake(patents_frame, "lemma", "doc_id",
                      relevant = patents_frame$upos %in% c("NOUN", "ADJ"))
rake$key <- factor(rake$keyword, levels = rev(rake$keyword))
ggplot(data = head(subset(rake, freq > 3), 30), aes(x = key, y = rake)) +
  geom_bar(stat = "identity") +
  coord_flip()

## Using a sequence of POS tags (noun phrases / verb phrases)
patents_frame$phrase_tag <- as_phrasemachine(patents_frame$upos, type = "upos")
phrases <- keywords_phrases(x = patents_frame$phrase_tag, term = patents_frame$token,
                            pattern = "(A|N)*N(P+D*(A|N)*N)*",
                            is_regex = TRUE, detailed = FALSE)
phrases <- subset(phrases, ngram > 1 & freq > 3)
phrases$key <- factor(phrases$keyword, levels = rev(phrases$keyword))
ggplot(data = head(phrases, 20), aes(x = key, y = freq)) +
  geom_bar(stat = "identity") +
  coord_flip()

lemma_over_years <- function(word) {
  patents_frame %>%
    dplyr::filter(lemma == word) %>%
    dplyr::mutate(year = years[strtoi(doc_id)]) %>%
    dplyr::group_by(year) %>%
    dplyr::count() %>%
    ggplot(aes(x = year, y = n)) +
    geom_bar(stat = "identity")
}

lemma_over_years("vehicle")

patents_frame %>%
  View()
typeof(years)
years

patents_frame %>%
  dplyr::filter(lemma == "vehicle") %>%
  dplyr::mutate(year = years[strtoi(doc_id)]) %>%
  dplyr::group_by(year) %>%
  dplyr::count() %>%
  View()