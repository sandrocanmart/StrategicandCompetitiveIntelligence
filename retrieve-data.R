library(udpipe)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(highcharter)
#install.packages("viridis")

patents = read_delim(
    "patents-10000.csv", #"patents-test.csv"
    delim=",")

udmodel <- udpipe_download_model(language = "english")

#udmodel <- udpipe_load_model(udmodel$file)
#patents_tagged <- udpipe_annotate(udmodel, x = patents$Abstract, doc_id = patents$"#") %>%
#  as_tibble()
#patents_frame <- data.frame(patents_tagged)
#years = patents$`Publication Year`
#patents_frame = patents_frame %>%
#  mutate(year=strtoi(years[strtoi(doc_id)]))
#write.csv(patents_frame, "patents_frame.csv", row.names=FALSE)

#patents_frame = read_delim(
#  "patents-10000.csv", #"patents-test.csv"
#  delim=",")
patents_frame = read_delim(
  "datasets/patents_frame.csv", #"patents-test.csv"
  delim=",")
patents_frame %>%
  View()

count_plot <- function(field) {
  noun <- subset(patents_frame, upos %in% c(field)) 
  noun <- txt_freq(noun$token)
  noun$key <- factor(noun$key, levels = rev(noun$key))
  ggplot(data=head(noun, 20), aes(x=key, y=freq)) +
    geom_bar(stat="identity") +
    coord_flip()
}
count_plot("NOUN")
count_plot("ADJ")
count_plot("VERB")


rake <- keywords_rake(patents_frame, "lemma", "doc_id", 
                      relevant = patents_frame$upos %in% c("NOUN", "ADJ"))
rake$key <- factor(rake$keyword, levels = rev(rake$keyword))
ggplot(data=head(subset(rake, freq > 3), 30), aes(x=key, y=rake)) +
  geom_bar(stat="identity") +
  coord_flip()

# protection box
## Using a sequence of POS tags (noun phrases / verb phrases)
patents_frame$phrase_tag <- as_phrasemachine(patents_frame$upos, type = "upos")
phrases <- keywords_phrases(x = patents_frame$phrase_tag, term = patents_frame$token, 
                            pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                            is_regex = TRUE, detailed = FALSE)
phrases <- subset(phrases, ngram > 1 & freq > 3)
phrases$key <- factor(phrases$keyword, levels = rev(phrases$keyword))
ggplot(data=head(phrases, 20), aes(x=key, y=freq)) +
  geom_bar(stat="identity") +
  coord_flip()

lemma_over_years <- function(word) {
  years = patents$`Publication Year`
  patents_frame %>%
    filter(lemma==word) %>%
    mutate(year=strtoi(years[strtoi(doc_id)])) %>%
    filter(year > 2000) %>%
    group_by(year) %>%
    count() %>%
    ggplot(aes(x=year, y=n)) +
      geom_bar(stat="identity")
}
lemma_over_years("vehicle")
lemma_over_years("drone")
lemma_over_years("unmanned")
lemma_over_years("protection")
lemma_over_years("plant")

ngrams_over_years <- function(abc) {
  ret <- data.frame(year = integer(), freq = integer())
  for (y in 2000:2023) {
    year = y
    year_patents_frame = patents_frame %>%
      filter(year == y) %>%
      mutate(year=strtoi(years[strtoi(doc_id)]))
    
    year_patents_frame$phrase_tag <- as_phrasemachine(year_patents_frame$upos, type = "upos")
    phrases <- keywords_phrases(x = year_patents_frame$phrase_tag, term = year_patents_frame$token, 
                                pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                                is_regex = TRUE, detailed = FALSE)
    
    phrases <- subset(phrases, ngram > 1 & freq > 3)
    phrases$key <- factor(phrases$keyword, levels = rev(phrases$keyword))
    phrases <- phrases %>%
      filter(key == abc)
    x = phrases$freq
    print(x)
    ret <- ret %>% 
      add_row(year = year, freq = x)
  }
  ret %>%
    ggplot(aes(x=year, y=freq)) +
    geom_line()
  ret
}
NOY_plant_protection = ngrams_over_years("plant protection")
NOY_plant_protection %>%
  filter(year < 2023) %>%
  ggplot(aes(x=year, y=freq)) +
  geom_line()
