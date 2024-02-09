library(udpipe)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(highcharter)
library(reshape2)
#install.packages("reshape2")

patents = read_delim(
  "datasets/plant-protection-patents.csv", #"patents-test.csv"
  delim=",")

udmodel <- udpipe_download_model(language = "english")

udmodel <- udpipe_load_model(udmodel$file)
patents_tagged <- udpipe_annotate(udmodel, x = paste(patents$Title, " ", patents$Abstract), doc_id = patents$"#") %>%
  as_tibble()
patents_frame <- data.frame(patents_tagged)

count_plot <- function(field) {
  noun <- subset(patents_frame, upos %in% c(field)) 
  noun <- txt_freq(noun$token)
  noun$key <- factor(noun$key, levels = rev(noun$key))
  ggplot(data=head(noun, 20), aes(x=key, y=freq)) +
    geom_bar(stat="identity") +
    coord_flip()
}
#count_plot("NOUN")
#count_plot("ADJ")
#count_plot("VERB")


rake <- keywords_rake(patents_frame, "lemma", "doc_id", 
                      relevant = patents_frame$upos %in% c("NOUN", "ADJ"))
rake$key <- factor(rake$keyword, levels = rev(rake$keyword))
ggplot(data=head(subset(rake, freq > 3), 30), aes(x=key, y=rake)) +
  geom_bar(stat="identity") +
  coord_flip() +
  ggtitle("RAKE Analisys - Plant Protection")
# aerial vehicle, protection box, 

## Using a sequence of POS tags (noun phrases / verb phrases)
patents_frame$phrase_tag <- as_phrasemachine(patents_frame$upos, type = "upos")
phrases <- keywords_phrases(x = patents_frame$phrase_tag, term = patents_frame$token, 
                            pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                            is_regex = TRUE, detailed = FALSE)
phrases <- subset(phrases, ngram > 1 & freq > 3)
phrases$key <- factor(phrases$keyword, levels = rev(phrases$keyword))
ggplot(data=head(phrases, 20), aes(x=key, y=freq)) +
  geom_bar(stat="identity") +
  coord_flip() +
  ggtitle("NGrams Analisys - Plant Protection")
#Technologies: aerial vehicle, plant protection unmanned aerial vehicle, pesticide box, liquid medicine, water tank, control system

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
#lemma_over_years("drone")
#lemma_over_years("unmanned")
#lemma_over_years("protection")
#lemma_over_years("plant")

years = patents$`Publication Year`
ret <- data.frame(year = integer(), freq = integer())
year_patents_frame = patents_frame %>%
  mutate(year=strtoi(years[strtoi(doc_id)])) %>%
  filter(year == 2015)
View(year_patents_frame)







ngrams_over_years <- function(to_search) {
  ret <- data.frame(year = integer(), freq = integer())
  for (y in 2000:2022) {
    year = y
    year_patents_frame = patents_frame %>%
      mutate(year=strtoi(years[strtoi(doc_id)])) %>%
      filter(year == y)
    year_patents_frame$phrase_tag <- as_phrasemachine(year_patents_frame$upos, type = "upos")
    phrases <- keywords_phrases(x = year_patents_frame$phrase_tag, term = year_patents_frame$token, 
                                pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                                is_regex = TRUE, detailed = FALSE)
    
    phrases <- subset(phrases, ngram > 1 & freq > 3)
    phrases$key <- factor(phrases$keyword, levels = rev(phrases$keyword))
    
    phrases <- phrases %>%
      filter(key == to_search)
    x = phrases$freq
    print(x)
    ret <- ret %>% 
      add_row(year = year, freq = x)
  }
  ret
}

NOY_1 = ngrams_over_years("aerial vehicle")
NOY_2 = ngrams_over_years("plant protection unmanned aerial vehicle")
NOY_3 = ngrams_over_years("pesticide box")
NOY_4 = ngrams_over_years("liquid medicine")
NOY_5 = ngrams_over_years("water tank")
NOY_6 = ngrams_over_years("control system")


NOYs <- data.frame(year = 2010:2022) %>%
  merge(y=NOY_1, by="year", all.x=TRUE) %>%
  mutate(`aerial vehicle` = freq) %>%
  subset(select = -c(freq)) %>%
  
  #merge(y=NOY_2, by="year", all.x=TRUE) %>%
  #mutate(`plant protection\nunmanned aerial vehicle` = freq) %>%
  #subset(select = -c(freq)) %>%
  
  merge(y=NOY_3, by="year", all.x=TRUE) %>%
  mutate(`pesticide box` = freq) %>%
  subset(select = -c(freq)) %>%
  
  merge(y=NOY_4, by="year", all.x=TRUE) %>%
  mutate(`liquid medicine` = freq) %>%
  subset(select = -c(freq)) %>%
  
  merge(y=NOY_5, by="year", all.x=TRUE) %>%
  mutate(`water tank` = freq) %>%
  subset(select = -c(freq)) %>%
  
  merge(y=NOY_6, by="year", all.x=TRUE) %>%
  mutate(`control system` = freq) %>%
  subset(select = -c(freq)) %>%
  
  reshape2::melt(id.var = "year")


ggplot(NOYs, aes(x=year, y=log(value), color=variable)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Frequency")

patents_by_year = patents %>% 
  mutate(year = `Publication Year`) %>% 
  group_by(year) %>% 
  count() %>%
  filter(year >= 2010) %>%
  filter(year <= 2022)

ggplot(patents_by_year, aes(x=year, y=n)) +
  geom_line() +
  geom_point()
       
indipendent_values = NOYs %>%
  merge(y=patents_by_year, by="year", all.x=TRUE) %>%
  mutate(yearly_patents = n) %>%
  subset(select = -c(n)) %>%
  
  mutate(indipendent_val = value / yearly_patents)

ggplot(indipendent_values, aes(x=year, y=log(indipendent_val), color=variable)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Log Frequency", 
     title = "Occurrences of N-Grams regardless of sector performance") +
  xlim(2014, 2022)


geo_df = data.frame(Continents = c("Africa", "America", "Asia", "Europa", "Oceania"), Score = c(33, 18, 47, 43, 2)) %>%
  arrange(-Score)

ggplot(geo_df) +
  geom_bar(aes(x=Continents,y=Score, fill=Continents),stat="identity") +
  labs(title = "Continents score")
  
  
