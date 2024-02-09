library(udpipe)
library(tidyverse)

udmodel <- udpipe_download_model(language = "english")

x <- udpipe(x = "Competitive intelligence (CI) is the process and forward-looking practices used in producing 
            knowledge about the competitive environment to improve organizational performance.", object = udmodel)
#Il token id si basa sulla frase (sentence)
# Un token è un unita di significato, infatti un punto vale quanto un termine qualunque
list_of_files <- list.files(path = "AI_patents_2020_2021_claim/",
                            pattern = "\\.txt$",
                            full.names = TRUE)
n_patents <- 100
patents <-tibble(id = rep("", n_patents),
                 title = rep("", n_patents),
                 abstract = rep("", n_patents))

for(i in 1 : n_patents){
  raw_text <- read_file(list_of_files[[i]])
  patents[[i,1]] <- str_remove_all(
    string = list_of_files[[i]],
    pattern = "AI_patents_2020_2021_claim//|.txt"
  )
  patents[[i, 2]] <- str_extract(
    string = raw_text,
    pattern = "<title>\n(.*?)\n</title>") %>%
    str_remove_all("<title>\n|\n</title>")
  patents[[i,3]] <- str_extract(
    string = raw_text, 
    pattern = "<abstract>\n(.*?)\n</abstract>") %>% 
    str_remove_all("<abstract>\n|\n</abstract>")
}

udmodel <- udpipe_load_model(udmodel$file)
patents_tagged <- udpipe_annotate(udmodel, x = patents$abstract, doc_id = patents$id) %>%
  as_tibble()

patents_tagged %>%
  group_by(lemma) %>%
  count() %>%
  arrange(n) %>%
  View()

patents_tagged %>%
  count(upos) %>%
  ggplot(aes(x = reorder(upos, n), y = n)) +
  geom_bar(stat="identity") +
  coord_flip() 

to_search <- c('device', 'data')

patents_tagged %>%
  filter(lemma %in% to_search) %>%
  count(lemma) %>%
  ggplot(aes(x = reorder(lemma, n), y = n)) +
  geom_bar(stat="identity") +
  coord_flip() 
