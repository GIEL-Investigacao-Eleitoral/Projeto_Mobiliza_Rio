

library(readxl) 
library(janitor)
library(tidytext)
library(dplyr)
MOBILIZA <- read_excel("C:/Users/Hp/Desktop/Borba Mobiliza Rio/dados/TRANSCRIÇÃO PROJETO MOBILIZA.xlsx")
head(MOBILIZA)

# trocando o nome
nomes<-c("sent_id" ,"text",'cat')
colnames(MOBILIZA)<-nomes
names(MOBILIZA)

MOBILIZAtidy<-MOBILIZA %>%
    unnest_tokens(word,text)

MOBILIZAtidy %>%
  count(word,sort = TRUE)

# tirando as palavras comuns (o, a, que, de, um, uma, etc) - palavras banidas
pt_stop<-get_stopwords(language = "pt")
palavras_extras<- data.frame(word = c("é","á","tambem","assim","ha","ainda","outra",'então','porque',
                                      'além','pra'),lexicon=rep("customizado",11),stringsAsFactors = FALSE)
# juntando as minhas palavras com a lista de palavras banidas
palavra_onibus<-pt_stop %>% bind_rows(palavras_extras)

# lista de palavras utilizadas por Machado de Assis (lista de palavras automaticas banidas)
MOBILIZAtidy %>%
  anti_join(get_stopwords(language = "pt")) %>%
  anti_join(palavra_onibus) %>%
  count(word,sort = TRUE)


library(ggplot2)
library(forcats)
MOBILIZAtidy %>%
  anti_join(palavra_onibus) %>%
  count(word,sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(fct_reorder(word,n),n)) +
  geom_col(fill="royalblue")+
  coord_flip()+
  xlab("Palavra")+
  ylab("Quantidade")



library(wordcloud)
library(ggwordcloud)


wordcloud_df <-MOBILIZAtidy %>%
  anti_join(get_stopwords(language = "pt")) %>%
  anti_join(palavra_onibus) %>%
  count(word,sort = TRUE) %>% 
  top_n(300)

wordcloud_df %>%
  ggplot(aes(color = 'blue')) + 
  geom_text_wordcloud_area(aes(label = word, size = n,color = 'blue')) +
  scale_size_area(max_size = 15)+
  theme_minimal()


# que palvras ficam juntas (correlacao de palavras)
tidy_ngram<- MOBILIZA %>%
  unnest_tokens(bigram,text, token= "ngrams",n=2)

library(tidyr)
tidy_ngram %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% palavra_onibus$word,
         !word2 %in% palavra_onibus$word) %>%
  count(word1, word2, sort = TRUE)



par(bg="black")
wordcloud(wordcloud_df$word,wordcloud_df$n, max.words=300,colors=c("white","#eaef88","#e1e85a","#e1e85a"))

theme_get()$plot.margin

par(mar = rep(0, 4))

wordcloud(wordcloud_df$word,wordcloud_df$n, max.words=Inf,colors=c("royalblue"))+
  theme_minimal()+
  theme(plot.margin = margin(0,0,0,0, "cm"))

# Cluster



#----------------------------------------------------------------
# criando uma identificacao unica (id) para cada tweet
#----------------------------------------------------------------
MOBILIZA$id <-1:dim(MOBILIZA)[1]

library(textmineR)
#----------------------------------------------------------------
# criando o document term matrix 
#----------------------------------------------------------------
dtm <- CreateDtm(doc_vec = MOBILIZA$text, # character vector of documents
                 doc_names = MOBILIZA$id, # document names
                 ngram_window = c(1, 2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("pt"), # stopwords from tm
                                  stopwords::stopwords(source = "smart")), # this is the default value
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = FALSE, # Turn off status bar for this demo
                 cpus = 2) # default is all available cpus on the system

# construct the matrix of term counts to get the IDF vector
tf_lab <- TermDocFreq(dtm)


head(tf_lab)
# TF-IDF 
tfidf <- t(dtm[ , tf_lab$term ]) * tf_lab$idf
tfidf <- t(tfidf)

# similaridade por cosseno
csim <- tfidf / sqrt(rowSums(tfidf * tfidf))

# %*% is matrix multiplication
csim <- csim %*% t(csim)

cdist <- as.dist(1 - csim)

hc <- hclust(cdist, method="ward.D")

clustering <- cutree(hc, 6)
par(bg="white")

plot(hc, main = "agrupamento hierárquico dos \nrelatos do grupo de foco",
     ylab = "", xlab = "", yaxt = "n")

rect.hclust(hc, 6, border = "red")

p_words <- colSums(dtm) / sum(dtm)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- dtm[ clustering == x , ]
  
  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]
  
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})


# create a summary table of the top 5 words defining each cluster
cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)

cluster_summary
kable(cluster_summary)


