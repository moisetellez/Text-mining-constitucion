###### Proyecto text mining constitucion chilena

#Creador: Moises Tellez
#Fecha: 2022-02-20

#Librerias
{
library(data.table)
library(pdftools)
library(dplyr)
library(tidyr)
library(stopwords)
library(tidytext)
library(stringr)
library(ggplot2)
library(wordcloud) #Para generar el wordcloud
library(RColorBrewer) #Para los colores del wordcloud
library(ggraph)
library(igraph)
}




#abrir texto a analizar (separara el texto por paginas)
texto <-pdf_text("constitucion_politica.pdf")


#Juntamos todas las paginas en un solo gran texto
texto<-paste(texto, collapse = '')
length(texto)


#Separando el texto en frases (separacion por punto)
vector = c()
for(i in 1:length(texto)){
  temp<-(strsplit(texto[[i]], "\\.")[[1]])
  print(temp)
  vector <- c(vector, temp)
}


#Pasando el texto a formato table (tibble)
texto<-tibble(vector)





################ PRIMER ANALISIS: TOKEN DE PALABRAS ################

#Pasando el texto a tokens (palabras)
texto2 <- texto %>% 
  unnest_tokens(word,vector,drop = FALSE) #el primer "word" indica que la separaci?n ser? a nivel palabra

texto2$vector<- as.character(texto2$vector)


#Nos creamos un lexicon de stopwords en espa?ol 
lexiconSW<-stopwords("es")
lexiconSW <- append(lexiconSW,c("1","2","3","4","5","n","19","art?culo","45","7","letra","12","6","24","113")) #con append se pueden agregar stopwords
lexiconSW<- tibble(lexiconSW)
lexiconSW$lexiconSW <-as.character(lexiconSW$lexiconSW)


#sacando las stopwords al texto
texto3<- anti_join(texto2,lexiconSW, by= c("word"="lexiconSW"))


#Generando tabla de fecuencias de palabras
frec_texto<-texto3 %>% 
  group_by(word) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

frec_texto %>% 
  top_n(20,n) %>%   ##Se selecciona el top, usando la variable n
  mutate(word = reorder(word, n)) %>% #Ordena de mayor a menor las palabras
  ggplot(aes(x= n,y=word)) +
  geom_col()+
  labs(y = NULL)+
  ggtitle("Palabras m?s usadas - Constituci?n chilena")


#Generamos el wordcloud
wordcloud(words = frec_texto$word, freq = frec_texto$n,
          max.words = 400, random.order = FALSE, rot.per = 0.35,
          colors = brewer.pal(8, "Dark2"))



############ Bigramas #################
#Es la relacion de dos palabras (Ej: Don-Quijote, Sancho-Panza)


review_bigrams <- texto %>%  ##Usamos la tabla sin eliminar por stop words
  unnest_tokens(bigram, vector, token = "ngrams", n = 2) %>%  # ahora, separamos cada 2 token
  separate(bigram, c("word1", "word2"), sep = " ") %>%  # separamos word por bigrama
  filter(!word1 %in% lexiconSW$lexiconSW) %>% 
  filter(!word2 %in% lexiconSW$lexiconSW) %>% 
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>%
  mutate(frase = paste(word1,word2,sep=" " )) %>% 
  group_by(word1,word2) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))


# Generando red semantica

set.seed(175)
review_bigrams %>%
  filter(n >= 12) %>%   #Filtrar hasta que frecuencia se quiere ver en el mapa
  graph_from_data_frame() %>%  ###Desde aqui en adelante, no tocar nada
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Red semantica - constituci?n chilena')


#Grafico barras
review_bigrams %>% 
  ungroup() %>% 
  top_n(20,n) %>%   ##Se selecciona el top, usando la variable n
  mutate(frase = paste(word1,word2,sep=" " )) %>% 
  select(frase,n) %>% 
  mutate(frase = reorder(frase, n)) %>% #Ordena de mayor a menor las palabras
  ggplot(aes(x= n,y=frase)) +
  geom_col()+
  labs(y = NULL)+
  ggtitle("An?lisis por pares de palabras")








