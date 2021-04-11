## Visual Assignment - Atticus ##


## Examples Used 

#1 Data Explorer for EDA 
#2 Data Cleaning - Text Preprocessing 
#3 LDA with Text2Vec 
#4 D3PartitionR Example 
#5 Cora Explorer Example
#6 Sentiment Analysis for All Media
#7 Stock Market Example with Dygraph
#8 Stock Market Example with MLR
#9 Word Network
#10 N-Gram Word Plot 


## Load Packages ## 
packages <- c("tidyverse","tidytext","dplyr","ggplot2","broom",
              "stringr","LDAvis","textclean","textmineR","stopwords", 
              "text2vec", "tm", "gplots", "corrplot", 
              "stringr", "textclean", "RColorBrewer", "wordcloud",
              "arulesSequences", "ggtext", "tidygraph","stringr","text2vec",
              "igraph", "textplot", "udpipe", "ggraph", "BTM", "concaveman", 
              "quanteda", "corporaexplorer", "stm", 
              "D3partitionR", "data.table", "magrittr", "highcharter",
              "syuzhet", "viridis", "tibble", "purrr", "tidyr", "readr", "glue", 
              "GGally", "reshape2", "gridExtra", "plotly", "dygraphs",
              'ggraph', 'igraph', "xts", "zoo", "purrrlyr")



for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

## Load Data ## 

# READ DATA # 
test <- read_csv("./data/zerotennew.csv")
test$Date <- as.Date(test$Date, format = "%d/%m/%y")


##################################################################
##################################################################

## 1 - Data Explorer for EDA ## 

#CHECK YOUR DATA FOR MISSING VALUES USING DATA EXPLORER##
library(DataExplorer)
data(test)
plot_missing(test)
plot_histogram(test)

#Create Report 
create_report(test)
##This is a very useful package to do preliminary data exploration and provide an overall guide on where you want to focus first
## You can also use the Janitor package to clean your data but we will not do this as the data is clean##

##################################################################
##################################################################

## 2 - Data Cleaning - Text Pre-processing ##
#Text Pre-processing with StringR and Textclean
test$Cleaned<-tolower(test$Text)%>%#convert to lowercase
  replace_contraction() %>% #lengthening words (eg,isn't -> is not)
  replace_word_elongation() %>% #reducing informal writing (eg,heyyyyyyy -> hey)
  str_replace_all("[0-9]", " ") %>% #removing numbers
  str_replace_all("[[:punct:]]","")%>%#remove punctuation
  str_replace_all("covid|wuhan virus","coronavirus")%>%#word substitution
  str_squish()%>% #reduce repeated whitespace 
  str_trim#removes whitespace from start and end of string
#view(test)

##################################################################
##################################################################

## 3 - LDA with Text2Vec ## 

#Tokenize 
tokens = tolower(test$Cleaned)
tokens = word_tokenizer(tokens)
it = itoken(tokens, ids = test$docid, progressbar = FALSE)
v = create_vocabulary(it)
v = prune_vocabulary(v, term_count_min = 10, doc_proportion_max = 0.2)

vectorizer = vocab_vectorizer(v)
dtm2 = create_dtm(it, vectorizer, type = "dgTMatrix")

#LDA for 20 topics
lda_model = LDA$new(n_topics = 20, doc_topic_prior = 0.1, topic_word_prior = 0.01)
doc_topic_distr = 
  lda_model$fit_transform(x = dtm2, n_iter = 1000, 
                          convergence_tol = 0.001, n_check_convergence = 25, 
                          progressbar = FALSE)


barplot(doc_topic_distr[1, ], xlab = "topic", 
        ylab = "proportion", ylim = c(0, 1), 
        names.arg = 1:ncol(doc_topic_distr))

lda_model$get_top_words(n = 10, topic_number = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 19L, 20L), lambda = 0.4)

#Apply learned model to new data 
it = itoken(test$Cleaned[4001:5000], tolower, word_tokenizer, ids = test$docid[4001:5000])
new_dtm =  create_dtm(it, vectorizer, type = "dgTMatrix")
new_doc_topic_distr = lda_model$transform(new_dtm)

#Test perplexity the lower the score the better
perplexity(new_dtm, topic_word_distribution = lda_model$topic_word_distribution, doc_topic_distribution = new_doc_topic_distr)

#Plot the graph 
lda_model$plot()
# Reference URL http://text2vec.org/topic_modeling.html

##################################################################
##################################################################

## 4 - D3PartitionR Example 
#Using Fread is faster
media_data <- fread("Data/zerotensmallerv1.csv")
#view(media_data)

#Creating attributes for plotting
var_names=c('Type', 'Source', 'Topic','Text', 'Total','Month','Comments')


#Preparing data 
data_plot=media_data[,.N,by=var_names]
data_plot[,(var_names):=lapply(var_names,function(x){data_plot[[x]]=paste0(x,' ',data_plot[[x]])
})]

#Plotting the chart normal
D3partitionR() %>%
  add_data(data_plot,count ='N',steps=c('Type', 'Source', 'Topic','Month','Type')) %>%
  add_title('Local Media Coverage in 2020') %>%
  plot()

D3partitionR() %>%
  add_data(data_plot,count ='N',steps=c('Type', 'Source', 'Topic','Month','Type'), tooltip=c('name','N'),label='name') %>%
  add_title('Local Media Coverage in 2020') %>%
  plot()

#closer Attempt
D3partitionR() %>%
  add_data(data_plot,count ='N',steps=c('Topic', 'Month', 'Source', 'Type'), tooltip=c('name','N'),label='name') %>%
  add_title('Local Media Coverage in 2020') %>%
  plot()


#d3 = D3partitionR() %>%
add_data(data_plot,count = 'N',steps=c('Type', 'Source', 'Topic', 'Month','Type')) %>%
  set_chart_type('treemap') %>%
  add_nodes_data(list('Month 1'=list('N' = 'Total'),
                      'Month 2'=list('N' = 'Total'),
                      'Month 3'=list('N' = 'Total'),
                      'Month 4'=list('N' = 'Total'),
                      'Month 5'=list('N' = 'Total'),
                      'Month 6'=list('N' = 'Total'),
                      'Month 7'=list('N' = 'Total'),
                      'Month 8'=list('N' = 'Total'),
                      'Month 9'=list('N' = 'Total'),
                      'Month 10'=list('N' = 'Total'),
                      'Month 11'=list('N' = 'Total'),
                      'Month 12'=list('N' = 'Total'),
  )
  )



#d3 %>%
set_legend_parameters(zoom_subset = TRUE) %>%
  set_chart_type('circle_treemap') %>%
  set_tooltip_parameters(visible=TRUE, style='background-color:lightblue;',builder='basic') %>% 
  plot()


#### Plotting the chart normal v2 ###############
# Merging steps data and data with ages
data_plot = merge(media_data[,.N, by = c(var_names)], media_data[,.(mean_total=mean(Total,na.rm =TRUE), likes=Likes), by=c(var_names)], by=var_names)

##Improving steps naming
data_plot[,(var_names):=lapply(var_names,function(x){data_plot[[x]]=paste0(x,' ',data_plot[[x]])
})]

D3partitionR()%>%
  add_data(data_plot,count = 'N',steps=c('Type', 'Source', 'Topic','Month'),tooltip=c('N', 'mean_total'),label='name', color='Total',aggregate_fun = list(mean_total=weighted.mean,likes=weighted.mean)) %>%
  plot()

D3partitionR()%>%
  add_data(data_plot,count = 'N',steps=c('Type', 'Source', 'Topic'),tooltip=c('name','N','mean_total'),label='name',color='likes',aggregate_fun = list(mean_total=weighted.mean,likes=weighted.mean)) %>%
  set_chart_type('treemap') %>%
  set_labels_parameters(cut_off=1) %>%
  plot()

###################

## Plotting the chart treemap
D3partitionR() %>%
  add_data(data_plot,count ='N',steps=c('Topic', 'Source', 'Month')) %>%
  add_title('Local Media Coverage in 2020') %>%
  set_chart_type('treemap') %>%
  plot()

##################################################################
##################################################################

## 5 - Cora Explorer Example
# PREPARING THE DATA #
corpus_dora <- prepare_data(
  test,
  date_based_corpus = TRUE,
  grouping_variable = NULL,
  within_group_identifier = NULL,
  columns_doc_info = c("Topic", "Text", "Link", "Source", "Total", "Likes", "Shares", "Comments"),
  corpus_name = "Local Media Coverage in 2020",
  use_matrix = TRUE,
  matrix_without_punctuation = TRUE,
  tile_length_range = c(1, 25),
  columns_for_ui_checkboxes = TRUE,
)

# RUN DATA WITH FORMAT # 
explore(
  corpus_dora,
  search_options = list(optional_info = TRUE),
  ui_options = list(font_size = "10px"),
  plot_options = list(max_docs_in_wall_view = 10000)
)


##################################################################
##################################################################

## 6 - Sentiment Analysis for All Media

#Re-run to ensure date is in the correct format
test$Date <- as.Date(test$Date, format = "%d/%m/%y")

#Unnest tokens for media words
media_words <- unnest_tokens(text, word, text)
#view(media_words) #can remove # to run to check

#Run sentiment analysis for all media (can use nrc, bing or afinns to compare results)
media_sentiment <- inner_join(media_words, get_sentiments("afinn"), by = "word")
#view(media_sentiment) #can remove # to run to check

#Daily media sentiment
daily_sentiment <- group_by(media_sentiment, date)%>%
  summarise(mean(value))

#View(daily_sentiment) #can remove # to run to check

#Plot graph of media sentiment by time 
p_smooth <- ggplot(daily_sentiment, aes(x = date, y = `mean(value)`)) +
  geom_smooth() +
  ylab("sentiment") +
  xlab("date") +
  geom_hline(yintercept=0, linetype="dashed") +
  scale_x_date(date_labels = "%b %d")
p_smooth

##################################################################
##################################################################

## 7 - Stock Market Example with Dygraph

#Unnest media tokens
media_words <- unnest_tokens(text, word, text)
#view(media_words)

#Apply Bing Sentiment Analysis for all media 
media_sentiment_c <- media_words %>%
  inner_join(get_sentiments("bing")) %>%
  count(date, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#Read SGX Data
teststock3 <- read_csv("Data/sgx.csv")
teststock3$Date <- as.Date(teststock3$Date, format = "%d/%m/%y")

teststock3 <- teststock3 %>%
  left_join(media_sentiment_c, by = c("Date" = "date"))     
view(teststock3)

#Will need to convert to xts format first before plotting Dygraph
Date=teststock3$Date <- as.Date(as.character(teststock3$Date,"%Y-%m-%d"))
class(Date)
#Date=teststock3$Date <- as.xts(ts(start = c(2020-1-1), endc(2020teststock3$Date)
Open=teststock3$Open <- as.numeric(na.locf(teststock3$Open))
Close=teststock3$Close <- as.numeric(na.locf(teststock3$Close))
High=teststock3$High <- as.numeric(na.locf(teststock3$High))
Low=teststock3$Low <- as.numeric(na.locf(teststock3$Low))
Sentiment=teststock3$sentiment <- as.numeric(na.locf(teststock3$sentiment))
z=cbind(Open,Close,High, Low, Sentiment)
newdata=xts(z,Date)

#Plot Dygraph with events manually input
dygraph(newdata, main = "SGX VS Sentiment of Local News Headlines") %>%
  dyEvent("2020-01-23", "First COVID-19 Case in SG", labelLoc="bottom")%>%
  dyEvent("2020-01-29", "First Travel Restrictions to SG", labelLoc="bottom")%>%
  dyEvent("2020-03-27", "Circuit Breaker Phase 1 starts", labelLoc="bottom")%>%
  dyEvent("2020-05-19", "Circuit Breaker Phase 1 Ends", labelLoc="bottom")%>%
  dyEvent("2020-07-10", "Polling Day", labelLoc="bottom")%>%
  dyEvent("2020-08-09", "National Day", labelLoc="bottom")%>%
  dyEvent("2020-11-09", "Biden confirmed as Winner", labelLoc="bottom")%>%
  dyShading(from = "2020-03-27", to = "2020-5-19", color = "#FFE6E6") %>%
  dyHighlight(highlightCircleSize = 5, 
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = FALSE) %>%
  dyRangeSelector()


##################################################################
##################################################################

## 8 - Multiple Linear Regression Test for SGX and Sentiment

#Re-read SGX data
teststock2 <- read_csv("Data/sgx.csv")
teststock2$Date <- as.Date(teststock2$Date, format = "%d/%m/%y")


#Join Media sentiment and SGX data
teststock2 <- teststock2 %>%
  left_join(media_sentiment_c, by = c("Date" = "date"))     
#view(teststock2)

#Format as numeric for comparison
teststock2$Close <- as.numeric(gsub("\\.", "", teststock2$Close)) 
teststock2$Volume <- as.numeric(gsub("\\.", "", teststock2$Volume))
teststock2$Open <- as.numeric(gsub("\\.", "", teststock2$Open))
teststock2$High <- as.numeric(gsub("\\.", "", teststock2$High))
teststock2$Low <- as.numeric(gsub("\\.", "", teststock2$Low))

#Replace neutral sentiment 
teststock2[is.na(teststock2)] <- 0
colnames(teststock2) <- gsub("\\.", "_", colnames(teststock2))

#Build model
allCols <- colnames(teststock2)
regCols <- allCols[!allCols %in% c("AdjClose")]

#Create regression formula 
regCols <- paste(regCols, collapse = "+")
regCols <- paste("Close~",regCols, collapse = "+")
regCols <- as.formula(regCols)

#View MLR results
teststock2.lm <- lm(regCols, data = teststock2)
summary(teststock2.lm)


##################################################################
##################################################################

## 9 - Word Network

#Create bigrams
media_bigrams <- text %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)
media_bigrams

media_bigrams %>%
  count(bigram, sort = TRUE)

#Separate bigrams

bigrams_separated <- media_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#New bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
bigram_counts

#Trigrams

text %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)

#analyze
bigrams_filtered %>%
  filter(word2 == "covid") %>%
  count(source, word1, sort = TRUE)
view(bigrams_filtered)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

#tfidf
bigram_tf_idf <- bigrams_united %>%
  count(source, bigram) %>%
  bind_tf_idf(bigram, source, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf


#graph 
bigram_counts
bigram_graph <- bigram_counts %>%
  filter(n > 5) %>%
  graph_from_data_frame()
bigram_graph

set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


##################################################################
##################################################################

## 10 - N-Gram Word Plot 

n_word <- 20
n_top <- 150
n_gramming <- 3


trigrams <- tibble(text = test$Text) %>%
  unnest_tokens(trigram, text, token = "ngrams", n = n_gramming)

start_words <- c("grab", "gojek")

#
pattern <- str_c("^", start_words, " ", collapse = "|")
top_words <- trigrams %>%
  filter(str_detect(trigram, pattern)) %>%
  count(trigram, sort = TRUE) %>%
  slice(seq_len(n_top)) %>%
  pull(trigram)

trigrams <- trigrams %>%
  filter(trigram %in% top_words)

#nodes
str_nth_word <- function(x, n, sep = " ") {
  str_split(x, pattern = " ") %>%
    map_chr(~ .x[n])
}

#nodes

nodes <- map_df(seq_len(n_gramming),
                ~ trigrams %>%
                  mutate(word = str_nth_word(trigram, .x)) %>%
                  count(word, sort = TRUE) %>%
                  slice(seq_len(n_word)) %>% 
                  mutate(y = seq(from = n_word + 1, to = 0, 
                                 length.out = n() + 2)[seq_len(n()) + 1],
                         x = .x))

nodes %>% 
  ggplot(aes(x, y, label = word)) +
  geom_text()

sigmoid <- function(x_from, x_to, y_from, y_to, scale = 5, n = 100) {
  x <- seq(-scale, scale, length = n)
  y <- exp(x) / (exp(x) + 1)
  tibble(x = (x + scale) / (scale * 2) * (x_to - x_from) + x_from,
         y = y * (y_to - y_from) + y_from)
}

egde_lines <- function(trigram, from_word, to_word, scale = 5, n = 50, 
                       x_space = 0) {
  
  from_word <- from_word %>%
    select(-n) %>%
    set_names(c("from", "y_from", "x_from"))
  
  to_word <- to_word %>%
    select(-n) %>%
    set_names(c("to", "y_to", "x_to"))
  
  links <- crossing(from = from_word$from, 
                    to = to_word$to) %>%
    mutate(word_pair = paste(from, to),
           number = map_dbl(word_pair, 
                            ~ sum(str_detect(trigram$trigram, .x)))) %>%
    left_join(from_word, by = "from") %>%
    left_join(to_word, by = "to")
  
  links %>%
    by_row(~ sigmoid(x_from = .x$x_from + 0.2 + x_space,
                     x_to = .x$x_to - 0.05, 
                     y_from = .x$y_from, y_to = .x$y_to, 
                     scale = scale, n = n) %>%
             mutate(word_pair = .x$word_pair,
                    number = .x$number,
                    from = .x$from)) %>%
    pull(.out) %>%
    bind_rows()
}

egde_lines(trigram = trigrams, 
           from_word = filter(nodes, x == 1), 
           to_word = filter(nodes, x == 2)) %>%
  filter(number > 0) %>%
  ggplot(aes(x, y, group = word_pair, alpha = number, color = from)) +
  geom_line()

# egdes between first and second column
egde1 <- egde_lines(trigram = trigrams, 
                    from_word = filter(nodes, x == 1), 
                    to_word = filter(nodes, x == 2), 
                    n = 50) %>%
  filter(number > 0) %>%
  mutate(id = word_pair)

# Words in second colunm
## That start with he
second_word_he <- nodes %>%
  filter(x == 2) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      filter(str_nth_word(trigram, 1) == start_words[1]) %>%
      mutate(word = str_nth_word(trigram, 2)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

## That start with she
second_word_she <- nodes %>%
  filter(x == 2) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      filter(str_nth_word(trigram, 1) == start_words[2]) %>%
      mutate(word = str_nth_word(trigram, 2)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

# Words in third colunm
## That start with he
third_word_he <- nodes %>%
  filter(x == 3) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      filter(str_nth_word(trigram, 1) == start_words[1]) %>%
      mutate(word = str_nth_word(trigram, 3)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

## That start with she
third_word_she <- nodes %>%
  filter(x == 3) %>%
  select(-n) %>%
  left_join(
    trigrams %>% 
      filter(str_nth_word(trigram, 1) == start_words[2]) %>%
      mutate(word = str_nth_word(trigram, 3)) %>%
      count(word), 
    by = "word"
  ) %>%
  replace_na(list(n = 0))

# egdes between second and third column that starts with he
egde2_he <- egde_lines(filter(trigrams, 
                              str_detect(trigram, paste0("^", start_words[1], " "))), 
                       second_word_he, third_word_he, n = 50) %>%
  mutate(y = y + 0.05,
         from = start_words[1],
         id = str_c(from, word_pair, sep = " ")) %>%
  filter(number > 0)

# egdes between second and third column that starts with she
egde2_she <- egde_lines(filter(trigrams, 
                               str_detect(trigram, paste0("^", start_words[2], " "))), 
                        second_word_she, third_word_she, n = 50) %>%
  mutate(y = y - 0.05,
         from = start_words[2],
         id = str_c(from, word_pair, sep = " ")) %>%
  filter(number > 0)

# All edges
edges <- bind_rows(egde1, egde2_he, egde2_she)

p <- nodes %>% 
  ggplot(aes(x, y, label = word, size = n)) +
  geom_text(hjust = 0, color = "#DDDDDD") +
  theme_void() +
  geom_line(data = edges,
            aes(x, y, group = id, color = from, alpha = sqrt(number)),
            inherit.aes = FALSE) +
  theme(plot.background = element_rect(fill = "#666666", colour = 'black'),
        text = element_text(color = "#EEEEEE", size = 15)) +
  guides(alpha = "none", color = "none", size = "none") +
  xlim(c(0.9, 3.2)) +
  scale_color_manual(values = c("#5EF1F1", "#FA62D0")) +
  labs(title = " Vizualizing trigrams in Local Media Coverage") + 
  scale_size(range = c(3, 8))
p





