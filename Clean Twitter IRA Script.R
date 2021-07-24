#Loading in a bunch of Libraries
library(tidyverse)
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(tm) 
library(igraph)
library(ggraph)

#Loading the full IRA Tweet Data Set
IRA_Tweets <- read_csv("ira_tweets_csv_hashed.csv")

#Filtering out all of the tweets that are not in English
IRA_Tweets_en <- IRA_Tweets %>% 
  filter(tweet_language=="en")

#Saving the English Tweet Set
write_csv(IRA_Tweets_en,'IRA_Tweets_en.csv')

IRA_Tweets_en <- read_csv("IRA_Tweets_en.csv")


#Narrowing down IRA_Tweets_en variables
IRA_Tweets_en_Clean <- data.frame(
  IRA_Tweets_en$user_display_name,
  IRA_Tweets_en$follower_count,
  IRA_Tweets_en$following_count,
  IRA_Tweets_en$tweet_text,
  IRA_Tweets_en$tweet_time,
  IRA_Tweets_en$is_retweet, 
  IRA_Tweets_en$tweet_client_name,
  IRA_Tweets_en$hashtags,
  IRA_Tweets_en$urls)

#Removing http elements, RT, and @___ from words list
IRA_Tweets_en_Clean$stripped_text <- gsub("http\\S+","",  IRA_Tweets_en_Clean$IRA_Tweets_en.tweet_text)
IRA_Tweets_en_Clean$stripped_text <- gsub("RT ","",  IRA_Tweets_en_Clean$stripped_text)
IRA_Tweets_en_Clean$stripped_text <- gsub("@\\S+","", IRA_Tweets_en_Clean$stripped_text)

write_csv(IRA_Tweets_en_Clean,'IRA_Tweets_en_Clean.csv')

###Start of Actual Analysis###
IRA_Tweets_en_Clean <- read_csv("IRA_Tweets_en_Clean.csv")

#Creating a clean list of words for analysis
IRA_Tweets_Words <- IRA_Tweets_en_Clean %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(word, stripped_text)

#Removing stop words from list of words
data("stop_words")

Clean_IRA_Tweet_Words <- IRA_Tweets_Words %>%
  anti_join(stop_words)

#Making plot of Top 20 words. 
Clean_IRA_Tweet_Words %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col(fill = "royalblue4",
           colour = "grey100") +
  geom_text(aes(label = n), vjust = 1.8, size = 3) +
  xlab(NULL) +
  coord_flip() +
  labs(y = "Count",
       x = "Unique words",
       title = "Top 20 Words in IRA Tweets",
       subtitle = "Count of unique words")

#change scientific notation
options(scipen=6)

###Exploring Co-Occurence of Words###

#Re-run this...
IRA_Tweets_en_Clean$stripped_text <- gsub("http\\S+","",  IRA_Tweets_en_Clean$IRA_Tweets_en.tweet_text)
IRA_Tweets_en_Clean$stripped_text <- gsub("RT ","",  IRA_Tweets_en_Clean$stripped_text)
IRA_Tweets_en_Clean$stripped_text <- gsub("@\\S+","", IRA_Tweets_en_Clean$stripped_text)

#Installing and loading some more libraries
install.packages(devtools)
install_github("dgrtwo/widyr", force = TRUE)
library(devtools)
library(widyr)

#Identifying Paired Words
ira_tweets_paired_words <- IRA_Tweets_en_Clean %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(paired_words, stripped_text, token = "ngrams", n = 2)

ira_tweets_paired_words %>%
  count(paired_words, sort = TRUE)

#Loading another Library
library(tidyr)

#Separating the words into two columns
IRA_Tweets_Separated_Words <- ira_tweets_paired_words %>%
  separate(paired_words, c("word1", "word2"), sep = " ")

IRA_Tweets_Filtered <- IRA_Tweets_Separated_Words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

IRA_Words_Counts <- IRA_Tweets_Filtered %>%
  count(word1, word2, sort = TRUE)

#Loading in some more libraries
library(igraph)
library(ggraph)

#Word Network Plot
IRA_Words_Counts %>%
  filter(n >= 2000) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "dh") +
  geom_edge_link(color = "royalblue4", aes(edge_alpha = n, edge_width = n, colour = n)) +
  geom_node_point(color = "royalblue4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Paired Words",
       subtitle = "IRA Twitter Dataset",
       x = "", y = "")

#playing with graph
# IRA_Words_Counts %>%
#   filter(n >= 2000) %>%
#   graph_from_data_frame() %>%
#   ggraph(layout = 'kk') + 
#   geom_edge_density(aes(fill = n)) + 
#   geom_edge_link(aes(edge_alpha = n, edge_width = n, colour = n)) +
#   geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
#   labs(title = "Word Network: Paired Words",
#        subtitle = "IRA Twitter Dataset",
#        x = "", y = "")
#Re-run this...
IRA_Tweets_en_Clean$stripped_text <- gsub("http\\S+","",  IRA_Tweets_en_Clean$IRA_Tweets_en.tweet_text)
IRA_Tweets_en_Clean$stripped_text <- gsub("RT ","",  IRA_Tweets_en_Clean$stripped_text)
IRA_Tweets_en_Clean$stripped_text <- gsub("@\\S+","", IRA_Tweets_en_Clean$stripped_text)

#Network of 3 words
IRA_Tweets_Triad_Words <- IRA_Tweets_en_Clean %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(triad_words, stripped_text, token = "ngrams", n = 3)

IRA_Tweets_Triad_Words %>%
  count(triad_words, sort = TRUE)

IRA_Tweets_Separated_Triad_Words <- IRA_Tweets_Triad_Words %>%
  separate(triad_words, c("word1", "word2", "word3"), sep = " ")

IRA_Tweets_Filtered_3 <- IRA_Tweets_Separated_Triad_Words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

IRA_Triad_Words_Counts <- IRA_Tweets_Filtered_3 %>%
  count(word1, word2, word3, sort = TRUE)

IRA_Triad_Words_Counts %>%
  filter(n >= 500) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "dh") +
  geom_edge_link(color = "royalblue4", aes(edge_alpha = n, edge_width = n, colour = n)) +
  geom_node_point(color = "royalblue4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Three Word Analysis",
       subtitle = "IRA Twitter Dataset",
       x = "", y = "")


#Re-run this...
IRA_Tweets_en_Clean$stripped_text <- gsub("http\\S+","",  IRA_Tweets_en_Clean$IRA_Tweets_en.tweet_text)
IRA_Tweets_en_Clean$stripped_text <- gsub("RT ","",  IRA_Tweets_en_Clean$stripped_text)
IRA_Tweets_en_Clean$stripped_text <- gsub("@\\S+","", IRA_Tweets_en_Clean$stripped_text)

#Network of 5 words
IRA_Tweets_Five_Words <- IRA_Tweets_en_Clean %>%
  dplyr::select(stripped_text) %>%
  unnest_tokens(five_words, stripped_text, token = "ngrams", n = 5)

IRA_Tweets_Five_Words %>%
  count(five_words, sort = TRUE)

IRA_Tweets_Separated_Five_Words <- IRA_Tweets_Five_Words %>%
  separate(five_words, c("word1", "word2", "word3", "word4", "word5"), sep = " ")

IRA_Tweets_Filtered_5 <- IRA_Tweets_Separated_Five_Words %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word) %>%
  filter(!word5 %in% stop_words$word)

IRA_Five_Words_Counts <- IRA_Tweets_Filtered_5 %>%
  count(word1, word2, word3, word4, word5, sort = TRUE)

IRA_Five_Words_Counts %>%
  filter(n >= 100) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "dh") +
  geom_edge_link(color = "royalblue4", aes(edge_alpha = n, edge_width = n, colour = n)) +
  geom_node_point(color = "royalblue4", size = 3) +
  geom_node_text(aes(label = name), vjust = 1.8, size = 3) +
  labs(title = "Word Network: Five Word Analysis",
       subtitle = "IRA Twitter Dataset",
       x = "", y = "")


### Beginning Topic Modelling Analysis of Key Topics ###

#Just look at major concepts here... will definitely need to go back and fine tune this code/ clean it up/ 
  #go even more in depth with analysis... hashtags, links, etc. 


## List of Topics and Words by Level ##
    # One Word Analysis
        # News
        # Trump
        # Sports
        # Politics
        # People
        # Love
        # Police
        # World
        # Local
        # Life
        # Time
        # Obama
        # Day
        # Workout
        # Black
    
    # Two Word Analysis
        # Chicago World News
        # 24x7 Online Payday Loans
        # AmericaFirst MakeAmeericaGreatAgain
        # Black Lives
        # Sports Local
    
    # Three Word Analysis:
        # News World Local TopNews Breaking Donald
        # AmericaFirst MakeAmeericaGreatAgain
        # 24x7 Online Payday Loans 
        # Black Lives
        # Isis Targeted Accounts IceIsis
        # Goveernment Doesnt Care Nuclear Power Ukranian
    
    # Five Word Analysis:
        # 24x7 Online Payday Loans
        # Ukranian Government
        # Targeted Isis Accounts
        # Maga War WakeUpUSA DeepState ShadowGovernment Obamagate
        # MakeAmericaGreatAgain AmericaFirst

# Loading in Libraries
install.packages("topicmodels")
install.packages("qdap")

library(tm)
library(tidyverse)
library(tidytext)
library(topicmodels)
library(qdap)



###########################################################

#
# hc <- IRA[str_detect(IRA$IRA_Tweets_en.tweet_text,"@HillaryClinton"),]
# 
# IRA %>% group_by(IRA_Tweets_en.tweet_text) %>% summarize(count=n()) %>% arrange(desc(count)) %>% head()

# Pulling out 500 to play with... 
#corp.df <- cbind(as.character(1:500),IRA[base::sample(1:nrow(IRA),500),"IRA_Tweets_en.tweet_text"])
# names(corp.df) <- c("doc_id","text")
# 
# rm(IRA_1000)
# gc()
# 
# write_csv(corp.df,"sample.csv")

#Reloading Dataset
IRA_Tweets_en_Clean <- read_csv("IRA_Tweets_en_Clean.csv")

#Re-run this...
IRA_Tweets_en_Clean$stripped_text <- gsub("http\\S+","",  IRA_Tweets_en_Clean$IRA_Tweets_en.tweet_text)
IRA_Tweets_en_Clean$stripped_text <- gsub("RT ","",  IRA_Tweets_en_Clean$stripped_text)
IRA_Tweets_en_Clean$stripped_text <- gsub("@\\S+","", IRA_Tweets_en_Clean$stripped_text)


# #Filtering out so that the five word topics are represented
# Filter_Five <- str_detect(str_to_lower(IRA_Tweets_en_Clean$stripped_text), paste(c("ukranian","government"), collapse = "|"))
# Filter_Five <- str_detect(str_to_lower(IRA_Tweets_en_Clean$stripped_text), paste(c("targeted", "isis", "accounts"), collapse = "|"))
# Filter_Five <- str_detect(str_to_lower(IRA_Tweets_en_Clean$stripped_text), paste(c("maga", "war", "wakeupusa", "deepstate", "shadowgovernment",
#                                                                                    "obamagate"), collapse = "|"))
# Filter_Five <- str_detect(str_to_lower(IRA_Tweets_en_Clean$stripped_text), paste(c("makeamericagreatagain","americafirst"), collapse = "|"))

#Filtering based on main topics
Filter_BlackLives <- str_detect(str_to_lower(IRA_Tweets_en_Clean$stripped_text), paste(c("black", "lives"), collapse = "|"))
Filter_Obama <- str_detect(str_to_lower(IRA_Tweets_en_Clean$stripped_text), paste(c("obamagate", "obama"), collapse = "|"))
Filter_MakeAmericaGreat <- str_detect(str_to_lower(IRA_Tweets_en_Clean$stripped_text), paste(c("makeamericagreatagain","americafirst", "maga"), collapse = "|"))
Filter_Trump <- str_detect(str_to_lower(IRA_Tweets_en_Clean$stripped_text), paste(c("trump", "donald"), collapse = "|"))
Filter_DeepState <- str_detect(str_to_lower(IRA_Tweets_en_Clean$stripped_text), paste(c("wakeupusa", "deepstate", "shadowgovernment","obamagate"), collapse = "|"))
Filter_Hillary <- str_detect(str_to_lower(IRA_Tweets_en_Clean$stripped_text), paste(c("hillary", "clinton"), collapse = "|"))

# Setting Stripped Text as Corpus
data("stop_words")
c <- Corpus(VectorSource(IRA_Tweets_en_Clean$stripped_text[Filter_BlackLives]))

dtm <- DocumentTermMatrix(c,control = list(removePunctuation = TRUE,
                                           stopwords = TRUE,
                                           stemming = TRUE,
                                           wordLengths=c(0,Inf)))

# #Checking the dtm 
# inspect(dtm)
# 
# #Filtering out rows that are zero and making a new corpus
# clean.corp <- IRA_Tweets_en_Clean$stripped_text[!(which(rowSums(as.matrix(dtm))==0))]
# c <- Corpus(VectorSource(clean.corp))

#Ten Topic LDA on dtm
base_lda <- LDA(dtm, k=5, method = "VEM", control = NULL, model = NULL)

topics_beta <- tidy(base_lda, matrix = "beta")

top_terms <- topics_beta %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

as.data.frame(top_terms)

lda_gamma <- tidy(base_lda, matrix = "gamma")
tweet_class <- lda_gamma %>%  group_by(document) %>%
  top_n(1, gamma) %>% arrange(as.numeric(document)) %>% ungroup()

corp.df <- data.frame("text"=IRA_Tweets_en_Clean$stripped_text[Filter_BlackLives])
corp.df$doc_id <- as.character(1:nrow(corp.df))

tweet_class_text_BL <- merge(tweet_class,corp.df,by.x="document",by.y="doc_id") %>% arrange(as.numeric(document))

View(tweet_class_text_BL %>% arrange(desc(gamma)))

table(tweet_class_text$topic)

