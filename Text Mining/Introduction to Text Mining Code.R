## Text Mining Using R ##

library(tidyverse)
library(schrute)

data('theoffice')

## Check out the data structure ##

theoffice |>
  select(text) |>
  head(5)

## Understanding Tokenization ##

library(tidytext)

text_string <- tibble(text="Identity theft is not a joke, Jim!")

text_string |>
  unnest_tokens(word,text)

## Tokenize Season 7 Dialogue ##

s7 <- theoffice |>
  filter(season == 7) |>
  select(text) |>
  unnest_tokens(word,text)

s7 |>
  head()

## Okay let's remove the stop words with an anti join ##

library(stopwords)

data("stop_words")
data("data_stopwords_smart")
data("data_stopwords_snowball")
data("data_stopwords_stopwordsiso")

## Join all into one big dataset ##

stoppr <- tibble(word = c(stop_words$word,
                          data_stopwords_smart$en,
                          data_stopwords_snowball$en,
                          data_stopwords_stopwordsiso$en)
) |>
  distinct()

## Remove Stop Words ##

s7_stop <- s7 |>
  anti_join(stoppr)

s7_stop |>
  head()

## Word Stemming ##

library(SnowballC)

s7_stop <- s7_stop |>
  mutate(word2 = wordStem(word))

s7_stop |>
  head()

## Analyzing Frequencies ##

s7_stop |>
  count(word,sort=T) |>
  head()

## Create a Horizontal, Ordered Bar Chart of Top 10 Words ##

s7_stop |>
  count(word,sort=T) |>
  head(10) |>
  ggplot(aes(x=n,y=reorder(word,n))) +
  geom_bar(stat='identity',fill='#EDEFE2',
           color='#2A3C5F') +
  geom_text(aes(label=n),hjust=1.15,
            color="#2A3C5F") +
  labs(x="Word Frequency",
       y="Word",
       title="The Office Season 7 Top Words") +
  theme_minimal()

## Create a word cloud ##

library(wordcloud)

s7_frequencies <- s7_stop |>
  count(word)

wordcloud(words = s7_frequencies$word,
          freq = s7_frequencies$n,
          max.words = 50)

## Sentiment Analysis ##

## Examine the Bing dictionary ##

get_sentiments('bing')

## We want to inner join s7_stop with the Bing
## dictionary so we have only words which have
## matches in both datasets ##

sentiment <- s7_stop |>
  select(word) |>
  inner_join(get_sentiments('bing'))

sentiment |>
  head()

## Build Horizontal Ordered Boxplots using Facet Wrap! ##

sentiment |>
  group_by(sentiment) |>
  count(word,sentiment,sort=T) |>
  ungroup() |>
  group_by(sentiment) |>
  slice_max(n,n=10) |>
  ggplot(aes(x=n,y=reorder(word,n))) +
  geom_bar(stat='identity',fill='#EDEFE2',
           color='#2A3C5F') +
  geom_text(aes(label=n),hjust=1.15,
            color="#2A3C5F") +
  facet_wrap(~sentiment,scales='free_y') +
  labs(x="Word Frequency",
       y="Word",
       title="The Office Season 7 Top Words by Sentiment") +
  theme_minimal()

## We can do the same thing for wordclouds! ##
  
library(reshape2)

sentiment |>
  count(word, sentiment, sort = TRUE) |>
  acast(word ~ sentiment, value.var = "n", fill = 0) |>
  comparison.cloud(colors = c("gray20", "pink"),
                   max.words = 100,
                   title.size=2)

## Now let's go through a basic analysis, but this time using
## one of my all time favorite books, The Prophet by Kahlil Gibran ##

library(gutenbergr)

theprophet <- gutenberg_download(58585)

theprophet |>
  head()

## Tokenize ##

theprophet2 <- theprophet |>
  unnest_tokens(word,text)

## Remove Stop Words ##

theprophet2 <- theprophet2 |>
  anti_join(stoppr)

## Analyze Frequencies ##

## Bar Chart ##

theprophet2 |>
  count(word,sort=T) |>
  head(10) |>
  ggplot(aes(x=n,y=reorder(word,n))) +
  geom_bar(stat='identity',fill='white',
           color='black') +
  geom_text(aes(label=n),hjust=1.15,
            color="black") +
  labs(x="Word Frequency",
       y="Word",
       title="The Prophet Top Words") +
  theme_minimal()

## Word Cloud ##

p_counts <- theprophet2 |>
  count(word)

wordcloud(words=p_counts$word,
          freq=p_counts$n,
          max.words=50)

## Sentiment Analysis ##

p_sentiment <- theprophet2 |>
  select(word) |>
  inner_join(get_sentiments('bing'))

p_sentiment |>
  group_by(sentiment) |>
  count(word,sentiment,sort=T) |>
  ungroup() |>
  group_by(sentiment) |>
  slice_max(n,n=10) |>
  ggplot(aes(x=n,y=reorder(word,n))) +
  geom_bar(stat='identity',fill='white',
           color='black') +
  geom_text(aes(label=n),hjust=1.15,
            color="black") +
  facet_wrap(~sentiment,scales='free_y') +
  labs(x="Word Frequency",
       y="Word",
       title="The Prophet Top Words by Sentiment") +
  theme_minimal()

p_sentiment |>
  count(word, sentiment, sort = TRUE) |>
  acast(word ~ sentiment, value.var = "n", fill = 0) |>
  comparison.cloud(colors = c("gray20", "pink"),
                   max.words = 100,
                   title.size=2)
