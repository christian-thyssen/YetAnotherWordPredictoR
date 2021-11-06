library(tidyverse)
library(quanteda.textstats)
library(quanteda.textplots)

# Words

dfmat <- dfm(toks)
dfmat <- dfm_sort(dfmat)

# Top 10 Features

topfeatures(dfmat, 10)

dfmat %>%
    textstat_frequency(n = 10) %>%
    ggplot(aes(x = frequency, y = reorder(feature, frequency))) +
    geom_point() +
    labs(x = "Frequency", y = NULL)

textplot_wordcloud(dfmat, max_words = 10)

# 0 %, 10 %, 50 %, 90 %, 100 % coverage

0
sum(cumsum(colSums(dfmat) / sum(dfmat)) < .1) + 1
sum(cumsum(colSums(dfmat) / sum(dfmat)) < .5) + 1
sum(cumsum(colSums(dfmat) / sum(dfmat)) < .9) + 1
ncol(dfmat)

## Bigrams

toks_bigrams <- tokens_ngrams(toks, n = 2)

dfmat_bigrams <- dfm(toks_bigrams)
dfmat_bigrams <- dfm_sort(dfmat_bigrams)

# Top 10 Features

topfeatures(dfmat_bigrams, 10)

dfmat_bigrams %>%
    textstat_frequency(n = 10) %>%
    ggplot(aes(x = frequency, y = reorder(feature, frequency))) +
    geom_point() +
    labs(x = "Frequency", y = NULL)

textplot_wordcloud(dfmat_bigrams, max_words = 10)

# 0 %, 10 %, 50 %, 90 %, 100 % coverage

0
sum(cumsum(colSums(dfmat_bigrams) / sum(dfmat_bigrams)) < .1) + 1
sum(cumsum(colSums(dfmat_bigrams) / sum(dfmat_bigrams)) < .5) + 1
sum(cumsum(colSums(dfmat_bigrams) / sum(dfmat_bigrams)) < .9) + 1
ncol(dfmat_bigrams)

## 3-grams

toks_trigrams <- tokens_ngrams(toks, n = 3)

dfmat_trigrams <- dfm(toks_trigrams)

# Top 10 Features

topfeatures(dfmat_trigrams, 10)

dfmat_trigrams %>%
    textstat_frequency(n = 10) %>%
    ggplot(aes(x = frequency, y = reorder(feature, frequency))) +
    geom_point() +
    labs(x = "Frequency", y = NULL)

textplot_wordcloud(dfmat_trigrams, max_words = 10)

# 0 %, 10 %, 50 %, 90 %, 100 % coverage

0
sum(cumsum(colSums(dfmat_trigrams) / sum(dfmat_trigrams)) < .1) + 1
sum(cumsum(colSums(dfmat_trigrams) / sum(dfmat_trigrams)) < .5) + 1
sum(cumsum(colSums(dfmat_trigrams) / sum(dfmat_trigrams)) < .9) + 1
ncol(dfmat_trigrams)
