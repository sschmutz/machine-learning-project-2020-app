library(tidyverse)
library(tidytext)
library(here)


# initialize the parameter ------------------------------------------------

prior_prob_20min <- 0.6331913
prior_prob_nzz <- 0.3668087

cond_prob_20min <- read_csv("data/cond_prob_20min.csv")
cond_prob_nzz <- read_csv("data/cond_prob_nzz.csv")


# naive bayes classifier --------------------------------------------------

news_classifier <- function(headline){
  
  headline_tibble <- tibble(text = headline)
  
  headline_tidy <-
    headline_tibble %>%
    unnest_tokens(word, text)
  
  # create score of headline for each class
  # replace NA in column prob after left_join by 1/sum(n words in training) in order
  # to account for a word which was not present in the training set of this class
  score_20min <-
    headline_tidy %>%
    left_join(cond_prob_20min, by = "word") %>%
    mutate(prob = replace_na(prob, 1/(sum(cond_prob_20min$n)+sum(cond_prob_nzz$n)))) %>%
    pull(prob) %>%
    prod() * prior_prob_20min
  
  score_nzz <-
    headline_tidy %>%
    left_join(cond_prob_nzz, by = "word") %>%
    mutate(prob = replace_na(prob, 1/(sum(cond_prob_20min$n)+sum(cond_prob_nzz$n)))) %>%
    pull(prob) %>%
    prod() * prior_prob_nzz
  
  prob_20min <-
    score_20min/(score_20min+score_nzz)
  
  prob_nzz <-
    score_nzz/(score_20min+score_nzz)
  
  # compare probabilities to determine the larger (=predicted class)
  predicted_class <- c("<p style='color:#0d2880';>20min</p>", "<p style='color:#A40E4C';>NZZ</p>")[which.max(c(prob_20min, prob_nzz))]
  
  return(predicted_class)
  
}


# headline <- "Grosse Rettungsaktion, weil sich mehrere Personen verirrt haben"
# news_classifier(headline)