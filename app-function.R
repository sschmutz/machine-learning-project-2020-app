library(tidyverse)
library(tidytext)
library(ggtext)
library(here)


# initialize the parameter ------------------------------------------------

prior_prob_20min <- 0.6331913
prior_prob_nzz <- 0.3668087

cond_prob_20min <- read_csv("data/cond_prob_20min.csv")
cond_prob_nzz <- read_csv("data/cond_prob_nzz.csv")

# source: https://github.com/solariz/german_stopwords/blob/master/german_stopwords_plain.txt
stop_words_german <-
  read_csv(here("data", "stop_words_german.txt"), col_names = c("word"))


# naive bayes classifier --------------------------------------------------

class_conditional_probability <- function(headline){
  
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
    mutate(class = "20min")
  
  score_nzz <-
    headline_tidy %>%
    left_join(cond_prob_nzz, by = "word") %>%
    mutate(class = "nzz")
  
  return(bind_rows(score_20min, score_nzz))

}

# headline <- "Grosse Rettungsaktion, weil sich mehrere Personen verirrt haben"
# class_conditional_probability(headline)


class_conditional_probability_plot <- function(headline){
  
  class_cond_prob <-
    class_conditional_probability(headline) %>%
    #anti_join(stop_words_german, by = "word") %>%
    select(word, prob, class) %>%
    unique() %>%
    mutate(prob = replace_na(prob, 1/(sum(cond_prob_20min$n)+sum(cond_prob_nzz$n)))) %>%
    pivot_wider(names_from = class, values_from = prob) %>%
    mutate(prob_ratio = `20min`/nzz)
  
  largest_diff <-
    class_cond_prob %>%
    arrange(prob_ratio) %>%
    pull(word)
  
  limit <-
    class_cond_prob$prob_ratio %>%
    max(na.rm = TRUE) + 10

  class_cond_prob %>%
    mutate(word = factor(word, largest_diff)) %>%
    ggplot(aes(x = prob_ratio, y = word)) +
    geom_col(aes(fill = prob_ratio > 1), alpha = 0.6) +
    scale_fill_manual(values = c("#A40E4C", "#0d2880")) +
    scale_x_log10(limits = c(1/limit, limit)) +
    theme_minimal(base_family = "Source Sans Pro",
                  base_size = 16) +
    theme(legend.position = "none",
          plot.title = element_markdown(hjust = 0.5),
          plot.subtitle = element_markdown(hjust = 0.5)) + 
    labs(x = NULL,
         y = NULL,
         fill = NULL,
         title = "**Class Conditional Probabily Ratio**",
         subtitle = "<span style='color:#A40E4C'>**NZZ**</span> and <span style='color:#0d2880'>**20min**</span>")
  
}

# headline <- "Grosse Rettungsaktion, weil sich mehrere Personen verirrt haben"
# headline <- "Künstliche Intelligenz sagt erfolgreich voraus, zu welcher Struktur sich Proteine falten"
# class_conditional_probability_plot(headline)


news_classifier <- function(headline){
  
  class_cond_prob <- class_conditional_probability(headline)

  # create score of headline for each class
  # replace NA in column prob after left_join by 1/sum(n words in training) in order
  # to account for a word which was not present in the training set of this class
  score_20min <-
    class_cond_prob %>%
    filter(class == "20min") %>%
    mutate(prob = replace_na(prob, 1/(sum(cond_prob_20min$n)+sum(cond_prob_nzz$n)))) %>%
    pull(prob) %>%
    prod() * prior_prob_20min
  
  score_nzz <-
    class_cond_prob %>%
    filter(class == "nzz") %>%
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