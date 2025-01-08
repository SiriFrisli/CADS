library(tidyverse)
library(tidytext)
library(tm)

no_negative <- read.csv("https://github.com/ltgoslo/norsentlex/raw/master/Fullform/Fullform_Negative_lexicon.txt", header = FALSE, fileEncoding = "UTF-8")
no_positive <- read.csv("https://github.com/ltgoslo/norsentlex/raw/master/Fullform/Fullform_Positive_lexicon.txt", header = FALSE, fileEncoding = "UTF-8")
names(no_negative) <- "word"
names(no_positive) <- "word"

twitter_sent <- readRDS("D:/Data/Datasets/CADS_datasets/twitter_topics_30.RDS")

# twitter_count <- twitter_sent |>
#   group_by(topic) |>
#   count(topic)

################################################################################
## overall score for each topic

# Define negation words
negation_words <- c("ikke", "aldri")

tidy_twitter_neg <- twitter_sent |>
  group_by(topic) |>
  unnest_tokens(word, tweet) |>
  mutate(
    sentiment = case_when(
      word %in% no_positive$word & lag(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & lead(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & !(lag(word) %in% negation_words) ~ "positive",
      word %in% no_positive$word & !(lead(word) %in% negation_words) ~ "positive",
      
      word %in% no_negative$word & lag(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & lead(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & !(lag(word) %in% negation_words) ~ "negative",
      word %in% no_negative$word & !(lead(word) %in% negation_words) ~ "negative",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(sentiment))

sentiment_scores_neg <- tidy_twitter_neg |>
  count(topic, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  left_join(tidy_twitter_neg |>
              group_by(topic) |>
              summarize(total_words = n(), .groups = "drop"),
            by = "topic") |>
  mutate(sentiment_score = (positive - negative) / total_words)

sentiment_scores_neg$topic <- gsub("Topic", "", sentiment_scores_neg$topic)
sentiment_scores_neg$topic <- as.numeric(sentiment_scores_neg$topic)

sentiment_scores_neg |>
  ggplot(aes(x = topic, y = sentiment_score)) +
  geom_col() +
  labs(title = "Sentiment Scores for Norwegian COVID-19 Tweets",
       x = "Topic", 
       y = "Sentiment Score") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

sentiment_scores_neg$sentiment_score <- format(sentiment_scores_neg$sentiment_score, scientific = FALSE)

write.csv2(sentiment_scores_neg, "~/CADS/Results/sentiment_scores_30_avg_neg.csv")

################################################################################
## Sent score grouped by label

tidy_twitter_label<- twitter_sent |>
  group_by(topic, label) |>
  unnest_tokens(word, tweet) |>
  mutate(
    sentiment = case_when(
      word %in% no_positive$word & lag(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & lead(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & !(lag(word) %in% negation_words) ~ "positive",
      word %in% no_positive$word & !(lead(word) %in% negation_words) ~ "positive",
      
      word %in% no_negative$word & lag(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & lead(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & !(lag(word) %in% negation_words) ~ "negative",
      word %in% no_negative$word & !(lead(word) %in% negation_words) ~ "negative",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(sentiment))

sentiment_scores_label <- tidy_twitter_label |>
  count(topic, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  left_join(tidy_twitter_label |>
              group_by(topic, label) |>
              summarize(total_words = n(), .groups = "drop"),
            by = c("topic", "label")) |>
  mutate(sentiment_score = (positive - negative) / total_words)

sentiment_scores_label$topic <- gsub("Topic", "", sentiment_scores_label$topic)
sentiment_scores_label$topic <- as.numeric(sentiment_scores_label$topic)

sentiment_scores_label$sentiment_score <- format(sentiment_scores_label$sentiment_score, scientific = FALSE)

write.csv2(sentiment_scores_label, "~/CADS/Results/sentiment_scores_30_label.csv")

################################################################################
## timeline per topic

twitter_sent$label <- case_when(
  twitter_sent$label == "non.misinfo" ~"non misinformation",
  twitter_sent$label == "misinfo" ~"misinformation"
)

twitter_sent$quarter <- quarter(twitter_sent$date, with_year = TRUE)

twitter_4 <- twitter_sent |>
  select(tweet, id, quarter, label, topic, topic_value) |>
  filter(topic == "Topic4")

tidy_twitter_4 <- twitter_4 |>
  group_by(topic, quarter, label) |>
  unnest_tokens(word, tweet) |>
  mutate(
    sentiment = case_when(
      word %in% no_positive$word & lag(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & lead(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & !(lag(word) %in% negation_words) ~ "positive",
      word %in% no_positive$word & !(lead(word) %in% negation_words) ~ "positive",
      
      word %in% no_negative$word & lag(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & lead(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & !(lag(word) %in% negation_words) ~ "negative",
      word %in% no_negative$word & !(lead(word) %in% negation_words) ~ "negative",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(sentiment))

sentiment_scores_4 <- tidy_twitter_4 |>
  count(topic, quarter, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  left_join(tidy_twitter_4 |>
              group_by(topic, quarter, label) |>
              summarize(total_words = n(), .groups = "drop"),
            by = c("topic", "quarter", "label")) |>
  mutate(sentiment_score = (positive - negative) / total_words)

sent_4 <- sentiment_scores_4 |>
  ggplot(aes(x = as.factor(quarter), y = sentiment_score, group = label, color = label)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(-0.6, 0.15)) +
  labs(title = "",
       subtitle = "4: American Politics",
       x = "Quarter",
       y = "Sentiment Score",
       color = "Label") +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.subtitle = element_text(size = 12))

twitter_6 <- twitter_sent |>
  select(tweet, id, quarter, label, topic, topic_value) |>
  filter(topic == "Topic6")

tidy_twitter_6 <- twitter_6 |>
  group_by(topic, quarter, label) |>
  unnest_tokens(word, tweet) |>
  mutate(
    sentiment = case_when(
      word %in% no_positive$word & lag(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & lead(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & !(lag(word) %in% negation_words) ~ "positive",
      word %in% no_positive$word & !(lead(word) %in% negation_words) ~ "positive",
      
      word %in% no_negative$word & lag(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & lead(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & !(lag(word) %in% negation_words) ~ "negative",
      word %in% no_negative$word & !(lead(word) %in% negation_words) ~ "negative",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(sentiment))

sentiment_scores_6 <- tidy_twitter_6 |>
  count(topic, quarter, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  left_join(tidy_twitter_6 |>
              group_by(topic, quarter, label) |>
              summarize(total_words = n(), .groups = "drop"),
            by = c("topic", "quarter", "label")) |>
  mutate(sentiment_score = (positive - negative) / total_words)

sent_6 <- sentiment_scores_6 |>
  ggplot(aes(x = as.factor(quarter), y = sentiment_score, group = label, color = label)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(-0.6, 0.15)) +
  labs(title ="",
       subtitle = "6: Economy and Finance",
       x = "Quarter",
       y = "Sentiment Score",
       color = "Label") +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), legend.position = "none",
        plot.subtitle = element_text(size = 12))

twitter_13 <- twitter_sent |>
  select(tweet, id, quarter, label, topic, topic_value) |>
  filter(topic == "Topic13")

tidy_twitter_13 <- twitter_13 |>
  group_by(topic, quarter, label) |>
  unnest_tokens(word, tweet) |>
  mutate(
    sentiment = case_when(
      word %in% no_positive$word & lag(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & lead(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & !(lag(word) %in% negation_words) ~ "positive",
      word %in% no_positive$word & !(lead(word) %in% negation_words) ~ "positive",
      
      word %in% no_negative$word & lag(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & lead(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & !(lag(word) %in% negation_words) ~ "negative",
      word %in% no_negative$word & !(lead(word) %in% negation_words) ~ "negative",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(sentiment))

sentiment_scores_13 <- tidy_twitter_13 |>
  count(topic, quarter, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  left_join(tidy_twitter_13 |>
              group_by(topic, quarter, label) |>
              summarize(total_words = n(), .groups = "drop"),
            by = c("topic", "quarter", "label")) |>
  mutate(sentiment_score = (positive - negative) / total_words)

sent_13 <- sentiment_scores_13 |>
  ggplot(aes(x = as.factor(quarter), y = sentiment_score, group = label, color = label)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(-0.6, 0.15)) +
  labs(title ="",
       subtitle = "13: International News",
       x = "Quarter",
       y = "Sentiment Score",
       color = "Label") +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), legend.position = "none",
        plot.subtitle = element_text(size = 12))

twitter_18 <- twitter_sent |>
  select(tweet, id, quarter, label, topic, topic_value) |>
  filter(topic == "Topic18")

tidy_twitter_18 <- twitter_18 |>
  group_by(topic, quarter, label) |>
  unnest_tokens(word, tweet) |>
  mutate(
    sentiment = case_when(
      word %in% no_positive$word & lag(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & lead(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & !(lag(word) %in% negation_words) ~ "positive",
      word %in% no_positive$word & !(lead(word) %in% negation_words) ~ "positive",
      
      word %in% no_negative$word & lag(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & lead(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & !(lag(word) %in% negation_words) ~ "negative",
      word %in% no_negative$word & !(lead(word) %in% negation_words) ~ "negative",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(sentiment))

sentiment_scores_18 <- tidy_twitter_18 |>
  count(topic, quarter, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  left_join(tidy_twitter_18 |>
              group_by(topic, quarter, label) |>
              summarize(total_words = n(), .groups = "drop"),
            by = c("topic", "quarter", "label")) |>
  mutate(sentiment_score = (positive - negative) / total_words)

sent_18 <- sentiment_scores_18 |>
  ggplot(aes(x = as.factor(quarter), y = sentiment_score, group = label, color = label)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(-0.6, 0.15)) +
  labs(title ="",
       subtitle = "18: International Politics",
       x = "Quarter",
       y = "Sentiment Score",
       color = "Label") +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), legend.position = "none",
        plot.subtitle = element_text(size = 12))

################################################################################
twitter_1 <- twitter_sent |>
  select(tweet, id, quarter, label, topic, topic_value) |>
  filter(topic == "Topic1")

tidy_twitter_1 <- twitter_1 |>
  group_by(topic, quarter, label) |>
  unnest_tokens(word, tweet) |>
  mutate(
    sentiment = case_when(
      word %in% no_positive$word & lag(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & lead(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & !(lag(word) %in% negation_words) ~ "positive",
      word %in% no_positive$word & !(lead(word) %in% negation_words) ~ "positive",
      
      word %in% no_negative$word & lag(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & lead(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & !(lag(word) %in% negation_words) ~ "negative",
      word %in% no_negative$word & !(lead(word) %in% negation_words) ~ "negative",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(sentiment))

sentiment_scores_1 <- tidy_twitter_1 |>
  count(topic, quarter, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  left_join(tidy_twitter_1 |>
              group_by(topic, quarter, label) |>
              summarize(total_words = n(), .groups = "drop"),
            by = c("topic", "quarter", "label")) |>
  mutate(sentiment_score = (positive - negative) / total_words)

sent_1 <- sentiment_scores_1 |>
  ggplot(aes(x = as.factor(quarter), y = sentiment_score, group = label, color = label)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(-0.4,0.3)) +
  labs(title = "",
       subtitle = "1: Characteristics of the Virus",
       x = "Quarter",
       y = "Sentiment Score",
       color = "Label") +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.subtitle = element_text(size = 12))

twitter_2 <- twitter_sent |>
  select(tweet, id, quarter, label, topic, topic_value) |>
  filter(topic == "Topic2")

tidy_twitter_2 <- twitter_2 |>
  group_by(topic, quarter, label) |>
  unnest_tokens(word, tweet) |>
  mutate(
    sentiment = case_when(
      word %in% no_positive$word & lag(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & lead(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & !(lag(word) %in% negation_words) ~ "positive",
      word %in% no_positive$word & !(lead(word) %in% negation_words) ~ "positive",
      
      word %in% no_negative$word & lag(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & lead(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & !(lag(word) %in% negation_words) ~ "negative",
      word %in% no_negative$word & !(lead(word) %in% negation_words) ~ "negative",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(sentiment))

sentiment_scores_2 <- tidy_twitter_2 |>
  count(topic, quarter, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  left_join(tidy_twitter_2 |>
              group_by(topic, quarter, label) |>
              summarize(total_words = n(), .groups = "drop"),
            by = c("topic", "quarter", "label")) |>
  mutate(sentiment_score = (positive - negative) / total_words)

sent_2 <- sentiment_scores_2 |>
  ggplot(aes(x = as.factor(quarter), y = sentiment_score, group = label, color = label)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(-0.4,0.3)) +
  labs(title ="",
       subtitle = "2: Dangerous Vaccines and Side Effects",
       x = "Quarter",
       y = "Sentiment Score",
       color = "Label") +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), legend.position = "none",
        plot.subtitle = element_text(size = 12))

twitter_12 <- twitter_sent |>
  select(tweet, id, quarter, label, topic, topic_value) |>
  filter(topic == "Topic12")

tidy_twitter_12 <- twitter_12 |>
  group_by(topic, quarter, label) |>
  unnest_tokens(word, tweet) |>
  mutate(
    sentiment = case_when(
      word %in% no_positive$word & lag(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & lead(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & !(lag(word) %in% negation_words) ~ "positive",
      word %in% no_positive$word & !(lead(word) %in% negation_words) ~ "positive",
      
      word %in% no_negative$word & lag(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & lead(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & !(lag(word) %in% negation_words) ~ "negative",
      word %in% no_negative$word & !(lead(word) %in% negation_words) ~ "negative",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(sentiment))

sentiment_scores_12 <- tidy_twitter_12 |>
  count(topic, quarter, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  left_join(tidy_twitter_12 |>
              group_by(topic, quarter, label) |>
              summarize(total_words = n(), .groups = "drop"),
            by = c("topic", "quarter", "label")) |>
  mutate(sentiment_score = (positive - negative) / total_words)

sent_12 <- sentiment_scores_12 |>
  ggplot(aes(x = as.factor(quarter), y = sentiment_score, group = label, color = label)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(-0.4,0.3)) +
  labs(title ="",
       subtitle = "12: Mortality Rates and Vaccination Efforts",
       x = "Quarter",
       y = "Sentiment Score",
       color = "Label") +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), legend.position = "none",
        plot.subtitle = element_text(size = 12))

twitter_17 <- twitter_sent |>
  select(tweet, id, quarter, label, topic, topic_value) |>
  filter(topic == "Topic17")

tidy_twitter_17 <- twitter_17 |>
  group_by(topic, quarter, label) |>
  unnest_tokens(word, tweet) |>
  mutate(
    sentiment = case_when(
      word %in% no_positive$word & lag(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & lead(word) %in% negation_words ~ "negative",
      word %in% no_positive$word & !(lag(word) %in% negation_words) ~ "positive",
      word %in% no_positive$word & !(lead(word) %in% negation_words) ~ "positive",
      
      word %in% no_negative$word & lag(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & lead(word) %in% negation_words ~ "positive",
      word %in% no_negative$word & !(lag(word) %in% negation_words) ~ "negative",
      word %in% no_negative$word & !(lead(word) %in% negation_words) ~ "negative",
      TRUE ~ NA_character_
    )
  ) |>
  filter(!is.na(sentiment))

sentiment_scores_17 <- tidy_twitter_17 |>
  count(topic, quarter, sentiment) |>
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) |>
  left_join(tidy_twitter_17 |>
              group_by(topic, quarter, label) |>
              summarize(total_words = n(), .groups = "drop"),
            by = c("topic", "quarter", "label")) |>
  mutate(sentiment_score = (positive - negative) / total_words)

sent_17 <- sentiment_scores_17 |>
  ggplot(aes(x = as.factor(quarter), y = sentiment_score, group = label, color = label)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(-0.4,0.3)) +
  labs(title ="",
       subtitle = "17: General Vaccine Discussions",
       x = "Quarter",
       y = "Sentiment Score",
       color = "Label") +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12), legend.position = "none",
        plot.subtitle = element_text(size = 12))
