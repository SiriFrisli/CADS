packages <- c("tidyverse", "tidytext", "stm", "quanteda", "stopwords", "readxl",
              "Rtsne", "rsvd", "geometry", "tm", "tidystm", "stminsights", "igraph",
              "factoextra", "GGally", "labelled", "patchwork")
lapply(packages, library, character.only = TRUE)

Sys.setlocale(locale = "Norwegian")

theme_set(
  theme_bw() +
    theme(legend.position = "top")
  )


################################################################################

# Loading the data fixing the date column, and removing usernames and retweets
twitter <- readRDS("D:/Data/Datasets/covid_classified_final.RDS")

twitter$date <- ym(twitter$date)
glimpse(twitter)

# one of the date values is missing, need to fix this
which(is.na(twitter$date)) # 62653
row_index <- 62653
twitter$date[row_index] <- as.Date("2020-03-01")

# The date column must be nummeric for the STM
twitter$date_num <- as.numeric(twitter$date)

twitter_unnest <- unnest(twitter, referenced_tweets, names_sep = "_ref_", keep_empty = TRUE) 

twitter_rt <- twitter_unnest |>
  filter(referenced_tweets_ref_type == "retweeted") |>
  rename("orig_id" = "id")

twitter_nort <- twitter_unnest |>
  anti_join(twitter_rt, by = "id")

no_match_id <- twitter_rt |>
  anti_join(twitter_nort, by = c("referenced_tweets_ref_id" = "id"))

# saving this dataset for easy access
saveRDS(twitter_nort, "D:/Data/Datasets/CADS_datasets/twitter_data_no_rt.RDS")

################################################################################

twitter <- readRDS("E:/Data/Datasets/CADS_datasets/twitter_data_no_rt.RDS")
glimpse(twitter)

removeUsernames <- function(tweet) {
  return(gsub("@[a-z,A-Z,_]*[0-9]*[a-z,A-Z,_]*[0-9]*", "", tweet))
}
twitter$tweet <- apply(twitter["tweet"], 1, removeUsernames)

# Remove the "rt" at the start of retweeted tweets
# twitter$tweet <- gsub("^rt", "", twitter$tweet)
# twitter$tweet <- gsub("^RT", "", twitter$tweet)

# Converting the data to a corpus from the quanteda package
twitter_corpus <- corpus(twitter, text_field = "tweet")

# Loading a list of stopwords
stopwords <- read_xlsx("~/INORK_ST/stopwords.xlsx")
custom_words <- stopwords |>
  pull(word)

# Tokenizing, removing punctuation, symbols, etc.
twitter_tokens <- quanteda::tokens(twitter_corpus,
                                   split_hyphens = FALSE,
                                   remove_punct = TRUE,
                                   remove_symbols = TRUE,
                                   remove_numbers = TRUE,
                                   remove_url = TRUE,
                                   remove_separators = TRUE,
                                   split_tags = FALSE) |>
  tokens_remove(pattern = c(stopwords::stopwords(source = "snowball", language = "no"), custom_words)) |>
  tokens_remove(pattern = c(stopwords::stopwords(source = "snowball", language = "en")))

tokens_ngrams <- tokens_compound(twitter_tokens, 
                                 phrase(c("korona virus", "corona virus", "korona pandemi", "corona pandemi", 
                                          "korona viruset", "corona viruset", "korona pandemien", "corona pandemien", 
                                          "korona vaksine", "corona vaksine", "korona vaksinen", "corona vaksinen", 
                                          "fake news", "falske nyheter", "falsk nyhet", "pål steigan", "sosiale medier",
                                          "røde kors", "erna solberg", "jonas gahr støre", "bent høie", "ingvild kjerkol",
                                          "donald trump", "jørn sigurd maurud", "anthony fauci", "new york times", 
                                          "joe biden", "boris johnson", "big pharma", "bill gates", "stig frøland",
                                          "durek verrett", "joe rogan", "elon musk", "hong kong", "long covid",
                                          "espen nakstad", "zero covid", "karl johan", "raymon johansen", case_insensitive = TRUE)))

twitter_dfm <- dfm(tokens_ngrams, tolower = TRUE)
twitter_stm <- convert(twitter_dfm, to = "stm")
plotRemoved(twitter_stm$documents, lower.thresh = seq(1,100, by = 10))

twitter_prepped <- prepDocuments(twitter_stm$documents,
                                 twitter_stm$vocab, 
                                 twitter_stm$meta, 
                                 lower.thresh = 10)

twitter_prepped$meta$label <- as.factor(twitter_prepped$meta$label)

docs_twitter <- twitter_prepped$documents
vocab_twitter <- twitter_prepped$vocab
meta_twitter <- twitter_prepped$meta

# saveRDS(docs_twitter, "D:/Modell/CADS/docs_twitter.RDS")
# saveRDS(vocab_twitter, "D:/Modell/CADS/vocab_twitter.RDS")
# saveRDS(meta_twitter, "D:/Modell/CADS/meta_twitter.RDS")

################################################################################
stm_k_search <- stm(documents = docs_twitter,
                    vocab = vocab_twitter,
                    K = 0,
                    prevalence = ~label * s(date_num),
                    content = ~label,
                    max.em.its = 75,
                    data = meta_twitter,
                    init.type = "Spectral",
                    set.seed(1234),
                    verbose = TRUE)

stm_k_search # 76 topics

################################################################################

K <- c(20, 30, 40, 50, 60, 70)
stm_K_search <- searchK(documents = docs_twitter,
                        vocab = vocab_twitter,
                        K = K,
                        prevalence = ~label * s(date_num),
                        data = meta_twitter,
                        set.seed(1234),
                        verbose = TRUE)

# saveRDS(stm_K_search, "~/CADS/searchK.RDS")

plot(stm_K_search)
plot_stm_k <- data.frame("K" = K, 
                         "Coherence" = unlist(stm_K_search$results$semcoh),
                         "Exclusivity" = unlist(stm_K_search$results$exclus))

# Reshape to long format
library("reshape2")
plot <- melt(plot_stm_k, id=c("K"))
plot # 50 topics? Too many_

################################################################################

stm_30_interaction <- stm(documents = docs_twitter,
                          vocab = vocab_twitter,
                          K = 30,
                          prevalence = ~label * s(date_num),
                          content = ~label,
                          max.em.its = 75,
                          data = meta_twitter,
                          init.type = "Spectral",
                          set.seed(1234),
                          verbose = TRUE)
levels(meta_twitter$label) # misinformation as baseline?
saveRDS(stm_30_interaction, "D:/Modell/CADS/stm_30_interaction.RDS")

plot(stm_40_interaction)

###
### Topic distributions and words
###
stm_30_gamma <- tidy(stm_30_interaction,
                     matrix = "gamma")

stm_30_terms <- stm_30_gamma |>
  group_by(topic) |>
  summarise(gamma = mean(gamma)) |>
  arrange(desc(gamma)) |>
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_30_terms |>
  top_n(30, gamma) |>
  ggplot(aes(topic, gamma)) +
  geom_col(fill = "gray", color = "black", show.legend = FALSE) +
  labs(title = "Topic Proportions", y = "", x = "") +
  coord_flip()

# frex_20 <- tidy(stm_20_interaction, matrix = "frex") |>
#   group_by(topic) |>
#   slice_head(n = 10) |>
#   mutate(rank = row_number()) |>
#   ungroup() |>
#   pivot_wider(
#     names_from = "topic", 
#     names_glue = "topic {.name}",
#     values_from = term
#   ) |>
#   select(-rank) |>
#   knitr::kable()

topics_30 <- sageLabels(stm_30_interaction, n = 20)
sink("topics_30.txt", append = FALSE, split = TRUE)
print(topics_30)
sink()

################################################################################

effect30 <- estimateEffect(1:30 ~label * s(date_num),
                           stmobj = stm_30_interaction,
                           metadata = meta_twitter,
                           set.seed(1234),
                           uncertainty = "Global")
# saveRDS(effect30, "D:/Modell/CADS/effect30.RDS")

effect30_label <- get_effects(effect30, variable = "label",
                              type = "pointestimate")

effect30_label$topic <- reorder(x = effect30_label$topic,
                                effect30_label$proportion)

effect30_label$value <- str_replace_all(effect30_label$value, "misinfo", "misinformation")
effect30_label$value <- str_replace_all(effect30_label$value, "non.", "non-")

legend <- c("non-misinformation" = "#d8b365", "misinformation" = "#8c510a")

effect30_label_filt <- effect30_label |>
  filter(!topic %in% c(7, 10, 14, 27, 30)) |>
  droplevels()

effect30_label |>
  ggplot(aes(x = topic, y = proportion, color = value, group = value, fill = value)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.3) +
  scale_x_discrete(name = "Topic", limits = rev(levels(effect30_label_filt$topic))) +
  theme(legend.position = "inside",legend.position.inside = c(.8,.85), 
        legend.text = element_text(size = 14), legend.title = element_text(size = 15),
        axis.text.x = element_text(size = 12)) + 
  scale_color_manual(values = legend)+
  labs(title = "", y = "Proportion", x = "", color = "Label", fill = "Label")

ggsave("Figure 1.tiff",dpi = 300)

################################################################################
# specifying labels for the x-axis to be used in the plots

twitter$month <- format(twitter$date, format = "%Y-%m")

twitter_date <- twitter |>
  group_by(month, date_num) |>
  select(month, date_num) |>
  slice(1) |>
  ungroup()

twitter_date_2 <- twitter_date |> 
  filter(row_number() %% 2 != 0)

twitter_date <- as.character(twitter_date$month)
twitter_date_2 <- as.character(twitter_date_2$month)

# breaks_to_use <- unique(effect_time$value)[seq_along(twitter_date)]
labels_to_use <- twitter_date
labels_to_use_2 <- twitter_date_2
################################################################################

## after date
effect30_date <- extract.estimateEffect(effect30,
                                        "date_num",
                                        method = "continuous")

effect30_date <- effect30_date |>
  mutate(topic = fct_reorder(as.factor(topic), covariate.value))

effect30_date |>
  filter(topic == 23) |>
  ggplot(aes(x = covariate.value, y = estimate, group = topic,color = topic)) +
  geom_line(linewidth = 1) +
  scale_y_continuous() +
  scale_x_continuous(limits = c(18262,19312), 
                     breaks = seq(from = 18262, to = 19312, by = 30), 
                     labels = labels_to_use) + 
  theme(legend.position = "top", axis.text.x = element_text(angle = 45, hjust = 1))

################################################################################
# Change in time for the misinformation topics

effect_time <- get_effects(effect30, variable = "date_num",
                           type = "continuous",
                           moderator = "label",
                           modval = "misinfo") |>
  bind_rows(get_effects(effect30, variable = "date_num",
                        type = "continuous",
                        moderator = "label",
                        modval = "non.misinfo"))

# "#8c510a", "#d8b365", "#f6e8c3", "#c7eae5", "#5ab4ac", "#01665e"

top_five <-  effect_time |>
  filter(moderator == "misinfo") |>
  filter(topic %in% c(1, 6, 12, 13, 27))

top_five |>
   ggplot(aes(x = value, y = proportion, color = topic, group = topic, fill = moderator)) +
   geom_line(linewidth = 2) +
   scale_x_continuous(limits = c(18262,19312),
                      breaks = seq(from = 18262, to = 19312, by = 60),
                      labels = labels_to_use_2) +
   scale_y_continuous(limits = c(0,0.1)) +
   scale_color_manual(values=c("#8c510a", "#d8b365", "#f6e8c3", "#5ab4ac", "#01665e"), 
                      labels = c(
                        "1: Characteristics of the Virus",
                        "12: Statistics – Mortality Rates and Vaccination",
                        "13: International News",
                        "27: Sports and Jesus",
                        "6: Economy and Finance"
                      )) +
   theme(legend.position = "inside", legend.position.inside = c(.8,.8), 
         legend.text = element_text(size = 14), legend.title = element_text(size = 15),
         axis.text.x = element_text(angle = 45, hjust = 1, size = 12)) + 
   labs(title = "", y = "", x = "", color = "Topic")

health <- effect_time |>
  filter(moderator == "misinfo") |>
  filter(topic %in% c(1,2, 17, 24))
  
health_plot <- health |>
  ggplot(aes(x = value, y = proportion, color = topic, group = topic, fill = moderator)) +
  geom_line(linewidth = 2) +
  scale_x_continuous(limits = c(18262,19312),
                     breaks = seq(from = 18262, to = 19312, by = 60),
                     labels = labels_to_use_2) +
  scale_y_continuous(limits = c(0,0.1)) +
  scale_color_manual(values=c("#8c510a", "#d8b365", "#5ab4ac", "#01665e"), 
                     labels = c(
                       "1",
                       "17", 
                       "2", 
                       "24"
                     )) +
  labs(title = "Theme: Health and Medicine", y = "", x = "", color = "Topic")

governance <- effect_time |>
  filter(moderator == "misinfo") |>
  filter(topic %in% c(4, 11, 18, 21, 25)) 

governance_plot <- governance |>
  ggplot(aes(x = value, y = proportion, color = topic, group = topic, fill = moderator)) +
  geom_line(linewidth = 2) +
  scale_x_continuous(limits = c(18262,19312),
                     breaks = seq(from = 18262, to = 19312, by = 60),
                     labels = labels_to_use_2) +
  scale_y_continuous(limits = c(0,0.1)) +
  scale_color_manual(values=c("#8c510a", "#d8b365", "#f6e8c3", "#5ab4ac", "#01665e"), 
                     labels = c(
                       "11",
                       "18",
                       "21",
                       "25", 
                       "4"
                     )) +
  labs(title = "Theme: Governance and Policies", y = "", x = "", color = "Topic")

news <- effect_time |>
  filter(moderator == "misinfo") |>
  filter(topic %in% c(5, 8, 13, 15, 20, 26)) 

news_plot <- news |>
  ggplot(aes(x = value, y = proportion, color = topic, group = topic, fill = moderator)) +
  geom_line(linewidth = 2) +
  scale_x_continuous(limits = c(18262,19312),
                     breaks = seq(from = 18262, to = 19312, by = 60),
                     labels = labels_to_use_2) +
  scale_y_continuous(limits = c(0,0.1)) +
  scale_color_manual(values=c("#8c510a", "#d8b365", "#f6e8c3", "#c7eae5", "#5ab4ac", "#01665e"), 
                     labels = c(
                       "13",
                       "15",
                       "20",
                       "26", 
                       "5",
                       "8"
                     )) +
  labs(title = "Theme: News Discussions", y = "", x = "", color = "Topic")

society <- effect_time |>
  filter(moderator == "misinfo") |>
  filter(topic %in% c(3, 6, 22, 23, 28)) 

society_plot <- society |>
  ggplot(aes(x = value, y = proportion, color = topic, group = topic, fill = moderator)) +
  geom_line(linewidth = 2) +
  scale_x_continuous(limits = c(18262,19312),
                     breaks = seq(from = 18262, to = 19312, by = 60),
                     labels = labels_to_use_2) +
  scale_y_continuous(limits = c(0,0.1)) +
  scale_color_manual(values=c("#8c510a", "#d8b365", "#f6e8c3", "#5ab4ac", "#01665e"), 
                     labels = c(
                       "22",
                       "23",
                       "28", 
                       "3",
                       "6"
                     )) +
  labs(title = "Theme: Society and Societal Impact", y = "", x = "", color = "Topic")

statistics <- effect_time |>
  filter(moderator == "misinfo") |>
  filter(topic %in% c(9, 12, 16, 19, 29))

statistics_plot <- statistics |>
  ggplot(aes(x = value, y = proportion, color = topic, group = topic, fill = moderator)) +
  geom_line(linewidth = 2) +
  scale_x_continuous(limits = c(18262,19312),
                     breaks = seq(from = 18262, to = 19312, by = 60),
                     labels = labels_to_use_2) +
  scale_y_continuous(limits = c(0,0.1)) +
  scale_color_manual(values=c("#8c510a", "#d8b365", "#f6e8c3", "#5ab4ac", "#01665e"), 
                     labels = c(
                       "12",
                       "16",
                       "19",
                       "29",
                       "9"
                     )) +
  labs(title = "Theme: Statistics and Research", y = "", x = "", color = "Topic")

misc <- effect_time |>
  filter(moderator == "misinfo") |>
  filter(topic %in% c(7, 10, 14, 27, 30)) 

misc_plot <- misc |>
  ggplot(aes(x = value, y = proportion, color = topic, group = topic, fill = moderator)) +
  geom_line(linewidth = 2) +
  scale_x_continuous(limits = c(18262,19312),
                     breaks = seq(from = 18262, to = 19312, by = 60),
                     labels = labels_to_use_2) +
  scale_y_continuous(limits = c(0,0.1)) +
  scale_color_manual(values=c("#8c510a", "#d8b365", "#f6e8c3", "#5ab4ac", "#01665e"), 
                     labels = c(
                       "10",
                       "14",
                       "27",
                       "30",
                       "7"
                     )) +
  labs(title = "Theme: Miscellaneous", y = "", x = "", color = "Topic")

health_plot + governance_plot + news_plot + society_plot + statistics_plot + misc_plot +
  plot_layout(ncol = 2, axes = "collect") & 
  theme(legend.position = "top", 
        legend.text = element_text(size = 10), legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12))

################################################################################
# Compare prevalence between label within topics

effect_time$moderator <- case_when(
  effect_time$moderator == "non.misinfo" ~ "non misinformation",
  effect_time$moderator == "misinfo" ~ "misinformation")  

prev_4 <- effect_time |>
  filter(topic==4) |>
  mutate(moderator = as.factor(moderator)) |>
  ggplot(aes(x = value, y = proportion, color = moderator, group = moderator, fill = moderator)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(0.01,0.094)) +
  scale_x_continuous(limits = c(18262,19312),
                      breaks = seq(from = 18262, to = 19312, by = 60),
                      labels = labels_to_use_2) +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(legend.position = "inside", legend.position.inside = c(.5,.85),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.subtitle = element_text(size = 12)) +
    labs(title = "", 
         subtitle = "4: American Politics", 
         y = "Prevalence", x = "Month", color = "Label")

prev_6 <- effect_time |>
  filter(topic==6) |>
  mutate(moderator = as.factor(moderator)) |>
  ggplot(aes(x = value, y = proportion, color = moderator, group = moderator, fill = moderator)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(0.01,0.094)) +
  scale_x_continuous(limits = c(18262,19312),
                     breaks = seq(from = 18262, to = 19312, by = 60),
                     labels = labels_to_use_2) +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(legend.position = "inside", legend.position.inside = c(.5,.85),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.subtitle = element_text(size = 12)) +
  labs(title = "",
       subtitle = "6: Economy and Finance", 
       y = "Prevalence", x = "Month", color = "Label")

prev_13 <- effect_time |>
  filter(topic==13) |>
  mutate(moderator = as.factor(moderator)) |>
  ggplot(aes(x = value, y = proportion, color = moderator, group = moderator, fill = moderator)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(0.01,0.094)) +
  scale_x_continuous(limits = c(18262,19312),
                     breaks = seq(from = 18262, to = 19312, by = 60),
                     labels = labels_to_use_2) +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(legend.position = "inside", legend.position.inside = c(.5,.85),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.subtitle = element_text(size = 12)) +
  labs(title = "",
       subtitle = "13: International News", 
       y = "Prevalence", x = "Month", color = "Label")

prev_18 <- effect_time |>
  filter(topic==18) |>
  mutate(moderator = as.factor(moderator)) |>
  ggplot(aes(x = value, y = proportion, color = moderator, group = moderator, fill = moderator)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(0.01,0.094)) +
  scale_x_continuous(limits = c(18262,19312),
                     breaks = seq(from = 18262, to = 19312, by = 60),
                     labels = labels_to_use_2) +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(legend.position = "inside", legend.position.inside = c(.5,.85),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.subtitle = element_text(size = 12)) +
  labs(title = "", 
       subtitle = "18: International Politics",
       y = "Prevalence", x = "Month", color = "Label")

prev_4 + prev_6 + prev_13 + prev_18  +
  plot_layout(ncol = 2, axes = "collect") &
  theme(title = element_text(size = 12))

ggsave("Figure 2.tiff", width = 2500, height = 2800, unit = "px", dpi = 300, limitsize = FALSE)

################################################################################
prev_1 <- effect_time |>
  filter(topic==1) |>
  mutate(moderator = as.factor(moderator)) |>
  ggplot(aes(x = value, y = proportion, color = moderator, group = moderator, fill = moderator)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(0.02,0.14), breaks = c(0.04,0.08,0.12)) +
  scale_x_continuous(limits = c(18262,19312),
                     breaks = seq(from = 18262, to = 19312, by = 60),
                     labels = labels_to_use_2) +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(legend.position = "top",  axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.subtitle = element_text(size = 12)) +
  labs(title = "", 
       subtitle = "1: Characteristics of the Virus", 
       y = "Prevalence", x = "Month", color = "Label")

prev_2 <- effect_time |>
  filter(topic==2) |>
  mutate(moderator = as.factor(moderator)) |>
  ggplot(aes(x = value, y = proportion, color = moderator, group = moderator, fill = moderator)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(0.02,0.14), breaks = c(0.04,0.08,0.12)) +
  scale_x_continuous(limits = c(18262,19312),
                     breaks = seq(from = 18262, to = 19312, by = 60),
                     labels = labels_to_use_2) +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.subtitle = element_text(size = 12)) +
  labs(title = "",
       subtitle = "2: Dangerous Vaccines and Side Effects", 
       y = "Prevalence", x = "Month", color = "Label")

prev_12 <- effect_time |>
  filter(topic==12) |>
  mutate(moderator = as.factor(moderator)) |>
  ggplot(aes(x = value, y = proportion, color = moderator, group = moderator, fill = moderator)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(0.02,0.14), breaks = c(0.04,0.08,0.12)) +
  scale_x_continuous(limits = c(18262,19312),
                     breaks = seq(from = 18262, to = 19312, by = 60),
                     labels = labels_to_use_2) +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.subtitle = element_text(size = 12)) +
  labs(title = "",
       subtitle = "12: Mortality Rates and Vaccination Efforts", 
       y = "Prevalence", x = "Month", color = "Label")

prev_17 <- effect_time |>
  filter(topic==17) |>
  mutate(moderator = as.factor(moderator)) |>
  ggplot(aes(x = value, y = proportion, color = moderator, group = moderator, fill = moderator)) +
  geom_line(linewidth = 2) +
  scale_y_continuous(limits = c(0.012,0.14), breaks = c(0.04,0.08,0.12)) +
  scale_x_continuous(limits = c(18262,19312),
                     breaks = seq(from = 18262, to = 19312, by = 60),
                     labels = labels_to_use_2) +
  scale_color_manual(values=c("#8c510a", "#d8b365")) +
  theme(legend.position = "none",  axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        plot.subtitle = element_text(size = 12)) +
  labs(title = "", 
       subtitle = "17: General Vaccine Discussions",
       y = "Prevalence", x = "Month", color = "Label")

prev_1 + sent_1 + prev_2 + sent_2 + prev_12 + sent_12 + prev_17 + sent_17 +
  plot_layout(ncol = 2, axes = "collect") &
  theme(title = element_text(size = 10))

ggsave("Figure 3.tiff", width = 2300, height = 3000, unit = "px", dpi = 300, limitsize = FALSE)
################################################################################
## Extract tweets related to the topics
twitter_topics <- twitter |>
  semi_join(meta_twitter, by = "id")

theta <- make.dt(stm_30_interaction) 
theta1 <- theta |>
  arrange(desc(Topic6)) |>
  select(docnum, Topic6) |>
  slice(1:500)

tweets_1 <- twitter_topics[as.numeric(theta1$docnum), , drop = FALSE]
duplicates <- tweets_1 |>
  group_by(id) |>
  filter(n() > 1) |>
  ungroup()
# tweets_17 <- tweets_17[-72,]
tweets_1 <- tweets_1 |>
  anti_join(duplicates, by = "id")

tweets_1_mis <- tweets_1 |>
  filter(label == "misinfo")

tweets_28_nonmis <- tweets_28 |>
  filter(label == "non.misinfo")

tweets_1_full <- orig_twitter |>
  semi_join(tweets_1_mis, by = "id") |>
  select(c(text, id))

tweets_1_full <- tweets_1_full |>
  merge(tweets_1, by = "id")

write.csv2(tweets_1_full, "C:/Users/sirifris/OneDrive - OsloMet/Dokumenter/PHD/Article 2/topic_1_mis.csv")

################################################################################
## Assign topic-value to the original data

theta <- make.dt(stm_30_interaction)
theta_df <- as.data.frame(theta)
theta_df <- theta_df |>
  select(-docnum)

theta_df$topic <- names(theta_df)[max.col(theta_df)]
theta_df$topic_value <- apply(theta_df[-31], 1, max)

theta_df <- theta_df |>
  select(topic, topic_value)

twitter_topics <- twitter |>
  semi_join(meta_twitter, by = "id")

twitter_topics <- twitter_topics |>
  cbind(theta_df)

saveRDS(twitter_topics, "D:/Data/Datasets/CADS_datasets/twitter_topics_30.RDS")

################################################################################
## Clusters 

out_corr40 <- topicCorr(stm_30_interaction, cutoff = 0.04, set.seed(1234))
out_corr40$cor[out_corr40$cor<0.04] <- 0

network <- graph_from_adjacency_matrix(out_corr40$cor, weighted = T, mode = "undirected", diag =
                                          F, set.seed(1234))

plot(network, vertex.color = "light blue", vertex.label.color = "black")

out_corr40_2 <- topicCorr(stm_40_interaction, cutoff = 0.06, set.seed(1234))
cluster <- eclust(scale(out_corr40_2$cor), "hclust", k = 8, nboot = 500)

fviz_dend(cluster, rect = TRUE) # Dendrogram

clust_col <- cluster$cluster
clust_col <- to_factor(clust_col)

#leg.txt <- c("Svensk/dansk", "Engelsk", "Midtøsten", "Politikk og lov", "Div") # Labels

network_2 <- graph_from_adjacency_matrix(out_corr40_2$cor, weighted = TRUE, mode = "undirected", diag = FALSE, set.seed(1234))
plot(network_2, vertex.color = clust_col, vertex.label.color = "black",) # Med farger etter dendrogrammet

legend("topleft", leg.txt, fill = c("dark green", "blue", "orange", "light blue", "yellow"))



corr <- as.data.frame(out_corr40$cor)

ggc <- ggcorr(out_corr40$cor, hjust = .85, size = 0, label = TRUE, method = c("pairwise", "pearson"))
dat <- data.frame(x=seq(corr), y=seq(corr),
                  lbs = gsub("V", "\n", names(corr) ))

ggc +
  geom_text(data = dat, aes(x, y, label = lbs), nudge_x = 0.1, nudge_y = 0.4)
