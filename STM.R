packages <- c("tidyverse", "tidytext", "stm", "quanteda", "stopwords", "readxl",
              "Rtsne", "rsvd", "geometry", "tm", "tidystm", "stminsights")
lapply(packages, library, character.only = TRUE)

Sys.setlocale(locale = "Norwegian")

################################################################################

# Loading the data fixing the date column, and removing usernames
twitter <- readRDS("D:/Data/Datasets/covid_classified_final.RDS")

twitter$date <- ym(twitter$date)
glimpse(twitter)

# one of the date values is missing, need to fix this
which(is.na(twitter$date)) # 62653
row_index <- 62653
twitter$date[row_index] <- as.Date("2020-03-01")

# The date column must be nummeric for the STM
twitter$date_num <- as.numeric(twitter$date)

# saving this dataset for easy access
saveRDS(twitter, "D:/Data/Datasets/CADS_datasets/twitter_data.RDS")

################################################################################

twitter <- readRDS("E:/Data/Datasets/CADS_datasets/twitter_data.RDS")
glimpse(twitter)

removeUsernames <- function(tweet) {
  return(gsub("@[a-z,A-Z,_]*[0-9]*[a-z,A-Z,_]*[0-9]*", "", tweet))
}
twitter$tweet <- apply(twitter["tweet"], 1, removeUsernames)

# Remove the "rt" at the start of retweeted tweets
twitter$tweet <- gsub("^rt", "", twitter$tweet)
twitter$tweet <- gsub("^RT", "", twitter$tweet)

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
                                          "espen nakstad", case_insensitive = TRUE)))

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

# Some testing
stm_10 <- stm(documents = docs_twitter,
                    vocab = vocab_twitter,
                    K = 10,
                    prevalence = ~label + s(date_num),
                    content = ~label,
                    max.em.its = 75,
                    data = meta_twitter,
                    init.type = "Spectral",
                    set.seed(1234),
                    verbose = TRUE)

stm_10_gamma <- tidy(stm_10,
                    matrix = "gamma")

stm_10_terms <- stm_10_gamma |>
  group_by(topic) |>
  summarise(gamma = mean(gamma)) |>
  arrange(desc(gamma)) |>
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_10_terms |>
  top_n(20, gamma) |>
  ggplot(aes(topic, gamma)) +
  geom_col(fill = "#00bfc4", color = "black", show.legend = FALSE) +
  labs(title = "Topic distributions", y = "", x = "") +
  coord_flip()

topics <- sageLabels(stm_10, n = 20)
sink("topics.txt", append = FALSE, split = TRUE)
print(topics)
sink()

################################################################################
stm_10_interaction <- stm(documents = docs_twitter,
                vocab = vocab_twitter,
                K = 10,
                prevalence = ~label * s(date_num),
                content = ~label,
                max.em.its = 75,
                data = meta_twitter,
                init.type = "Spectral",
                set.seed(1234),
                verbose = TRUE)

stm_10_gamma_int <- tidy(stm_10_interaction,
                     matrix = "gamma")

stm_10_terms_int <- stm_10_gamma_int |>
  group_by(topic) |>
  summarise(gamma = mean(gamma)) |>
  arrange(desc(gamma)) |>
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_10_terms_int |>
  top_n(20, gamma) |>
  ggplot(aes(topic, gamma)) +
  geom_col(fill = "#00bfc4", color = "black", show.legend = FALSE) +
  labs(title = "Topic distributions", y = "", x = "") +
  coord_flip()

topics_int <- sageLabels(stm_10_interaction, n = 20)
sink("topics_int.txt", append = FALSE, split = TRUE)
print(topics_int)
sink()

################################################################################
stm_simp <- stm(documents = docs_twitter,
                       vocab = vocab_twitter,
                       K = 10,
                       prevalence = ~date_num,
                       content = ~label,
                       max.em.its = 75,
                       data = meta_twitter,
                       init.type = "Spectral",
                       set.seed(1234),
                       verbose = TRUE)

stm_10_gamma_simp <- tidy(stm_simp,
                         matrix = "gamma")

stm_10_terms_simp <- stm_10_gamma_simp |>
  group_by(topic) |>
  summarise(gamma = mean(gamma)) |>
  arrange(desc(gamma)) |>
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_10_terms_simp |>
  top_n(20, gamma) |>
  ggplot(aes(topic, gamma)) +
  geom_col(fill = "#00bfc4", color = "black", show.legend = FALSE) +
  labs(title = "Topic distributions", y = "", x = "") +
  coord_flip()

topics_simp <- sageLabels(stm_simp, n = 20)
sink("topics_simps.txt", append = FALSE, split = TRUE)
print(topics_simp)
sink()

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

saveRDS(stm_K_search, "~/CADS/searchK.RDS")

plot(stm_K_search)
plot_stm_k <- data.frame("K" = K, 
                   "Coherence" = unlist(stm_K_search$results$semcoh),
                   "Exclusivity" = unlist(stm_K_search$results$exclus))

# Reshape to long format
library("reshape2")
plot <- melt(plot_stm_k, id=c("K"))
plot # 50 topics?

################################################################################

stm_50_interaction <- stm(documents = docs_twitter,
                          vocab = vocab_twitter,
                          K = 50,
                          prevalence = ~label * s(date_num),
                          content = ~label,
                          max.em.its = 75,
                          data = meta_twitter,
                          init.type = "Spectral",
                          set.seed(1234),
                          verbose = TRUE)
levels(meta_twitter$label) # misinformation as baseline?
# saveRDS(stm_50_interaction, "~/CADS/Models/STM_50_interaction.RDS")

plot(stm_20_interaction)

###
### Topic distributions and words
###
stm_20_gamma <- tidy(stm_20_interaction,
                     matrix = "gamma")

stm_20_terms <- stm_20_gamma |>
  group_by(topic) |>
  summarise(gamma = mean(gamma)) |>
  arrange(desc(gamma)) |>
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_20_terms |>
  top_n(20, gamma) |>
  ggplot(aes(topic, gamma)) +
  geom_col(fill = "#00bfc4", color = "black", show.legend = FALSE) +
  labs(title = "Topic distributions", y = "", x = "") +
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

topics_20 <- sageLabels(stm_20_interaction, n = 20)
sink("topics_20.txt", append = FALSE, split = TRUE)
print(topics_20)
sink()

################################################################################

stm_15_interaction <- stm(documents = docs_twitter,
                          vocab = vocab_twitter,
                          K = 15,
                          prevalence = ~label * s(date_num),
                          content = ~label,
                          max.em.its = 75,
                          data = meta_twitter,
                          init.type = "Spectral",
                          set.seed(1234),
                          verbose = TRUE)
# saveRDS(stm_15_interaction, "~/CADS/Models/STM_15_interaction.RDS")

###
### Topic distributions and words
###
stm_15_gamma <- tidy(stm_15_interaction,
                     matrix = "gamma")

stm_15_terms <- stm_15_gamma |>
  group_by(topic) |>
  summarise(gamma = mean(gamma)) |>
  arrange(desc(gamma)) |>
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))

stm_15_terms |>
  top_n(20, gamma) |>
  ggplot(aes(topic, gamma)) +
  geom_col(fill = "#00bfc4", color = "black", show.legend = FALSE) +
  labs(title = "Topic distributions", y = "", x = "") +
  coord_flip()

topics_15 <- sageLabels(stm_15_interaction, n = 20)
sink("topics_15.txt", append = FALSE, split = TRUE)
print(topics_15)
sink()

################################################################################

effect20 <- estimateEffect(1:20 ~label * s(date_num),
                           stmobj = stm_20_interaction,
                           metadata = meta_twitter,
                           set.seed(1234),
                           uncertainty = "Global")

effect20_label <- get_effects(effect20, variable = "label",
                      type = "pointestimate")

effect20_label$topic <- reorder(x = effect20_label$topic,
                                effect20_label$proportion)

effect20_label |>
  ggplot(aes(x = topic, y = proportion, color = value, group = value, fill = value)) +
  geom_point(size = 3) +
  scale_x_discrete(name = "Topic", limits = rev(levels(effect20_label$topic))) +
  theme(legend.position = "top")


## after date
effect20_date <- extract.estimateEffect(effect20,
                                        "date_num",
                                        method = "continuous")

effect20_date <- effect20_date |>
  mutate(topic = fct_reorder(as.factor(topic), covariate.value))

ggplot(effect20_date, aes(x = covariate.value, y = estimate, group = topic,color = topic)) +
  geom_line(size = 1) +
  scale_y_continuous() +
  #scale_x_continuous(breaks = seq(from = 12055, to = 18625, by = 730), labels = labels2) +
  theme(legend.position = "top") 
  #labs(title = "Innvandringskritikk", y = "", x = "", color = "Emner:") +
  # scale_color_manual(labels = c("5: Verdibasert uforenlighet", "16: Økonomiske konsekvenser"), values=c("#F8766D", "#00BFC4"))

################################################################################


effect15 <- estimateEffect(1:15 ~label * s(date_num),
                           stmobj = stm_20_interaction,
                           metadata = meta_twitter,
                           set.seed(1234),
                           uncertainty = "Global")

effect15_label <- extract.estimateEffect(effect15,
                                         "label",
                                         method = "pointestimate")

effect15_label$topic <- reorder(x = effect15_label$topic,
                                effect15_label$estimate)

ggplot(effect15_label, aes(x = topic, y = estimate, group = covariate.value, color = covariate.value)) +
  geom_point(size = 3) +
  scale_y_continuous(name = "") +
  scale_x_discrete(name = "Topic", limits = rev(levels(effect15_label$topic))) +
  theme(legend.title = element_blank(),legend.position = "top")


################################################################################
# Change in time between the two groups

# effect20 <- estimateEffect(1:20 ~label * s(date_num),
#                            stmobj = stm_20_interaction,
#                            metadata = meta_twitter,
#                            set.seed(1234),
#                            uncertainty = "Global")

effect_time <- get_effects(effect20, variable = "date_num",
                      type = "continuous",
                      moderator = "label",
                      modval = "misinfo") |>
  bind_rows(get_effects(effect20, variable = "date_num",
                        type = "continuous",
                        moderator = "label",
                        modval = "non.misinfo"))

effect_time |>
  filter(topic==10) |>
  mutate(moderator = as.factor(moderator)) |>
  ggplot(aes(x = value, y = proportion, color = moderator, group = moderator, fill = moderator)) +
  geom_line(size = 1) +
  # scale_x_continuous("", breaks = seq(from = 12055, to = 18625, by = 730), labels = labels2) +
  # scale_y_continuous(limits = c(0.02,0.25)) +
  theme(legend.position = "top") +
  labs(title = "Topic", x = "", y = "", color = "Label", group = "Label", fill = "Label")
