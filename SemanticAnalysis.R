#1. Package installation ----
install.packages("reticulate")
install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("tm")
install.packages("NLP")
install.packages("SentimentAnalysis")



#2. Importing Libraries ----
library("httr")
library("jsonlite")
library("ggplot2")
library("reshape2")
library("dplyr")
library("tm")
library("quanteda") 
library("NLP")
library("reticulate")
library("SentimentAnalysis")




#3. Creating virtual python environment and installing python packages ----
version <- "3.12.3"
install_python(version = version)
virtualenv_create("R-Python", python_version = version)
use_virtualenv("R-Python", required = TRUE)
virtualenv_install("R-Python", packages = c("google-api-python-client","numpy", "pandas", "matplotlib"))




#4. Youtube API Key (just for reference) ----
#Youtube Data API Key
#api_key <- "AIzaSyARJii5J_c5aprgWnJciLofzv5fItNDAIU"
#video_id <- "HlBYdiXdUa8"



#5. Running python file ----
script_path <- "youtube_comments.py"
py_run_file(script_path)
comments_df <- read.csv("youtube_comments.csv")

# Viewing comments dataframe 
View(comments_df)



#6. Sentiment Tagging ----
comments_df$text <- as.character(comments_df$text)

# Perform sentiment analysis
sentiment_results <- analyzeSentiment(comments_df$text)
print(str(sentiment_results))

print(sentiment_results)

comments_df$GI_Positive <- sentiment_results$PositivityGI
comments_df$GI_Negative <- sentiment_results$NegativityGI
comments_df$GI_Net <- comments_df$GI_Positive - comments_df$GI_Negative

# Categorize overall sentiment based on net sentiment score
comments_df$Sentiment <- ifelse(comments_df$GI_Net > 0, "Positive",
                                ifelse(comments_df$GI_Net < 0, "Negative", "Neutral"))

# View the first few rows of the dataframe to inspect the sentiment tags
head(comments_df)

# Visualizing the Sentiment Distribution
ggplot(comments_df, aes(x = GI_Net)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Net Sentiment Scores",
       x = "Net Sentiment Score",
       y = "Count") +
  theme_minimal()
ggsave(filename = "Net_Sentiment_Distribution.png")

# distribution of sentiment score by category with the scores, shows us the intensity of the sentiment
ggplot(comments_df, aes(x = GI_Net, fill = Sentiment)) +
  geom_histogram(bins = 30, alpha = 0.6) +
  facet_wrap(~Sentiment) +
  labs(title = "Sentiment Score Distribution by Category",
       x = "Net Sentiment Score", y = "Count")
ggsave(filename = "Sentiment_Score_Distribution.png")




#7. Data Pre-processing ----
# Convert text to lowercase
comments_df$clean_text <- tolower(comments_df$text)

# Remove punctuation
comments_df$clean_text <- gsub("[[:punct:]]+", " ", comments_df$clean_text)

# Remove numbers
comments_df$clean_text <- gsub("[[:digit:]]+", " ", comments_df$clean_text)

# Remove excessive whitespace
comments_df$clean_text <- gsub("[ \t]{2,}", " ", comments_df$clean_text)
comments_df$clean_text <- trimws(comments_df$clean_text)

# # Emojis to convert to text
# emoji_to_text <- list(
#   "😊" = "happy_face",
#   "😢" = "sad_face",
#   "❤️" = "heart",
#   "😂" = "laughing",
#   "🤣" = "sideways laughing",
#   "😒" = "unamused",
#   "😉" = "winking",
#   "😍" = "heart_eyes",
#   "😔" = "pensive",
#   "😘" = "kissing_heart",
#   "😭" = "crying_loudly",
#   "🥰" = "smiling_face_with_hearts",
#   "😡" = "angry_face",
#   "🤔" = "thinking_face",
#   "😎" = "cool_sunglasses",
#   "😱" = "screaming_in_fear",
#   "😏" = "smirking_face",
#   "🙄" = "face_with_rolling_eyes",
#   "😇" = "smiling_face_with_halo",
#   "🥳" = "partying_face",
#   "😜" = "winking_face_with_tongue",
#   "🤗" = "hugging_face",
#   "🤐" = "zipper_mouth_face",
#   "😴" = "sleeping_face",
#   "🤑" = "money_mouth_face",
#   "🤢" = "nauseated_face",
#   "🙃" = "upside_down_face",
#   "🤮" = "vomiting_face",
#   "🤧" = "sneezing_face",
#   "😷" = "face_with_medical_mask",
#   "🤒" = "face_with_thermometer",
#   "👍" = "thumbs_up",
#   "👎" = "thumbs_down",
#   "🎉" = "celebration",
#   "💔" = "broken_heart",
#   "🔥" = "fire",
#   "🤩" = "star_struck",
#   "🤬" = "face_with_symbols_on_mouth",
#   "😰" = "anxious_face_with_sweat",
#   "😥" = "sad_but_relieved_face",
#   "🤯" = "exploding_head",
#   "😳" = "flushed_face",
#   "👏" = "clapping_hands",
#   "🙌" = "raising_hands",
#   "😤" = "face_with_steam_from_nose",
#   "😵" = "dizzy_face",
#   "🥵" = "hot_face",
#   "🥶" = "cold_face",
#   "🥺" = "pleading_face",
#   "🤟" = "love_you_gesture",
#   "👌" = "ok_hand",
#   "🚶" = "person_walking",
#   "🏃" = "person_running",
#   "💃" = "woman_dancing",
#   "🕺" = "man_dancing"
# )
# 
# 
# # Converting the emojis to text, so that a wider range of emotions can be captured
# replace_emoji_with_text <- function(text, mapping) {
#   for (emoji in names(mapping)) {
#     text <- gsub(emoji, mapping[[emoji]], text)
#   }
#   return(text)
# }

#comments_df$clean_text <- sapply(comments_df$clean_text, replace_emoji_with_text, mapping = emoji_to_text)

#Removing the emojis
comments_df$clean_text <- iconv(comments_df$clean_text, from = "UTF-8", to = "ASCII", sub = "")

head(comments_df$clean_text)
View(comments_df)

yt_stopwords <- c("a", "about", "above", "after", "again", "against", "all", "am", "an", 
                  "and", "any", "are", "as", "at", "be", "because", "been", "before", 
                  "being", "below", "between", "both", "but", "by", "could", "did", 
                  "do", "does", "doing", "down", "during", "each", "few", "for", "from", 
                  "further", "had", "has", "have", "having", "he", "her", "here", 
                  "hers", "herself", "him", "himself", "his", "how", "i", "if", "in", 
                  "into", "is", "it", "its", "itself", "just", "me", "more", "most", 
                  "my", "myself", "no", "nor", "not", "now", "of", "off", "on", "once", 
                  "only", "or", "other", "our", "ours", "ourselves", "out", "over", 
                  "own", "same", "she", "should", "so", "some", "such", "than", "that", 
                  "the", "their", "theirs", "them", "themselves", "then", "there", 
                  "these", "they", "this", "those", "through", "to", "too", "under", 
                  "until", "up", "very", "was", "we", "were", "what", "when", "where", 
                  "which", "while", "who", "whom", "why", "will", "with", "you", "your", 
                  "yours", "yourself", "yourselves")

# Remove stopwords
comments_df$clean_text <- sapply(comments_df$clean_text, function(x) {
  words <- unlist(strsplit(x, "\\s+"))
  words <- words[!words %in% yt_stopwords]
  paste(words, collapse = " ")
})

# Removing Rows with NA values
comments_df <- na.omit(comments_df)

cleaned_comments_df <- comments_df[1:3000, -c(2, 5, 6, 7, 8)]

View(cleaned_comments_df)



#8. Corpus and matrix creation ----
positive_corpus <- Corpus(VectorSource(cleaned_comments_df$clean_text[cleaned_comments_df$Sentiment == "Positive"]))
negative_corpus <- Corpus(VectorSource(cleaned_comments_df$clean_text[cleaned_comments_df$Sentiment == "Negative"]))
neutral_corpus <- Corpus(VectorSource(cleaned_comments_df$clean_text[cleaned_comments_df$Sentiment == "Neutral"]))

# Creating a corpus that includes all documents
all_corpus <- Corpus(VectorSource(cleaned_comments_df$clean_text))


# Create Term-Document Matrices
create_tdm <- function(corpus) {
  # Term Frequency Matrix
  tf <- TermDocumentMatrix(corpus, control = list(weighting = weightTf))
  
  # TF-IDF Matrix
  tfidf <- TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
  
  return(list(tf = tf, tfidf = tfidf))
}

positive_tdm <- create_tdm(positive_corpus)
negative_tdm <- create_tdm(negative_corpus)
neutral_tdm <- create_tdm(neutral_corpus)
all_tdm <- create_tdm(all_corpus)

print(as.matrix(positive_tdm$tf)[1:10, 1:3])
print(as.matrix(negative_tdm$tf)[1:10, 1:3])
print(as.matrix(neutral_tdm$tf)[1:10, 1:3])
print(as.matrix(all_tdm$tf)[1:10, 1:3])

create_dtm <- function(corpus) {
  # Term Frequency Matrix
  tf <- DocumentTermMatrix(corpus, control = list(weighting = weightTf))
  
  # TF-IDF Matrix
  tfidf <- DocumentTermMatrix(corpus, control = list(weighting = weightTfIdf))
  
  return(list(tf = tf, tfidf = tfidf))
}

positive_dtm <- create_dtm(positive_corpus)
negative_dtm <- create_dtm(negative_corpus)
neutral_dtm <- create_dtm(neutral_corpus)
all_dtm <- create_dtm(all_corpus)

print(as.matrix(positive_dtm$tf)[1:10, 1:3])
print(as.matrix(negative_dtm$tf)[1:10, 1:3])
print(as.matrix(neutral_dtm$tf)[1:10, 1:3])
print(as.matrix(all_dtm$tf)[1:10, 1:3])

# Analyze Term Distribution
# Function to sum term frequencies and sort them
summarize_terms <- function(tdm) {
  term_matrix <- as.matrix(tdm$tf)
  term_freq <- rowSums(term_matrix)
  sort(term_freq, decreasing = TRUE)
}

# Applying the function to each sentiment
positive_term_freq <- summarize_terms(positive_tdm)
negative_term_freq <- summarize_terms(negative_tdm)
neutral_term_freq <- summarize_terms(neutral_tdm)
all_term_freq <- summarize_terms(all_tdm)

# Displaying the top 10 terms for each sentiment
head(positive_term_freq, 10)
head(negative_term_freq, 10)
head(neutral_term_freq, 10)
head(all_term_freq, 10)

# Function to plot term frequencies
plot_term_freq <- function(term_freq, title) {
  top_terms <- head(term_freq, 10)
  barplot(top_terms, las = 2, main = title, col = "blue", xlab = "Terms", ylab = "Frequency")
}

# Plotting the top 10 terms for each sentiment
png(filename = "Top 10 Terms in Positive Comments.png")
plot_term_freq(positive_term_freq, "Top 10 Terms in Positive Comments")
dev.off()

png(filename = "Top 10 Terms in Negative Comments.png")
plot_term_freq(negative_term_freq, "Top 10 Terms in Negative Comments")
dev.off()

png(filename = "Top 10 Terms in Neutral Comments.png")
plot_term_freq(neutral_term_freq, "Top 10 Terms in Neutral Comments")
dev.off()

png(filename = "Top 10 Terms in All Comments.png")
plot_term_freq(all_term_freq, "Top 10 Terms in All Comments")
dev.off()

# Function to summarize TF-IDF scores
summarize_tfidf <- function(tdm) {
  tfidf_matrix <- as.matrix(tdm$tfidf)
  max_tfidf <- apply(tfidf_matrix, 1, max)  # Get the maximum TF-IDF score for each term
  sort(max_tfidf, decreasing = TRUE)
}

# Applying the function to each sentiment
positive_tfidf <- summarize_tfidf(positive_tdm)
negative_tfidf <- summarize_tfidf(negative_tdm)
neutral_tfidf <- summarize_tfidf(neutral_tdm)
all_tfidf <- summarize_tfidf(all_tdm)

# Displaying the terms with the highest TF-IDF scores
head(positive_tfidf, 10)
head(negative_tfidf, 10)
head(neutral_tfidf, 10)
head(all_tfidf, 10)

View(cleaned_comments_df)






# LSA ----
install.packages("lsa")
library("lsa")
#6.1 Create Semantic Space from your corpus
all_dtm <- create_dtm(all_corpus)
all_dtm<-TermDocumentMatrix(all_corpus, 
                            control = list(bounds = list(global = c(2, Inf))))
inspect(all_dtm)
#dim=2
lsaSpace<-lsa(all_dtm,dims=2)
lsaSpace
#dim=5
lsaSpace<-lsa(all_dtm,dims=5)
lsaSpace

TermDocDataMatrix <- as.textmatrix(lsaSpace)
TermDocDataMatrix

#get term/feature 
termSpace<-lsaSpace$tk
lables=rownames(termSpace)
termSpace

#get document/feature 
docSpace<-lsaSpace$dk
lablesdoc=rownames(docSpace) #what is your observation?
docSpace

# Create data frame for plotting
doc_space_df <- data.frame(
  x = docSpace_reduced[, 1], # Extracting the first dimension of docSpace
  y = docSpace_reduced[, 2], # Extracting the second dimension of docSpace
  z = docSpace_reduced[, 3], # Extracting the third dimension of docSpace
  w = docSpace_reduced[, 4]  # Extracting the fourth dimension of docSpace
)

# Create 3D scatter plot
scatterplot3d(
  doc_space_df$x,
  doc_space_df$y,
  doc_space_df$z,
  color = "blue",
  pch = 16,
  xlab = "Dimension 1",
  ylab = "Dimension 2",
  zlab = "Dimension 3",
  main = "Document Space Visualization (Reduced to 70 Documents)"
)

# LDA ----
set.seed(123)
options(max.print = 100)
# 1. create Document Term Matrix

positive_dtm<-DocumentTermMatrix(positive_corpus, 
                                 control = list(bounds = list(global = c(2, Inf))))
inspect(positive_dtm[1:20, ])
positive_dtm_tfidf<-DocumentTermMatrix(positive_corpus, 
                                       control = list(weighting = function(x)
                                         weightTfIdf(x, normalize =TRUE)))
negative_dtm<-DocumentTermMatrix(negative_corpus, 
                                 control = list(bounds = list(global = c(2, Inf))))
inspect(negative_dtm[1:20, ])
negative_dtm_tfidf<-DocumentTermMatrix(negative_corpus, 
                                       control = list(weighting = function(x)
                                         weightTfIdf(x, normalize =TRUE)))
neutral_dtm<-DocumentTermMatrix(neutral_corpus, 
                                control = list(bounds = list(global = c(2, Inf))))
inspect(neutral_dtm[1:20, ])
neutral_dtm_tfidf<-DocumentTermMatrix(all_corpus, 
                                      control = list(weighting = function(x)
                                        weightTfIdf(x, normalize =TRUE)))
all_dtm<-DocumentTermMatrix(all_corpus, 
                            control = list(bounds = list(global = c(2, Inf))))
inspect(all_dtm[1:20, ])
all_dtm_tfidf<-DocumentTermMatrix(all_corpus, 
                                  control = list(weighting = function(x)
                                    weightTfIdf(x, normalize =TRUE)))

# 2. Term Distribution analysis

#converting matrix to dataframe for analysis
Positive_DocumentTermDataFrame <- as.data.frame(as.matrix(positive_dtm))
Negative_DocumentTermDataFrame <- as.data.frame(as.matrix(negative_dtm))
Neutral_DocumentTermDataFrame <- as.data.frame(as.matrix(neutral_dtm))
All_DocumentTermDataFrame <- as.data.frame(as.matrix(all_dtm))

#get list of all terms
names(Positive_DocumentTermDataFrame)
names(Negative_DocumentTermDataFrame)
names(Neutral_DocumentTermDataFrame)
names(All_DocumentTermDataFrame)

# Get Most Frequent Words
#get top words in corpus
Positive_WordFreq <- colSums(Positive_DocumentTermDataFrame)
View(Positive_WordFreq)
Negative_WordFreq <- colSums(Negative_DocumentTermDataFrame)
View(Negative_WordFreq)
Neutral_WordFreq <- colSums(Neutral_DocumentTermDataFrame)
View(Neutral_WordFreq)
All_WordFreq <- colSums(All_DocumentTermDataFrame)
View(All_WordFreq)

#convert it to dataframe
Positive_WordFreq_df <- data.frame(Term = names(Positive_WordFreq), Frequency = Positive_WordFreq, row.names = NULL)
View(Positive_WordFreq_df)

Positive_top_words <-  Positive_WordFreq_df %>%
  top_n(20, Frequency) %>%
  arrange(desc(Frequency))
View(Positive_top_words)

Negative_WordFreq_df <- data.frame(Term = names(Negative_WordFreq), Frequency = Negative_WordFreq, row.names = NULL)
View(Negative_WordFreq_df)

Negative_top_words <-  Negative_WordFreq_df %>%
  top_n(20, Frequency) %>%
  arrange(desc(Frequency))
View(Negative_top_words)

Neutral_WordFreq_df <- data.frame(Term = names(Neutral_WordFreq), Frequency = Neutral_WordFreq, row.names = NULL)
View(Neutral_WordFreq_df)

Neutral_top_words <-  Neutral_WordFreq_df %>%
  top_n(20, Frequency) %>%
  arrange(desc(Frequency))
View(Neutral_top_words)

All_WordFreq_df <- data.frame(Term = names(All_WordFreq), Frequency = All_WordFreq, row.names = NULL)
View(All_WordFreq_df)

All_top_words <-  All_WordFreq_df %>%
  top_n(20, Frequency) %>%
  arrange(desc(Frequency))
View(All_top_words)

# 3. WordCloud

install.packages("wordcloud")
# https://www.geeksforgeeks.org/generating-word-cloud-in-r-programming/
library(wordcloud)
All_wordcloud <- wordcloud(All_top_words$Term,All_top_words$Frequency)
Positive_wordcloud <- wordcloud(Positive_top_words$Term,Positive_top_words$Frequency)
Negative_wordcloud <- wordcloud(Negative_top_words$Term,Negative_top_words$Frequency)
Neutral_wordcloud <- wordcloud(Neutral_top_words$Term,Neutral_top_words$Frequency)

# 4. Latent Dirichlet Allocation (LDA)

library("topicmodels")
library("ldatuning")

# 4.1 -	lda for each sentiment and for the entire corpus
# Entire corpus
all_dtm_matrix <- as.matrix(all_dtm)
row_sums <- rowSums(all_dtm_matrix)
non_zero_rows <- row_sums > 0
all_dtm_filtered <- all_dtm[non_zero_rows, ]

All_ldaResult <- LDA(all_dtm_filtered, 4, method = "Gibbs",
                     control = list(nstart = 4, seed = list(1, 2, 3, 4), best = TRUE, iter = 10))

All_ldaResult.terms <- as.matrix(terms(All_ldaResult, 5))
All_ldaResult.terms

# Positive Sentiment
positive_dtm_matrix <- as.matrix(positive_dtm)
row_sums <- rowSums(positive_dtm_matrix)
non_zero_rows <- row_sums > 0
positive_dtm_filtered <- positive_dtm[non_zero_rows, ]

Positive_ldaResult <- LDA(positive_dtm_filtered, 4, method = "Gibbs",
                          control = list(nstart = 4, seed = list(1, 2, 3, 4), best = TRUE, iter = 10))


Positive_ldaResult.terms <- as.matrix(terms(Positive_ldaResult, 5))
Positive_ldaResult.terms

# Negative Sentiment
negative_dtm_matrix <- as.matrix(negative_dtm)
row_sums <- rowSums(negative_dtm_matrix)
non_zero_rows <- row_sums > 0
negative_dtm_filtered <- negative_dtm[non_zero_rows, ]

Negative_ldaResult <- LDA(negative_dtm_filtered, 4, method = "Gibbs",
                          control = list(nstart = 4, seed = list(1, 2, 3, 4), best = TRUE, iter = 10))

Negative_ldaResult.terms <- as.matrix(terms(Negative_ldaResult, 5))
Negative_ldaResult.terms

# Neutral Sentiment
neutral_dtm_matrix <- as.matrix(neutral_dtm)
row_sums <- rowSums(neutral_dtm_matrix)
non_zero_rows <- row_sums > 0
neutral_dtm_filtered <- neutral_dtm[non_zero_rows, ]

Neutral_ldaResult <- LDA(neutral_dtm_filtered, 4, method = "Gibbs",
                         control = list(nstart = 4, seed = list(1, 2, 3, 4), best = TRUE, iter = 10))


Neutral_ldaResult.terms <- as.matrix(terms(Neutral_ldaResult, 5))
Neutral_ldaResult.terms

# 4.2 Topic Word Distribution

# Entire Corpus
library("tidytext")
All_lda.topics <- tidy(All_ldaResult,matrix = "beta")
All_top_terms <- All_lda.topics %>%
  group_by(topic) %>%
  top_n(5,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)

All_plot_topic <- All_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
All_plot_topic

# Postive Sentiment
Positive_lda.topics <- tidy(Positive_ldaResult,matrix = "beta")
Positive_top_terms <- Positive_lda.topics %>%
  group_by(topic) %>%
  top_n(5,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)

Positive_plot_topic <- Positive_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
Positive_plot_topic

# Negative Sentiment
Negative_lda.topics <- tidy(Negative_ldaResult,matrix = "beta")
Negative_top_terms <- Negative_lda.topics %>%
  group_by(topic) %>%
  top_n(5,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)

Negative_plot_topic <- Negative_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
Negative_plot_topic

# Neutral Sentiment
Neutral_lda.topics <- tidy(Neutral_ldaResult,matrix = "beta")
Neutral_top_terms <- Neutral_lda.topics %>%
  group_by(topic) %>%
  top_n(5,beta) %>% 
  ungroup() %>%
  arrange(topic,-beta)

Neutral_plot_topic <- Neutral_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
Neutral_plot_topic

# 4.3 Document Word Distribution

# Entire Corpus
All_lda.document <- tidy(All_ldaResult, matrix = "gamma")
All_lda.document

All_top_terms <- All_lda.document %>%
  group_by(topic) %>%
  top_n(5,gamma) %>% 
  ungroup() %>%
  arrange(topic,-gamma)

All_plot_topic <- All_top_terms %>%
  mutate(term = reorder_within(document, gamma, topic)) %>%
  ggplot(aes(document, gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
All_plot_topic

# Positive Sentiment
Positive_lda.document <- tidy(Positive_ldaResult, matrix = "gamma")
Positive_lda.document

Positive_top_terms <- Positive_lda.document %>%
  group_by(topic) %>%
  top_n(5,gamma) %>% 
  ungroup() %>%
  arrange(topic,-gamma)

Positive_plot_topic <- Positive_top_terms %>%
  mutate(term = reorder_within(document, gamma, topic)) %>%
  ggplot(aes(document, gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
Positive_plot_topic

# Negative Sentiment
Negative_lda.document <- tidy(Negative_ldaResult, matrix = "gamma")
Negative_lda.document

Negative_top_terms <- Negative_lda.document %>%
  group_by(topic) %>%
  top_n(5,gamma) %>% 
  ungroup() %>%
  arrange(topic,-gamma)

Negative_plot_topic <- Negative_top_terms %>%
  mutate(term = reorder_within(document, gamma, topic)) %>%
  ggplot(aes(document, gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
Negative_plot_topic

# Neutral Sentiment
Neutral_lda.document <- tidy(Neutral_ldaResult, matrix = "gamma")
Neutral_lda.document

Neutral_top_terms <- Neutral_lda.document %>%
  group_by(topic) %>%
  top_n(5,gamma) %>% 
  ungroup() %>%
  arrange(topic,-gamma)

Neutral_plot_topic <- Neutral_top_terms %>%
  mutate(term = reorder_within(document, gamma, topic)) %>%
  ggplot(aes(document, gamma, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()
Neutral_plot_topic

# 4.4 LDA Interactive Visualization

topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}
library("LDAvis")
serVis(topicmodels2LDAvis(All_ldaResult))
serVis(topicmodels2LDAvis(Positive_ldaResult))
serVis(topicmodels2LDAvis(Negative_ldaResult))
serVis(topicmodels2LDAvis(Neutral_ldaResult))


library(wordcloud)

# Create a word cloud for the entire corpus--- wordcloud
word_freq_all <- sort(colSums(as.matrix(all_tdm$tf)), decreasing = TRUE)
wordcloud(words = names(word_freq_all), freq = word_freq_all, min.freq = 5, max.words = 100, colors = brewer.pal(8, "Dark2"))



# Converting to 2-Gram terms Representation---

library(tidytext)
library(dplyr)
library(ggplot2)

# Create a 2-gram Term-Document Matrix
comments_bigrams <- comments_df %>%
  unnest_tokens(bigram, clean_text, token = "ngrams", n = 2)

bigram_counts <- comments_bigrams %>%
  count(bigram, sort = TRUE)

top_bigrams <- bigram_counts %>% top_n(20, n)

ggplot(top_bigrams, aes(x = reorder(bigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "powderblue") +
  coord_flip() +
  labs(title = "Top 20 Comments", x = "2-GRAM REPRESENTATION ", y = "Frequency") +
  theme_minimal()

# Correlated Topic Model  CTM --- 

library(topicmodels)
library(tidyverse)

dtm <- as.DocumentTermMatrix(all_tdm$tf)

dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]
ctm_model <- CTM(dtm, k = 5, control = list(seed = 1234))
summary(ctm_model)

ctm_terms <- terms(ctm_model, 10)
print(ctm_terms)

ctm_terms_df <- as.data.frame(ctm_terms)
top_terms_ctm <- ctm_terms_df %>%
  rownames_to_column(var = "term") %>%
  pivot_longer(cols = -term, names_to = "topic", values_to = "term_value")
ggplot(top_terms_ctm, aes(x = reorder(term_value, term), fill = topic)) +
  geom_bar(stat = "count") +
  coord_flip() +
  labs(title = "Top Terms in CTM Topics", x = "Terms", y = "Frequency") +
  theme_minimal()


#---LTM 
install.packages("ltm")
library(ltm) 
binary_dtm <- as.matrix(dtm)
binary_dtm[binary_dtm > 0] <- 1

ltm_model <- rasch(binary_dtm)
summary(ltm_model)
ltm_terms <- coef(ltm_model)
top_items <- head(sort(ltm_terms[, 1], decreasing = TRUE), 20)

# Plot top items
barplot(top_items, las = 2, main = "Top Terms in LTM (Rasch Model)", col = "slateblue")


