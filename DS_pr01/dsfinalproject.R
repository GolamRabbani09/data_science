# --- 1. Install and Load Libraries ---
# Ensure all necessary R packages are installed and loaded.
# Run this section once on a new machine to set up the environment.
#install.packages(c("rvest", "httr", "dplyr", "openxlsx", "tidyverse",
 #                  "textstem", "SnowballC", "stopwords", "readxl", "hunspell",
  #                 "tm", "topicmodels", "tidytext", "ggplot2", "reshape2", # Added reshape2
  #                 "wordcloud", "RColorBrewer")) # Added for word cloud

library(rvest)       # For web scraping
library(httr)        # For HTTP requests (often used with rvest)
library(dplyr)       # For data manipulation
library(openxlsx)    # For reading/writing Excel files
library(tidyverse)   # A collection of R packages including dplyr, readr, ggplot2, etc.
library(textstem)     # For lemmatization
library(SnowballC)   # For stemming
library(stopwords)   # For managing stopwords
library(readxl)      # For reading Excel files
library(hunspell)    # Spell checking (though not directly used in the current cleaning steps)
library(tm)          # For text mining (corpus, DTM)
library(topicmodels) # For LDA topic modeling
library(tidytext)    # For tidy text analysis with topic models
library(ggplot2)     # For visualizations
library(reshape2)    # Often a dependency for other text/viz packages
library(wordcloud)   # For generating word clouds
library(RColorBrewer) # For color palettes in visualizations

# --- Project Setup: Directories ---
# Create necessary directories if they don't already exist to store data and outputs.
if (!dir.exists("data")) {
  dir.create("data")
}
if (!dir.exists("output")) {
  dir.create("output")
}
if (!dir.exists("plots")) {
  dir.create("plots")
}
cat("Project directories 'data', 'output', 'plots' checked/created.\n")


# --- 2. Web Scraping ---
# Objective: Extract news content from a news portal and save it.

# IMPORTANT: For a Bengali news portal, you'll need to update:
# 1. base_url to the Bengali news site's main URL.
# 2. category_map with actual paths on that site.
# 3. html_nodes selectors in scrape_article_details might need adjustment.

base_url <- "https://edition.cnn.com" # Example: CNN.com (replace for Bengali portal)

category_map <- list(
  "Style" = c(
    "/style/arts", "/style/design", "/style/fashion",
    "/style/architecture", "/style/luxury", "/style/beauty"
  ),
  "World" = c(
    "/world/africa", "/world/americas", "/world/asia", "/world/australia",
    "/world/europe", "/world/middle-east"
  ),
  "Sports" = c(
    "/sport/football", "/sport/tennis", "/sport/golf", "/sport/motorsport",
    "/sport/paris-olympics-2024", "/sport/climbing"
  ),
  "Politics" = c(
    "/politics/president-donald-trump-47", "/politics/fact-check",
    "/polling", "/election/2025"
  ),
  "Business" = c(
    "/markets", "/business/tech", "/business/media", "/business/financial-calculators"
  )
)

# Function to scrape article links from specified category paths.
scrape_category <- function(category_paths, max_needed = 3, max_pages = 1) {
  links_collected <- character()
  for (path in category_paths) {
    for (page in 1:max_pages) {
      url <- paste0(base_url, path, "?page=", page)
      res <- tryCatch(read_html(url), error = function(e) {
        cat("Error loading page:", e$message, "\n")
        return(NULL)
      })
      if (is.null(res)) next
      
      links <- res %>% html_nodes("a") %>% html_attr("href") %>% na.omit()
      filtered <- links[grepl("^/\\d{4}/\\d{2}/\\d{2}/", links)]
      full_links <- paste0(base_url, filtered)
      
      links_collected <- unique(c(links_collected, full_links))
      if (length(links_collected) >= max_needed) break
      Sys.sleep(1) # Be polite when scraping
    }
    if (length(links_collected) >= max_needed) break
  }
  return(head(links_collected, max_needed))
}

# Function to scrape detailed information from a single article URL.
scrape_article_details <- function(url) {
  page <- tryCatch(read_html(url), error = function(e) {
    cat("Failed to load article:", e$message, "\n")
    return(NULL)
  })
  if (is.null(page)) return(NULL)
  
  title <- page %>% html_node("h1") %>% html_text(trim = TRUE)
  # Assuming article text is within <p> tags inside an <article> element
  article_text <- page %>% html_nodes("article p") %>% html_text(trim = TRUE) %>% paste(collapse = " ")
  date <- page %>% html_node("meta[name='pubdate'], meta[property='article:published_time']") %>% html_attr("content")
  if (is.na(date)) {
    date <- page %>% html_node("time") %>% html_attr("datetime")
  }
  
  return(data.frame(
    URL = url,
    Article_Title = title, # Renamed to Article_Title for clarity with project spec
    Article_Text = article_text, # Renamed for clarity with project spec
    Date = date,
    Source = "CNN", # Change if using a different portal
    stringsAsFactors = FALSE
  ))
}

# Execute scraping
all_articles <- list()
for (main_cat in names(category_map)) {
  urls <- scrape_category(category_map[[main_cat]], max_needed = 5, max_pages = 1) # Increased max_needed for more data
  details <- lapply(urls, scrape_article_details)
  df_scraped <- bind_rows(details)
  df_scraped$Main_Category <- main_cat
  all_articles[[main_cat]] <- df_scraped
}
final_df_raw <- bind_rows(all_articles) # Renamed to final_df_raw for clarity

# Save raw scraped content to CSV
write_csv(final_df_raw, "data/raw_scraped_articles.csv")
cat("Raw scraped data saved to data/raw_scraped_articles.csv\n")


# --- 3. Text Preprocessing ---
# Objective: Clean the scraped text content.

# Load the raw scraped data for preprocessing
df_preprocess <- read_csv("data/raw_scraped_articles.csv")
cat("Loaded raw data for preprocessing from data/raw_scraped_articles.csv\n")

target_col_text <- "Article_Text" # Column containing the text to be cleaned

# Define custom stop words (add Bengali stop words here if applicable)
# For English, you can extend the default list.
custom_stopwords <- c(stopwords("en"), "cnn", "said", "just", "will", "can") # Example custom list

# Define contractions (for English)
contractions <- c(
  "can't" = "cannot", "won't" = "will not", "i'm" = "i am",
  "he's" = "he is", "she's" = "she is", "it's" = "it is",
  "they're" = "they are", "we're" = "we are",
  "didn't" = "did not", "doesn't" = "does not",
  "don't" = "do not", "isn't" = "is not",
  "aren't" = "are not", "weren't" = "were not", "hasn't" = "has not",
  "haven't" = "have not", "hadn't" = "had not", "couldn't" = "could not",
  "wouldn't" = "would not", "shouldn't" = "should not", "mustn't" = "must not",
  "i've" = "i have", "you've" = "you have", "we've" = "we have",
  "they've" = "they have", "i'd" = "i would", "you'd" = "you would",
  "he'd" = "he would", "she'd" = "she would", "we'd" = "we would",
  "they'd" = "they would", "i'll" = "i will", "you'll" = "you will",
  "he'll" = "he will", "she'll" = "she will", "we'll" = "we will",
  "they'll" = "they will"
)

# Text cleaning function
clean_text <- function(text) {
  if (is.na(text) || text == "") return("")
  
  text <- tolower(text) # Convert to lowercase (if English)
  for (c in names(contractions)) { # Expand contractions
    text <- str_replace_all(text, fixed(c), contractions[[c]])
  }
  text <- str_replace_all(text, "http\\S+|www\\S+", " ") # Remove URLs
  text <- str_replace_all(text, "[[:punct:]]", " ") # Remove punctuation
  text <- str_replace_all(text, "[0-9]", " ") # Remove numbers
  text <- str_replace_all(text, "[^\x01-\x7F]", "") # Remove non-ASCII
  text <- str_replace_all(text, "\\s+", " ") %>% str_trim() # Clean spaces
  
  words <- str_split(text, "\\s+", simplify = TRUE) %>% as.vector() # Tokenize
  words <- words[!(words %in% custom_stopwords)] # Remove custom stop words
  
  # Apply lemmatization and stemming (primarily for English)
  words <- lemmatize_words(words)
  words <- wordStem(words, language = "en") # 'en' for English; adjust if needed
  
  cleaned <- str_c(words, collapse = " ")
  return(cleaned)
}

safe_clean_text <- safely(clean_text)

# Apply cleaning to the Article_Text column
df_processed <- df_preprocess %>%
  mutate(
    cleaned_text = map_chr(
      .data[[target_col_text]],
      ~ {
        out <- safe_clean_text(.x)
        if (!is.null(out$result)) out$result else ""
      }
    )
  )

# Save preprocessed data
write_csv(df_processed, "output/preprocessed_articles.csv")
cat("Preprocessed data saved to output/preprocessed_articles.csv\n")


# --- 4. Exploratory Text Analysis ---
# Objective: Visualize word frequencies (word cloud, bar chart).

df_eda <- read_csv("output/preprocessed_articles.csv")
cat("Loaded preprocessed data for EDA.\n")

# Filter out empty or NA cleaned texts for analysis
df_eda <- df_eda %>% filter(!is.na(cleaned_text) & cleaned_text != "")

# Create a tidy tokenized dataset
tidy_words <- df_eda %>%
  unnest_tokens(word, cleaned_text) %>%
  filter(nchar(word) > 2) # Remove very short words that might be artifacts

# Calculate word frequencies
word_freqs <- tidy_words %>%
  count(word, sort = TRUE)

# Generate a Word Cloud
set.seed(123) # for reproducibility of wordcloud layout
png("plots/wordcloud.png", width=800, height=800, res=100)
wordcloud(words = word_freqs$word,
          freq = word_freqs$n,
          min.freq = 5, # Minimum frequency for a word to be plotted
          max.words = 100, # Maximum number of words to plot
          random.order = FALSE,
          colors = brewer.pal(8, "Dark2"))
dev.off()
cat("Word cloud saved to plots/wordcloud.png\n")

# Create a bar chart of the top 20 most common words
top_20_words <- word_freqs %>%
  head(20)

plot_top_20_words <- ggplot(top_20_words, aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() + # Flip coordinates to make horizontal bars
  labs(title = "Top 20 Most Common Words", x = "Word", y = "Frequency") +
  theme_minimal()
ggsave("plots/top_20_words_bar_chart.png", plot_top_20_words, width = 8, height = 6)
cat("Top 20 words bar chart saved to plots/top_20_words_bar_chart.png\n")

# Comment on patterns: (This will be printed to console, you'd analyze the plots)
cat("\n--- Exploratory Text Analysis Comments ---\n")
cat("Based on the word cloud and top 20 words bar chart, observe which words appear most frequently.\n")
cat("Common patterns might include: terms related to global events, specific political figures, or trending topics\n")
cat("relevant to the news categories scraped. If scraping from a Bengali portal, expect terms related to Bengali culture,\n")
cat("politics, or common phrases. Highly frequent words might indicate the dominant themes in the collected articles.\n")
cat("-------------------------------------------\n")


# --- 5. Topic Modeling ---
# Objective: Uncover underlying topics using LDA.

df_tm <- read_csv("output/preprocessed_articles.csv")
cat("Loaded preprocessed data for Topic Modeling.\n")

# Ensure 'cleaned_text' is valid for DTM construction
df_tm <- df_tm %>% filter(!is.na(cleaned_text) & cleaned_text != "")

# Assign unique document IDs
df_tm$document_id <- as.character(1:nrow(df_tm))

# Create corpus and DTM
corpus_tm <- VCorpus(VectorSource(df_tm$cleaned_text))
dtm <- DocumentTermMatrix(corpus_tm, control = list(bounds = list(global = c(2, Inf))))

# Remove empty rows from DTM and corresponding rows from dataframe
row_totals_tm <- apply(dtm, 1, sum)
dtm <- dtm[row_totals_tm > 0, ]
df_tm <- df_tm[row_totals_tm > 0, ]
cat(paste0("Number of documents for LDA after DTM filtering: ", nrow(dtm), "\n"))


# Apply Latent Dirichlet Allocation (LDA)
num_topics <- 5 # Identify 3-5 main topics as per project spec
cat(paste0("Running LDA model with ", num_topics, " topics...\n"))
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))
cat("LDA model trained successfully.\n")

# Visualize: Top words per topic using bar plots (Beta distribution)
topic_words <- tidy(lda_model, matrix = "beta")

top_terms_lda <- topic_words %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

plot_topic_words <- top_terms_lda %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  scale_y_reordered() +
  labs(title = "Top 10 Words per Topic (LDA Beta)", x = "Beta (Probability)", y = "Term") +
  theme_minimal()
ggsave("plots/lda_top_words_bar_plot.png", plot_topic_words, width = 10, height = 7)
cat("Top words per topic plot saved to plots/lda_top_words_bar_plot.png\n")

# Visualize: Document-topic distribution (Gamma distribution)
doc_topics <- tidy(lda_model, matrix = "gamma")

# For simplicity, let's visualize the average topic proportion across all documents
avg_doc_topics <- doc_topics %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  ungroup() %>%
  mutate(topic_label = paste("Topic", topic))

# Stacked bar plot for average topic distribution
plot_avg_doc_topics <- ggplot(avg_doc_topics, aes(x = "", y = gamma, fill = topic_label)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + # Creates a pie chart effect if you want a pie.
  labs(title = "Average Document-Topic Distribution (LDA Gamma)",
       fill = "Topic") +
  theme_void() + # Minimal theme for pie/stacked bar
  theme(plot.title = element_text(hjust = 0.5))
ggsave("plots/lda_doc_topic_distribution.png", plot_avg_doc_topics, width = 8, height = 6)
cat("Document-topic distribution plot saved to plots/lda_doc_topic_distribution.png\n")

# Optional: Map topics to original categories for interpretation
# Ensure the 'document_id' column in df_tm matches the 'document' column in doc_topics.
# The 'document' column in tidytext output is typically character numbers ("1", "2", etc.).
# df_tm$document <- as.character(1:nrow(df_tm)) # Already done earlier as document_id
df_topics_joined <- df_tm %>%
  left_join(doc_topics %>%
              group_by(document) %>%
              slice_max(gamma, n = 1) %>%
              ungroup() %>%
              select(document, topic), # Select only necessary columns
            by = c("document_id" = "document")) # Join by matching id to document

topic_category_map <- df_topics_joined %>%
  group_by(topic, Main_Category) %>%
  tally(sort = TRUE) %>%
  slice_max(n, n = 1) %>%
  ungroup() %>%
  mutate(topic_label = paste("Topic", topic))

cat("\n--- Topic-to-Main_Category Mapping ---\n")
print(topic_category_map)
cat("This table suggests which of your original news categories (e.g., 'Politics', 'Sports') is most represented in each identified topic.\n")
cat("---------------------------------------\n")

# Final output of top terms for each topic
cat("\n--- Top 10 terms for each topic (from topicmodels package) ---\n")
print(terms(lda_model, 10))
cat("--------------------------------------------------------------\n")

cat("\nProject script execution complete. Check 'data', 'output', and 'plots' folders for results.\n")