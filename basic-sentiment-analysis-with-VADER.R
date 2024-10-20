# Basic sentiment analysis using R with VADER
# See blog post on https://blog.marketingdatascience.ai
# By Joe Domaleski

# Load necessary libraries
library(tidyverse)
library(vader)
library(scales)

# Step 1: Define a tibble with some sentences
texts <- tibble(sentences = c("I feel happy today.",
                              "I feel happy today!",
                              "I feel HAPPY today!",
                              "I feel NOT HAPPY today!",
                              "I feel REALLY NOT HAPPY today!",
                              "I feel happy, but the weather is terrible.",
                              "I feel terrible, but I'm still hopeful."))

# Step 2: Apply get_vader() to each sentence to extract the sentiment data
# get_vader() will return both word-level and overall sentiment analysis
vader_results <- texts %>%
  mutate(vader_output = map(sentences, ~ get_vader(.x)))

# Step 3: Extract relevant components: word scores, compound, pos, neu, neg, and but count
vader_results <- vader_results %>%
  mutate(
    word_scores = map(vader_output, ~ .x[names(.x) != "compound" & 
                                           names(.x) != "pos" & 
                                           names(.x) != "neu" & 
                                           names(.x) != "neg" & 
                                           names(.x) != "but_count"]),  # Extract word-level scores
    compound = map_dbl(vader_output, ~ as.numeric(.x["compound"])),
    pos = map_dbl(vader_output, ~ as.numeric(.x["pos"])),
    neu = map_dbl(vader_output, ~ as.numeric(.x["neu"])),
    neg = map_dbl(vader_output, ~ as.numeric(.x["neg"])),
    but_count = map_dbl(vader_output, ~ as.numeric(.x["but_count"]))
  )

# Step 4: Display the final results as a table
vader_results %>%
  select(sentences, word_scores, compound, pos, neu, neg, but_count) %>%
  unnest(word_scores) %>%
  knitr::kable()

# Step 5: Visualize compound sentiment score for each sentence
ggplot(vader_results, aes(x = sentences, y = compound, fill = compound > 0)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red"), labels = c("Positive", "Negative")) +
  labs(title = "Overall Compound Sentiment for Each Sentence",
       x = "Sentences",
       y = "Compound Sentiment",
       fill = "Sentiment") +
  coord_flip() +  # Flip for easier readability
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Label wrapping and adjusting angle

# Step 6: Visualize positive, neutral, and negative proportions as a stacked bar plot
vader_results %>%
  mutate(pos_pct = pos * 100, neu_pct = neu * 100, neg_pct = neg * 100) %>%
  select(sentences, pos_pct, neu_pct, neg_pct) %>%
  pivot_longer(cols = c(pos_pct, neu_pct, neg_pct), names_to = "sentiment", values_to = "percentage") %>%
  mutate(sentiment = factor(sentiment, levels = c("pos_pct", "neu_pct", "neg_pct"), labels = c("Positive", "Neutral", "Negative"))) %>%
  ggplot(aes(x = sentences, y = percentage, fill = sentiment)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("Positive" = "blue", "Neutral" = "gray", "Negative" = "red")) +
  labs(title = "Proportion of Positive, Neutral, and Negative Sentiment",
       x = "Sentences",
       y = "Percentage",
       fill = "Sentiment") +
  coord_flip() +
  theme_minimal()

