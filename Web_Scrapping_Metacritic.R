library(rvest)
library(dplyr)
library(stringr)
library(httr)
library(lubridate)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(patchwork)
library(fpp3)

# Define safe HTML reader
get_page <- function(url) {
  tryCatch({
    response <- GET(url, user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)"))
    read_html(response)
  }, error = function(e) {
    message("Failed to read: ", url)
    return(NULL)
  })
}

# Define scraping function
scrape_metacritic_page <- function(url) {
  page <- get_page(url)
  if (is.null(page)) return(NULL)
  
  cards <- html_nodes(page, ".c-finderProductCard_info")
  if (length(cards) == 0) return(NULL)
  
  result <- lapply(cards, function(card) {
    title <- card %>% html_node(".c-finderProductCard_title span") %>% html_text(trim = TRUE)
    date <- card %>% html_node(".c-finderProductCard_meta span.u-text-uppercase") %>% html_text(trim = TRUE)
    description <- card %>% html_node(".c-finderProductCard_description span") %>% html_text(trim = TRUE)
    metascore <- card %>% html_node(".c-siteReviewScore span") %>% html_text(trim = TRUE)
    
    data.frame(
      title = title,
      date = date,
      description = description,
      metascore = metascore,
      stringsAsFactors = FALSE
    )
  })
  
  bind_rows(result)
}

#-----------------------------------------------------------------------------------------------------
# Horror Movies
# First 3 pages
horror_base_url <- "https://www.metacritic.com/browse/movie/all/horror/all-time/new/?releaseYearMin=1910&releaseYearMax=2025&genre=horror&page="
horror_three_pages <- paste0(horror_base_url, 0:3)

# Scrape all pages with progress
horror_three_data <- list()
for (i in seq_along(horror_three_pages)) {
  message("Scraping page ", i - 1)
  Sys.sleep(2)  # polite pause
  data <- scrape_metacritic_page(horror_three_pages[i])
  if (!is.null(data) && nrow(data) > 0) {
    horror_three_data[[length(horror_three_data) + 1]] <- data
  } else {
    message("No data found on page ", i - 1)
  }
}


# Combine into single dataframe
horror_three <- bind_rows(horror_three_data)

# Extract day, month, year
horror_three <- horror_three %>%
  mutate(date_parsed = mdy(date)) %>%
  mutate(
    day = day(date_parsed),
    month = month(date_parsed, label = TRUE, abbr = FALSE),
    year = year(date_parsed)
  ) %>%
  select(-date_parsed)

str(horror_three)
#-----------------------------------------------------------------------------------------------------
# Comedy Movies
# First 3 pages
comedy_base_url <- "https://www.metacritic.com/browse/movie/all/comedy/all-time/new/?releaseYearMin=1910&releaseYearMax=2025&genre=comedy&page="
comedy_three_pages <- paste0(comedy_base_url, 0:3)

# Scrape all pages with progress
comedy_three_data <- list()
for (i in seq_along(comedy_three_pages)) {
  message("Scraping page ", i - 1)
  Sys.sleep(2)  # polite pause
  data <- scrape_metacritic_page(comedy_three_pages[i])
  if (!is.null(data) && nrow(data) > 0) {
    comedy_three_data[[length(comedy_three_data) + 1]] <- data
  } else {
    message("No data found on page ", i - 1)
  }
}

# Combine into single dataframe
comedy_three <- bind_rows(comedy_three_data)

# Extract day, month, year
comedy_three <- comedy_three %>%
  mutate(date_parsed = mdy(date)) %>%
  mutate(
    day = day(date_parsed),
    month = month(date_parsed, label = TRUE, abbr = FALSE),
    year = year(date_parsed)
  ) %>%
  select(-date_parsed)

str(comedy_three)
# Analysis for first 3 pages -----------------------------------------------------------------------------------------------------------------------

# Add genre labels
comedy_three$genre <- "Comedy"
horror_three$genre <- "Horror"

comedy_na<-sum(is.na(comedy_three))
horror_na<-sum(is.na(horror_three))

# Combine
movies_three <- bind_rows(comedy_three, horror_three)

# Count total and NA metascores by genre
na_stats_three <- movies_three %>%
  group_by(genre) %>%
  summarise(
    total_movies = n(),
    na_metascore = sum(is.na(metascore)),
    na_ratio = na_metascore / total_movies
  )

print(na_stats_three)

# Metascore analysis with NA
movies_three <- movies_three %>% mutate(metascore = as.numeric(metascore))
ggplot(movies_three %>% filter(!is.na(metascore)), aes(x = genre, y = metascore, fill = genre)) +
  geom_boxplot() +
  labs(title = "Metascore Comparison (First 3 Pages)")

t.test(metascore ~ genre, data = movies_three)

# Clean words
words_clean_three <- movies_three %>%
  select(genre, description) %>%
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "^[0-9]+$"))

# Top 10 words per genre
top_words_three <- words_clean_three %>%
  count(genre, word, sort = TRUE) %>%
  group_by(genre) %>%
  slice_max(order_by = n, n = 10) %>%
  ungroup()

# Bar chart
top_words_mirror_three <- top_words_three %>%
  mutate(n = ifelse(genre == "Comedy", -n, n))

ggplot(top_words_mirror_three, aes(x = reorder(word, abs(n)), y = n, fill = genre)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(title = "Top 10 Words (First 3 Pages)", x = "Word", y = "Count") +
  theme_minimal() +
  scale_fill_manual(values = c("Comedy" = "skyblue", "Horror" = "salmon"))

# Wordclouds
wordcloud2(words_clean_three %>% filter(genre == "Comedy") %>% count(word, sort = TRUE),
           size = 0.8, color = "random-light", backgroundColor = "black")
wordcloud2(words_clean_three %>% filter(genre == "Horror") %>% count(word, sort = TRUE),
           size = 0.8, color = "random-light", backgroundColor = "black")

# Use 'bing' sentiment lexicon
sentiment_bing <- get_sentiments("bing")

# Sentiment analysis for first three pages
sentiment_three <- words_clean_three %>%
  inner_join(sentiment_bing, by = "word") %>%
  count(genre, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)

print(sentiment_three)

# Plot
ggplot(sentiment_three, aes(x = genre, y = net_sentiment, fill = genre)) +
  geom_col() +
  labs(title = "Net Sentiment by Genre (First 3 Pages)", y = "Net Sentiment") +
  theme_minimal() +
  scale_fill_manual(values = c("Comedy" = "skyblue", "Horror" = "salmon"))

# TF-IDF (Important Words Per Genre) - First 3 Pages

# TF-IDF calculation
tf_idf_three <- words_clean_three %>%
  count(genre, word, sort = TRUE) %>%              # word count per genre
  bind_tf_idf(term = word, document = genre, n = n) %>%  # compute tf-idf
  arrange(desc(tf_idf)) %>%
  group_by(genre) %>%
  slice_max(order_by = tf_idf, n = 10) %>%
  ungroup()

# View result
print(tf_idf_three, n=30)

# Mirror Plot
tf_idf_three_mirror <- tf_idf_three %>%
  mutate(tf_idf = ifelse(genre == "Comedy", -tf_idf, tf_idf))

ggplot(tf_idf_three_mirror, aes(x = reorder(word, abs(tf_idf)), y = tf_idf, fill = genre)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(
    title = "Top TF-IDF Words: Comedy (Left) vs Horror (Right) - First 3 Pages",
    x = "Word",
    y = "TF-IDF Score"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Comedy" = "skyblue", "Horror" = "salmon"))



# Make sure month is an ordered factor
movies_three <- movies_three %>%
  mutate(month = factor(month, levels = month.name))

#  Plot monthly release count with average line
# Calculate average releases per month by genre
avg_month_3 <- movies_three %>%
  group_by(genre, month) %>%
  summarise(count = n()) %>%
  group_by(genre) %>%
  summarise(avg = mean(count))

# Plot monthly movie count

plot_by_month<-movies_three %>%
  group_by(genre, month) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = month, y = count, fill = genre)) +
  geom_col(position = "dodge") +
  geom_hline(data = avg_month_3, aes(yintercept = avg, color = genre), linetype = "dashed", size = 1) +
  geom_text(data = avg_month_3, aes(x = month.name[1], y = avg, label = paste("Avg:", round(avg, 1)), color = genre),
            vjust = -0.5, hjust = 0) +
  labs(title = "Monthly Movie Releases (First 3 Pages)",
       y = "Number of Movies", fill = "Genre", color = "Average Line") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot daily release count with average line
# Calculate average releases per day by genre
avg_day_3 <- movies_three %>%
  group_by(genre, day) %>%
  summarise(count = n()) %>%
  group_by(genre) %>%
  summarise(avg = mean(count))

# Plot daily movie count
plot_by_day<-movies_three %>%
  group_by(genre, day) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = day, y = count, fill = genre)) +
  geom_col(position = "dodge") +
  geom_hline(data = avg_day_3, aes(yintercept = avg, color = genre), linetype = "dashed", size = 1) +
  geom_text(data = avg_day_3, aes(x = 1, y = avg, label = paste("Avg:", round(avg, 1)), color = genre),
            vjust = -0.5, hjust = 0) +
  labs(title = "Daily Movie Releases (First 3 Pages)",
       y = "Number of Movies", fill = "Genre", color = "Average Line") +
  theme_minimal()

plot_by_month | plot_by_day

# -----------------------------------------------------------------------------------------------------------
# All pages (127 pages) - Horror
horror_all_pages <- paste0(horror_base_url, 0:127)

# Scrape all pages with progress
horror_all_data <- list()
for (i in seq_along(horror_all_pages)) {
  message("Scraping page ", i - 1)
  Sys.sleep(2)  # polite pause
  data <- scrape_metacritic_page(horror_all_pages[i])
  if (!is.null(data) && nrow(data) > 0) {
    horror_all_data[[length(horror_all_data) + 1]] <- data
  } else {
    message("No data found on page ", i - 1)
  }
}

# Combine into single dataframe
horror_all <- bind_rows(horror_all_data)

# Extract day, month, year
horror_all <- horror_all %>%
  mutate(date_parsed = mdy(date)) %>%
  mutate(
    day = day(date_parsed),
    month = month(date_parsed, label = TRUE, abbr = FALSE),
    year = year(date_parsed)
  ) %>%
  select(-date_parsed)

# -----------------------------------------------------------------------------------------------------------
# All pages (337 pages) - Comedy
comedy_all_pages <- paste0(comedy_base_url, 0:337)

# Scrape all pages with progress
comedy_all_data <- list()
for (i in seq_along(comedy_all_pages)) {
  message("Scraping page ", i - 1)
  Sys.sleep(2)  # polite pause
  data <- scrape_metacritic_page(comedy_all_pages[i])
  if (!is.null(data) && nrow(data) > 0) {
    comedy_all_data[[length(comedy_all_data) + 1]] <- data
  } else {
    message("No data found on page ", i - 1)
  }
}

# Combine into single dataframe
comedy_all <- bind_rows(comedy_all_data)

# Extract day, month, year
comedy_all <- comedy_all %>%
  mutate(date_parsed = mdy(date)) %>%
  mutate(
    day = day(date_parsed),
    month = month(date_parsed, label = TRUE, abbr = FALSE),
    year = year(date_parsed)
  ) %>%
  select(-date_parsed)

#---------------------------------------------------------------------------------------------
# Analysis 

# Add genre label to each dataframe
comedy_all$genre <- "Comedy"
horror_all$genre <- "Horror"

# Combine datasets
movies_all <- bind_rows(comedy_all, horror_all)
movies_all$genre <- as.character(movies_all$genre)

# Missing Data Analysis

# NA metascore count by genre
na_by_genre <- movies_all %>%
  mutate(missing_metascore = is.na(metascore)) %>%
  group_by(genre) %>%
  summarise(missing_count = sum(missing_metascore), total = n()) %>%
  mutate(pct_missing = round(100 * missing_count / total, 1))

print(na_by_genre)

# NA metascore by year
na_by_year_genre <- movies_all %>%
  mutate(missing = is.na(metascore)) %>%
  group_by(year, genre) %>%
  summarise(
    total = n(),
    na_count = sum(missing),
    pct_missing = 100 * na_count / total,
    .groups = "drop"
  )

# Make sure 'year' column exists and is numeric
horror_all$year <- as.numeric(horror_all$year)

# Filter for horror movies released in 1966
horror_1966 <- horror_all %>% filter(year == 1966)

# View the result
print(horror_1966)


# Plot
ggplot(na_by_year_genre, aes(x = year, y = pct_missing, fill = genre)) +
  geom_col(position = "dodge") +
  labs(
    title = "Percentage of Missing Metascore by Year and Genre",
    x = "Year",
    y = "% Missing"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Comedy" = "skyblue", "Horror" = "salmon")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#------------------------------------------------------------------------------------------------------
# Metascore Distribution Comparison
# Impute missing metascore with genre-wise average
horror_all <- horror_all %>%
  mutate(metascore = as.numeric(metascore)) %>%
  group_by(genre) %>%
  mutate(metascore = ifelse(is.na(metascore),
                            round(mean(metascore, na.rm = TRUE)), 
                            round(metascore))) %>%
  ungroup()

comedy_all <- comedy_all %>%
  mutate(metascore = as.numeric(metascore)) %>%
  group_by(genre) %>%
  mutate(metascore = ifelse(is.na(metascore),
                            round(mean(metascore, na.rm = TRUE)), 
                            round(metascore))) %>%
  ungroup()


movies_all <- movies_all %>%
  mutate(metascore = as.numeric(metascore)) %>%
  group_by(genre) %>%
  mutate(metascore = ifelse(is.na(metascore),
                            round(mean(metascore, na.rm = TRUE)), 
                            round(metascore))) %>%
  ungroup()


movies_score <- bind_rows(comedy_all, horror_all) %>%
  filter(!is.na(metascore)) %>%
  mutate(metascore = as.numeric(metascore))

ggplot(movies_score, aes(x = genre, y = metascore, fill = genre)) +
  geom_boxplot() +
  labs(title = "Metascore Comparison: Horror vs Comedy")

t.test(metascore ~ genre, data = movies_score)

# Function to count outliers based on IQR rule
count_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  sum(x < lower | x > upper, na.rm = TRUE)
}

# Count outliers by genre
movies_score %>%
  group_by(genre) %>%
  summarise(outliers = count_outliers(metascore))

# -------------------------------------------------------------------------------------------------
# Word Frequency Analysis
# Tokenize and clean, preserving genre

words_clean <- movies_all %>%
  select(genre, description) %>%  # keep genre
  unnest_tokens(word, description) %>%
  anti_join(stop_words, by = "word") %>%
  filter(!str_detect(word, "^[0-9]+$"))  # remove numbers

# Count top 10 words per genre
top_words <- words_clean %>%
  dplyr::count(genre, word, sort = TRUE) %>%
  dplyr::group_by(genre) %>%
  dplyr::slice_max(order_by = n, n = 10) %>%
  dplyr::ungroup()

print(top_words)

# Adjust the count direction by genre
top_words_mirrored <- top_words %>%
  mutate(n = ifelse(genre == "Comedy", -n, n))

# Plot mirrored bar chart
ggplot(top_words_mirrored, aes(x = reorder(word, abs(n)), y = n, fill = genre)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(
    title = "Top 10 Words in Horror vs Comedy",
    x = "Word", y = "Count"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Comedy" = "skyblue", "Horror" = "salmon"))

# Wordcloud --------------------------------------------------------------
# Generate wordcloud for Comedy
words_comedy <- words_clean %>%
  filter(genre == "Comedy") %>%
  dplyr::count(word, sort = TRUE)

# Comedy Wordcloud
wordcloud2(words_comedy, size = 0.8, color = "random-light", backgroundColor = "black")

# Generate wordcloud for Horror
words_horror <- words_clean %>%
  filter(genre == "Horror") %>%
  dplyr::count(word, sort = TRUE)

# Horror Wordcloud
wordcloud2(words_horror, size = 0.8, color = "random-light", backgroundColor = "black")

# Sentiment ---------------------------------------------------------------------------------------------
# Sentiment analysis for all pages
# Use 'bing' sentiment lexicon
sentiment_bing <- get_sentiments("bing")
sentiment_all <- words_clean %>%
  inner_join(sentiment_bing, by = "word") %>%
  count(genre, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)

print(sentiment_all)

# Plot
ggplot(sentiment_all, aes(x = genre, y = net_sentiment, fill = genre)) +
  geom_col() +
  labs(title = "Net Sentiment by Genre (All Pages)", y = "Net Sentiment") +
  theme_minimal() +
  scale_fill_manual(values = c("Comedy" = "skyblue", "Horror" = "salmon"))


# ----------------------------------------------------------------------------------------------------------
# TF-IDF (Important Words Per Genre)

# Make sure the data is in correct format
tf_idf <- words_clean %>%
  count(genre, word, sort = TRUE) %>%              # word count per genre
  bind_tf_idf(term = word, document = genre, n = n) %>%  # compute tf-idf
  arrange(desc(tf_idf)) %>%
  group_by(genre) %>%
  slice_max(order_by = tf_idf, n = 10) %>%
  ungroup()

# View result
print(tf_idf, n=50)

# Mirror the TF-IDF scores
tf_idf_mirror <- tf_idf %>%
  mutate(tf_idf = ifelse(genre == "Comedy", -tf_idf, tf_idf))

ggplot(tf_idf_mirror, aes(x = reorder(word, abs(tf_idf)), y = tf_idf, fill = genre)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(
    title = "Top TF-IDF Words: Comedy (Left) vs Horror (Right)",
    x = "Word",
    y = "TF-IDF Score"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Comedy" = "skyblue", "Horror" = "salmon"))

#-----------------------------------------------------------------------------------------------------
# By Year -------------------------------------------------------------
## Compute average releases per genre per year
avg_year <- movies_all %>%
  group_by(genre, year) %>%
  summarise(count = n()) %>%
  group_by(genre) %>%
  summarise(avg = mean(count))

## Bar plot with average lines
# Plot with average line and legend
movies_year<-movies_all %>%
  group_by(genre, year) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = year, y = count, fill = genre)) +
  geom_col(position = "dodge") +
  geom_hline(data = avg_year, aes(yintercept = avg, color = genre), linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_text(data = avg_year, aes(x = min(movies_all$year), y = avg, label = paste("Avg:", round(avg, 1)), color = genre),
            vjust = -0.5, hjust = 0, show.legend = FALSE) +
  labs(title = "Number of Movie Releases by Year with Genre Averages",
       y = "Number of Movies", fill = "Genre", color = "Average Line") +
  theme_minimal()

# By Month ------------------------------------------------------------
## Convert month to ordered factor
movies_all <- movies_all %>%
  mutate(month = factor(month, levels = month.name))

## Compute monthly average
avg_month <- movies_all %>%
  group_by(genre, month) %>%
  summarise(count = n()) %>%
  group_by(genre) %>%
  summarise(avg = mean(count))

## Plot
movies_month<-movies_all %>%
  group_by(genre, month) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = month, y = count, fill = genre)) +
  geom_col(position = "dodge") +
  geom_hline(data = avg_month, aes(yintercept = avg, color = genre), linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_text(data = avg_month, aes(x = month.name[1], y = avg, label = paste("Avg:", round(avg, 1)), color = genre),
            vjust = -0.5, hjust = 0, show.legend = FALSE) +
  labs(title = "Monthly Movie Releases with Genre Averages",
       y = "Number of Movies", fill = "Genre", color = "Average Line") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# By Day -------------------------------------------------------------
avg_day <- movies_all %>%
  group_by(genre, day) %>%
  summarise(count = n()) %>%
  group_by(genre) %>%
  summarise(avg = mean(count))

movies_day<-movies_all %>%
  group_by(genre, day) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = day, y = count, fill = genre)) +
  geom_col(position = "dodge") +
  geom_hline(data = avg_day, aes(yintercept = avg, color = genre), linetype = "dashed", size = 1, show.legend = TRUE) +
  geom_text(data = avg_day, aes(x = 1, y = avg, label = paste("Avg:", round(avg, 1)), color = genre),
            vjust = -0.5, hjust = 0, show.legend = FALSE) +
  labs(title = "Daily Movie Releases with Genre Averages",
       y = "Number of Movies", fill = "Genre", color = "Average Line") +
  theme_minimal()

movies_year | movies_month | movies_day
#------------------------------------------------------------------------------------------------------
# Temporal Patterns

## Release Seasonality + Metscore by Month -------------------------------------------------
# Average metascore per genre and month
movies_all <- movies_all %>%
  mutate(month = factor(month, levels = month.name))

monthly_metascore <- movies_all %>%
  filter(!is.na(metascore)) %>%
  group_by(genre, month) %>%
  summarise(avg_metascore = mean(as.numeric(metascore)), .groups = "drop")

# Plot
avg_meta_month<-ggplot(monthly_metascore, aes(x = month, y = avg_metascore, group = genre, color = genre)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Average Metascore by Month",
    x = "Month", y = "Avg Metascore"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Weekend vs Weekday Comparison ------------------------------------------

# Convert to weekday
movies_all$weekday <- wday(mdy(movies_all$date), label = TRUE)

# Check average metascore by weekday type
movies_all$day_type <- ifelse(movies_all$weekday %in% c("Sat", "Sun"), "Weekend", "Weekday")

weekday_metascore <- movies_all %>%
  filter(!is.na(metascore)) %>%
  group_by(genre, day_type) %>%
  summarise(avg_metascore = mean(as.numeric(metascore)), .groups = "drop")

# Plot
avg_meta_day<-ggplot(weekday_metascore, aes(x = day_type, y = avg_metascore, fill = genre)) +
  geom_col(position = "dodge") +
  labs(title = "Avg Metascore: Weekend vs Weekday by Genre", x = "Day Type", y = "Average Metascore") +
  theme_minimal()

avg_meta_month | avg_meta_day

# Forecasting ------------------------------------------------------------------------------------------------------

# Forecast Movie Count per Year per Genre -----------------------------------------------

# Summarize total movies per year and genre
movie_counts <- movies_all %>%
  group_by(genre, year) %>%
  summarise(count = n(), .groups = "drop")

# Create complete time series for each genre by year
movie_counts_ts <- movie_counts %>%
  as_tsibble(key = genre, index = year) %>%
  fill_gaps(count = NA)  # insert NAs for missing years

# Fit ARIMA model
movie_count_models <- movie_counts_ts %>%
  model(ARIMA(count))

# Forecast next 5 years
movie_count_forecast <- movie_count_models %>%
  forecast(h = 5)

# Plot forecast
forecast_movie<-autoplot(movie_count_forecast, movie_counts_ts) +
  labs(title = "Forecast: Number of Movies by Genre", y = "Movie Count") +
  facet_wrap(~genre) +
  theme_minimal()

# Forecast Average Metascore per Year per Genre --------------------------------------------------------
# Convert metascore to numeric and filter out NAs
movies_score_clean <- movies_all %>%
  filter(!is.na(metascore)) %>%
  mutate(metascore = as.numeric(metascore))

# Compute average metascore per year and genre
avg_metascore <- movies_score_clean %>%
  group_by(genre, year) %>%
  summarise(avg_score = mean(metascore), .groups = "drop")

# Fill missing years with NA avg_score per genre
avg_score_ts <- avg_metascore %>%
  as_tsibble(key = genre, index = year) %>%
  fill_gaps(avg_score = NA)

# Fit ARIMA model per genre
avg_score_models <- avg_score_ts %>%
  model(ARIMA(avg_score))

# Forecast next 5 years
avg_score_forecast <- avg_score_models %>%
  forecast(h = 5)

# Plot forecast
forecast_meta<-autoplot(avg_score_forecast, avg_score_ts) +
  labs(title = "Forecast: Average Metascore by Genre", y = "Metascore") +
  facet_wrap(~genre) +
  theme_minimal()

forecast_movie | forecast_meta

