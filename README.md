# 🎬 Metacritic Movie Genre Analysis: Comedy vs Horror

<p align="center">
  <img src="plots/metacritic_header.png" alt="Movie Poster Collage" width="30%">
</p>

[![R](https://img.shields.io/badge/R-4.3.1-blue.svg)](https://www.r-project.org/)  
[![Tidyverse](https://img.shields.io/badge/Libraries-Tidyverse%20%26%20ggplot2-green)](https://www.tidyverse.org/)  
[![License: Academic](https://img.shields.io/badge/License-Academic-lightgrey.svg)](https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm)  
[![Platform](https://img.shields.io/badge/Platform-RStudio-orange.svg)](https://posit.co/)

---

## 📌 Overview

This project explores and compares **Comedy** and **Horror** movie genres using web-scraped data from [Metacritic.com](https://www.metacritic.com). It integrates **web scraping**, **text mining**, **sentiment analysis**, and **time series forecasting** to uncover genre-specific patterns in movie content, release timing, reception, and emotional tone.

---

## 🧰 Methods Used

- **Web Scraping**: `rvest`, `httr`
- **Text Cleaning**: `tidytext`, `tm`, `stringr`
- **Sentiment Analysis**: `syuzhet`, `textdata` (NRC lexicon)
- **Visualization**: `ggplot2`, `wordcloud`, `facet_wrap`, `coord_flip`
- **Forecasting**: `forecast`, `fpp2`, `ARIMA`

---

## 📊 Key Insights

### 🎬 1. Genre Composition

- Total Movies Scraped:  
  - **Comedy**: 8088  
  - **Horror**: 3048

- Missing `Metascore`: Mostly newer releases lacking critic reviews.

---

### 📅 2. Release Patterns

- Comedy movies released more consistently throughout the year.
- Horror movies peak around **October**, suggesting a Halloween effect.

![Release by Month](plots/number_daily_montlhy_movie_release_with_genre_averages_all.png)

---

### 🧪 3. Metascore Comparison

- **Comedy** average: ~54  
- **Horror** average: ~51  
- **T-test** confirmed significant difference.

<p align="center">
  <img src="plots/metascore_comparison_all.png" alt="Metascore Boxplot" width="600"/>
</p>


---

### 🔠 4. Word Frequency & TF-IDF

- **Comedy Top Words**: life, love, wedding, quirky, ensemble  
- **Horror Top Words**: escape, killer, dark, infected, occult

<p align="center">
  <img src="plots/wordcloud2_comedy_all.png" alt="Wordcloud Comedy" width="400"/>
  <img src="plots/wordcloud2_horror_all.png" alt="Wordcloud Horror" width="400"/>
</p>

<p align="center">
  <img src="plots/tfidf_words_all.png" alt="TF-IDF" width="600"/>
</p>


---

### ❤️ 5. NRC Sentiment Analysis

- Both genres skew **negative**, but Horror has more negative value 

<p align="center">
  <img src="plots/bing_sentiment_all.png" alt="NRC Sentiment Comparison" width="600"/>
</p>

---

### 🔮 6. Forecasting

- ARIMA models forecast a **slight drop** in number of movies
- Metascore projected to **increase** gradually for both genres

![Forecasting Metascore](plots/forecast_number_movei_average_metascore_all.png)

---

📂For more plots and visualizations, please see the [plots folder](./plots).

---

## 👨‍🎓 Author 

**Author:** Azrul Zulhilmi Ahmad Rosli

## License

This project is for academic purposes. Data source: [Metacritic -Movie Reviews](https://www.metacritic.com/).

---

