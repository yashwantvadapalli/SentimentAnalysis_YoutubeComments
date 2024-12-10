# YouTube Sentiment Analysis

This project performs sentiment analysis on YouTube comments from the video ["I Want It That Way | Brooklyn Nine-Nine"](https://www.youtube.com/watch?v=HlBYdiXdUa8). Using Natural Language Processing (NLP) techniques and R, the project analyzes viewer comments to uncover patterns, identify sentiment trends, and visualize data.

---

## Introduction

The primary goal of this project is to analyze YouTube comments to classify sentiments as positive, negative, or neutral. This involves applying advanced text analysis techniques such as Term-Document Matrices (TDM), TF-IDF, and Latent Dirichlet Allocation (LDA). By extracting insights, the project aims to reveal trends in viewer opinions and highlight recurring themes.

---

## Features

- **Sentiment Analysis**: Classifies comments into positive, negative, and neutral categories.
- **Term Frequency Analysis**: Identifies frequently used words in different sentiment categories.
- **Latent Dirichlet Allocation (LDA)**: Clusters terms into topics based on their associations.
- **Visualizations**: Generates word clouds, sentiment distributions, and topic-word distributions.

---

## Technologies Used

- **Programming Language**: R
- **Libraries and Packages**:
  - `tm` for text mining.
  - `syuzhet` for sentiment scoring.
  - `ggplot2` for data visualization.
  - `topicmodels` for LDA.
  - `wordcloud` for visual representation.
- **Dataset**: Comments scraped from the YouTube video ["I Want It That Way | Brooklyn Nine-Nine"](https://www.youtube.com/watch?v=HlBYdiXdUa8).

---

## Results

### Key Findings:
- **Net Sentiment Distribution**: Most comments were neutral, likely due to factual discussions.
- **Positive Sentiments**: Highlighted appreciation for the humor and nostalgia of the scene.
- **Negative Sentiments**: Focused on specific lines or perceived issues with the content.

### Visualizations:
- Sentiment distribution charts, word clouds, and topic-word distributions effectively illustrate the analysis.
