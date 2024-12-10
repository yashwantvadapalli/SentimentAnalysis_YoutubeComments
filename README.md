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

## Summary

The sentiment analysis of YouTube comments for the video "I Want It That Way | Brooklyn Nine-Nine" revealed insightful trends in viewer feedback:
- The majority of comments were neutral, indicating that viewers often engaged with factual elements of the content without emotional bias.
- Positive comments highlighted the audience's appreciation for the humor and nostalgia, emphasizing the memorable nature of the scene.
- Negative comments were fewer but reflected strong opinions or critiques about specific elements.

The analysis demonstrated the effectiveness of NLP techniques, such as Term-Document Matrices, TF-IDF, and Latent Dirichlet Allocation, in uncovering patterns in textual data. Visualizations, including word clouds and sentiment distributions, provided a clear depiction of the insights derived. Overall, the project successfully identified the key themes and sentiments expressed by viewers, highlighting their engagement with the content.

