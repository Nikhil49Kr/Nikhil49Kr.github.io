library(readr)
library(tidyverse)
library(Matrix)
library(proxy)
library(cluster)
library(ggplot2)

# load data
df <- read_csv("/content/CleanTextData.csv") %>% na.omit()

# preprocess text
tfidf <- text2vec::TfidfVectorizer()
X <- as(tfidf$fit_transform(df$cleaned_text), "dgCMatrix")

# compute cosine distance between documents
dist_matrix <- dist(cosine_simil(as.matrix(X)))

# perform hierarchical clustering
linkage_matrix <- hclust(dist_matrix, method = "ward.D")

# plot dendrogram
ggplot() +
  geom_segment(aes(x = linkage_matrix$merge[, 1], y = linkage_matrix$height, xend = linkage_matrix$merge[, 2], yend = linkage_matrix$height), color = "gray") +
  geom_point(aes(x = 1:nrow(X), y = 0), size = 1) +
  scale_x_continuous(expand = c(0, 0), breaks = 1:nrow(X), labels = df$Index) +
  ylab("Distance (Cosine)") +
  xlab("Document Index") +
  ggtitle("Hierarchical Clustering Dendrogram") +
  theme_classic()