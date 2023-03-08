library(readr)
library(tidyverse)
library(arules)
library(arulesViz)

# read the data
data <- read_csv("/content/CleanTextData.csv") %>% na.omit()

# create the transaction list format
df <- as(split(data$cleaned_text, data$Index), "transactions")

# find frequent itemsets
frequent_itemsets <- apriori(df, parameter = list(support = 0.1), appearance = list())
frequent_itemsets <- sort(frequent_itemsets, decreasing = TRUE, by = "support")

# generate association rules
rules <- as(data.frame(quality(frequent_itemsets)), "rules")
rules <- sort(rules, decreasing = TRUE, by = "lift")

# print the rules
print(rules)

# Print the top 10 rules sorted by lift
print(rules[1:10])

# plot rules with support >= 0.05 and confidence >= 0.5
rules_to_plot <- subset(rules, support >= 0.05 & confidence >= 0.5)

# create scatter plot of rules
plot(rules_to_plot, method = "scatterplot", measure = c("support", "confidence"), xlim = c(0.05, 1), ylim = c(0.5, 1), shade = TRUE, alpha = 0.5, control = list(reorder = TRUE))

# add axis labels and title
title("Association Rules")