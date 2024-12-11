library(rpart) 
library(rpart.plot)

rm(list=ls())

ebay <- read.csv("eBayAuctions.csv")
ebay$Duration <- as.factor(ebay$Duration)
print(ebay$Duration)

# partition
set.seed(1)  
train.index <- sample(c(1:dim(ebay)[1]), dim(ebay)[1]*0.6)  
train.df <- ebay[train.index, ]
valid.df <- ebay[-train.index, ]

#Part a

## Full Tree

class.tree <- rpart(Competitive. ~ ., data = train.df, method = "class", cp=0,
                    control = rpart.control(minsplit = 1, maxdepth = 7, minbucket = 50))

prp(class.tree, type = 1, extra = 1, split.font = 2, varlen = -20)

print(class.tree)

##Pruned tree

pruned_tree <- prune(class.tree, cp = class.tree$cptable[which.min(class.tree$cptable[,"xerror"]), "CP"])
rpart.plot(pruned_tree, type = 3, extra = 104, under = TRUE, varlen = 0)
printcp(pruned_tree)

# part d (Tree with selected predictors and Pruned Tree)
class.tree2 <- rpart(Competitive. ~ OpenPrice + sellerRating + Category + 
                       endDay + currency , data = train.df, method = "class", 
                    control = rpart.control(maxdepth = 7, minbucket = 50))
print(class.tree2)

prp(class.tree2, type = 1, extra = 1, split.font = 1, varlen = -10) 

pruned_tree2 <- prune(class.tree2, cp = class.tree2$cptable[which.min(class.tree2$cptable[,"xerror"]), "CP"])

rpart.plot(pruned_tree2, type = 3, extra = 104, under = TRUE, varlen = 0)

printcp(pruned_tree2)

# part e

library(ggplot2)

##scatter plot
ggplot(train.df, aes(x = OpenPrice, y = sellerRating, color = factor(Competitive.))) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("pink", "blue"), labels = c("Noncompetitive", "Competitive")) +
  labs(x = "Opening Price", y = "Seller Rating", color = "Auction Outcome") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 100)) +  # Set x-axis limits and breaks
  geom_vline(xintercept = class.tree2$splits[, "index"], linetype = "dashed", color = "green") +
  geom_hline(yintercept = class.tree2$splits[, "index"], linetype = "dashed", color = "yellow")


#zoomed in
ggplot(train.df, aes(x = OpenPrice, y = sellerRating, color = factor(Competitive.))) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("pink", "blue"), labels = c("Noncompetitive", "Competitive")) +
  labs(x = "Opening Price", y = "Seller Rating", color = "Auction Outcome") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10)) +  # Set x-axis limits and breaks
  geom_vline(xintercept = class.tree2$splits[, "index"], linetype = "dashed", color = "green") +
  geom_hline(yintercept = class.tree2$splits[, "index"], linetype = "dashed", color = "yellow")


# part f
library(caret)

## Predict on validation data
validation_preds <- predict(pruned_tree2, valid.df, type = "class")

validation_preds <- factor(validation_preds, levels = levels(valid.df$Competitive.))
valid.df$Competitive. <- factor(valid.df$Competitive.)


## Confusion matrix
conf_matrix <- confusionMatrix(validation_preds, valid.df$Competitive.)
print(conf_matrix)

##lift chart

library(dplyr)

# Lift chart calculation
valid_probs <- predict(pruned_tree2, valid.df, type = "prob")[, "1"]
valid.df <- valid.df%>%mutate(predicted_prob = valid_probs)

install.packages("gains")
library(gains)

actual = valid.df$Competitive.
actual_numeric <- as.numeric(as.character(actual))

unique_probs <- unique(valid_probs)
gain1 = gains(actual_numeric, valid_probs, group = length(unique_probs))

plot(c(0, gain1$cume.pct.of.total*sum(actual_numeric))~c(0, gain1$cume.obs), type = "l", 
     xlab = "#Cases", ylab = "Cumulative Competitive", main = "Lift Chart for Competitive Auctions")
segments(0, 0, nrow(valid.df), sum(actual_numeric), lty = "dashed", col = "red", lwd = 2)




















