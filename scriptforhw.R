library(readr)
library(ggplot2)
library(dplyr)
consumer_df <- read_delim("consumer_dataset.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)
# dataset available on: https://www.kaggle.com/datasets/imakash3011/customer-personality-analysis


# clean dataset
consumer_df <- na.omit(consumer_df)
consumer_df <- consumer_df[-c(which(consumer_df$Income == max(consumer_df$Income)),which(consumer_df$Year_Birth <= 1900)), ]

# Adjust dataset
consumer_df$Age = 2022 - consumer_df$Year_Birth
consumer_df$Children = consumer_df$Kidhome + consumer_df$Teenhome
consumer_df$Marital_Status[consumer_df$Marital_Status %in% c('Absurd', 'Alone', 'YOLO')] <- 'Other'
consumer_df <- consumer_df %>%
  mutate(TotalPurchases = rowSums(across(c(NumStorePurchases,NumWebPurchases,NumCatalogPurchases))))



round(mean(mean(consumer_df$NumWebPurchases / consumer_df$TotalPurchases), na.rm = TRUE), 2)

consumer_df$TotalPurchases
  


?fluidRow
mean(consumer_df$NumStorePurchases)
consumer_df[, c(NumStorePurchases,NumWebPurchases)]


consumer_df[c("NumStorePurchases","NumWebPurchases", "NumCatalogPurchases", "TotalPurchases")]

problems()
print(n = ...)
nrow(consumer_df)

colnames(consumer_df)

table(consumer_df$Marital_Status)

mean(consumer_df$NumWebPurchases / consumer_df$TotalPurchases)


table(consumer_df$Education)
table(consumer_df$Marital_Status)
colnames(consumer_df)

consumer_df

ggplot(consumer_df, aes(x = MntSweetProducts, y = Kidhome)) + 
  geom_point()

ggplot(consumer_df, aes(MntWines, NumStorePurchases)) +
  geom_point() +
  facet_wrap(~Kidhome)

table(consumer_df$NumStorePurchases, consumer_df$Kidhome)
ggplot(consumer_df, aes(Marital_Status, Income)) + 
  geom_violin()

summary(consumer_df)

which(consumer_df$Income == max(consumer_df$Income))
which(consumer_df$Year_Birth == min(consumer_df$Year_Birth))
consumer_df[229, ]


summary(consumer_df)
which(max(consumer_df$Income))



