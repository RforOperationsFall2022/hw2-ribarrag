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
consumer_df <- consumer_df %>%
  mutate(MntPurchases = rowSums(across(c(MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds))))


table(consumer_df$Education)

consumer_df$MntPurchases

ggplot()

round(mean(mean(consumer_df$NumWebPurchases / consumer_df$TotalPurchases), na.rm = TRUE), 2)

consumer_df$TotalPurchases

colnames(consumer_df)

ggplot(consumer_df, aes(TotalPurchases)) + 
  geom_histogram()

ggplot(consumer_df, aes(Income)) + 
  geom_histogram(bins = 40)

colnames(consumer_df)
mpg$cty

# agregado <- 
aggregate(mpg$cty, by=list(mpg$manufacturer), FUN=mean)

mean(consumer_df$MntFishProducts)

colMeans(consumer_df[, c(MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds)])

colMeans(consumer_df[, 10:15])


ggplot(consumer_df, aes(MntFishProducts)) +
  geom_density(na.rm = TRUE)  
  xlim(58, 68) + 
  theme(legend.position = "none")

# g <- ggplot(consumer_df, aes(y = MntWines, x = Income, colour = Children))
g <- ggplot(consumer_df, aes(y = MntWines, x = MntFruits, colour = Age))
g + geom_count(show.legend=F, alpha = 0.6) +
  labs(subtitle="mpg: city vs highway mileage", 
       y="hwy", 
       x="cty", 
       title="Counts Plot")

  xlim(58, 68) + 
  theme(legend.position = "none")

library(plotly)
  
medias <- colMeans(consumer_df[, 10:15])
colnames(medias)

ggplot(medias, aes(x=make, y=mileage)) + 
  geom_point(col="tomato2", size=3) +   # Draw points
  geom_segment(aes(x=make, 
                   xend=make, 
                   y=min(mileage), 
                   yend=max(mileage)), 
               linetype="dashed", 
               size=0.1) +   # Draw dashed lines
  labs(title="Dot Plot", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") +  
  coord_flip()

cty_mpg

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


a <- NA
a
b <- NULL
b
max(is.na(a), a)
max(is.na(a), 0.0001)
max(TRUE, a)
b > 9
is.na(a)

a %>% replace(is.na(.), 0)
a

max(consumer_df$Income)

