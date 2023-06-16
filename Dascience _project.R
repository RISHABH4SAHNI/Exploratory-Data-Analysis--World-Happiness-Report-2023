WHR2023 <- read.csv("C:/Users/hsahn/Downloads/WHR2023.csv")

WHR2023 <- read.csv("C:/Users/hsahn/Downloads/WHR2023.csv")
# Load the required library


small_iris6<- WHR2023[ , c('Ladder.score', 'Standard.error.of.ladder.score', 'upperwhisker','lowerwhisker','Logged.GDP.per.capita','Social.support','Healthy.life.expectancy','Freedom.to.make.life.choices','Generosity','Perceptions.of.corruption','Ladder.score.in.Dystopia','Explained.by..Log.GDP.per.capita','Explained.by..Social.support','Explained.by..Healthy.life.expectancy','Explained.by..Freedom.to.make.life.choices','Explained.by..Generosity','Explained.by..Perceptions.of.corruption','Dystopia...residual')]
small_iris6 <- na.omit(small_iris6)
percentiles <- apply(small_iris6, 2, function(x) quantile(x, probs = 0.97, na.rm = TRUE))
print(percentiles)
percentiles2 <- apply(small_iris6, 2, function(x) quantile(x, probs = 0.03, na.rm = TRUE))
print(percentiles2)




library(moments)

# Compute skewness of the 'Social.support' variable
skewness_value <- skewness(WHR2023$Social.support, na.rm = TRUE)

# Print the skewness value
cat("Skewness of 'Social.support':", skewness_value, "\n")

# Draw a histogram of the 'Social.support' variable
hist(WHR2023$Social.support, main = "Histogram of 'Social.support'", xlab = "Social Support")

library(dplyr)

# Sort the countries in descending order of "Healthy life expectancy"
sorted_data <- arrange(WHR2023, desc(Healthy.life.expectancy))

# Create a bar chart
barplot(sorted_data$`Healthy.life.expectancy`, names.arg = sorted_data$Country.name,
        main = "Healthy Life Expectancy by Country",
        xlab = "Country", ylab = "Healthy Life Expectancy",
        las = 2, horiz = TRUE)

cor_matrix <- cor(WHR2023[, c("Ladder.score", "Standard.error.of.ladder.score", "Logged.GDP.per.capita", "Social.support", "Healthy.life.expectancy")], use = "pairwise.complete.obs")

# Create the correlation heatmap
heatmap(cor_matrix, 
        col = colorRampPalette(c("#FF0000", "#FFFFFF", "#0000FF"))(100),
        main = "Correlation Heatmap",
        xlab = "Features", ylab = "Features",
        cex.main = 1.2, cex.axis = 0.9)


plot(WHR2023$`Healthy.life.expectancy`, WHR2023$`Ladder.score`,
     main = "Scatter Plot of Ladder Score vs Healthy Life Expectancy",
     xlab = "Healthy Life Expectancy",
     ylab = "Ladder Score",
     col = "blue",
     pch = 16)

# Add a regression line
abline(lm(WHR2023$`Ladder.score` ~ WHR2023$`Healthy.life.expectancy`),
       col = "red")

# Add correlation coefficient to the plot
cor_coef <- cor(WHR2023$`Healthy.life.expectancy`, WHR2023$`Ladder.score`)
cor_text <- paste("Correlation:", round(cor_coef, 2))
text(60, 7, cor_text, pos = 4, col = "darkgreen")


boxplot(WHR2023$`Healthy.life.expectancy`, main = "Box Plot of Healthy Life Expectancy",
        ylab = "Healthy Life Expectancy")

boxplot(WHR2023$`Ladder.score`, main = "Box Plot of Ladder Score",
        ylab = "Ladder Score")





threshold <- quantile(WHR2023$`Healthy.life.expectancy`, probs = 0.9, na.rm = TRUE)

# Filter the dataset to include countries with healthy life expectancy above the threshold
top_countries <- WHR2023[WHR2023$`Healthy.life.expectancy` > threshold, ]

# Display the top countries
top_countries$Country.name




numeric_columns <- sapply(WHR2023, is.numeric)
numeric_data <- WHR2023[, numeric_columns]
stats <- summary(numeric_data)

# Print the descriptive statistics
print(stats)



library(ggplot2)

# Calculate the frequencies of each value in the 'Socialism' column
frequencies <- table(WHR2023$Social.support)

# Convert frequencies to a data frame
df <- data.frame(Value = names(frequencies), Frequency = as.numeric(frequencies))
df <- df[order(-df$Frequency), ]

# Calculate the cumulative percentage
df$cumulative_percentage <- cumsum(df$Frequency) / sum(df$Frequency) * 100

# Create the Pareto chart using ggplot2
ggplot(df, aes(x = Value, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "white") +
  geom_line(aes(y = cumulative_percentage), color = "red", size = 1.5) +
  geom_text(aes(y = cumulative_percentage, label = paste0(round(cumulative_percentage, 2), "%")),
            hjust = 1, vjust = 0.5, color = "black", size = 3) +
  coord_flip() +
  labs(title = "Pareto Chart for Social Support",
       x = "Social Support",
       y = "Frequency / Cumulative Percentage")