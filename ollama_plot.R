library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)

olama$Answer<-factor(olama$Answer, levels=c("B", "M"))
olama$`Expected Answer`<-factor(olama$`Expected Answer`, levels=c("B", "M"))

str()

#confusion matrix heatmap
acc_class <- olama %>%
  group_by(`Expected Answer`) %>%
  summarise(
    Accuracy = mean(`correct/incorrect` == "correct")
  )

ggplot(acc_class,
       aes(x = `Expected Answer`,
           y = Accuracy,
           fill = `Expected Answer`)) +
  geom_col() +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0,1)
  ) +
  labs(
    x = "True Diagnosis",
    y = "Accuracy"
  ) +
  theme_bw()

#accuracy-by-class
conf <- as.data.frame(table(
  Expected = olama$`Expected Answer`,
  Predicted = olama$Answer
))

ggplot(conf,
       aes(x = Expected,
           y = Predicted,
           fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq),
            size = 5) +
  scale_fill_gradient(
    low = "white",
    high = "darkred"
  ) +
  labs(
    title = "Ollama Classification Confusion Matrix",
    x = "Expected Diagnosis",
    y = "Predicted Diagnosis",
    fill = "Count"
  ) +
  theme_bw()