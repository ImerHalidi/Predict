library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)

measurements<-X407af7079c9711633e3c9dcd773782023eff30f5_2_
head(measurements)
measurements$chatgpt_p1<-factor(measurements$chatgpt_p1, levels=c("B","N","M"), ordered=T)
measurements$chatgpt_p2<-factor(measurements$chatgpt_p2, levels=c("B","N","M"), ordered=T)
measurements$chatgpt_p3<-factor(measurements$chatgpt_p3, levels=c("B","N","M"), ordered=T)

measurements$deepseek_p1<-factor(measurements$deepseek_p1, levels=c("B","N","M"), ordered=T)
measurements$deepseek_p2<-factor(measurements$deepseek_p2, levels=c("B","N","M"), ordered=T)
measurements$deepseek_p3<-factor(measurements$deepseek_p3, levels=c("B","N","M"), ordered=T)

measurements$copilot_p1<-factor(measurements$copilot_p1, levels=c("B","N","M"), ordered=T)
measurements$copilot_p2<-factor(measurements$copilot_p2, levels=c("B","N","M"), ordered=T)
measurements$copilot_p3<-factor(measurements$copilot_p3, levels=c("B","N","M"), ordered=T)

measurements$diagnosis<-factor(measurements$diagnosis, levels=c("B","N","M"), ordered=T)
str(measurements)

long <- measurements %>%
  pivot_longer(
    cols = chatgpt_p1:copilot_p3,
    names_to = "agent_prompt",
    values_to = "response"
  ) %>%
  filter(!is.na(response))

long$agent <- sub("_p.*", "", long$agent_prompt)
long$prompt <- paste0("P", sub(".*_p", "", long$agent_prompt))

ai_cols <- c(
  "chatgpt_p1","chatgpt_p2","chatgpt_p3",
  "deepseek_p1","deepseek_p2","deepseek_p3",
  "copilot_p1","copilot_p2","copilot_p3"
)

long <- do.call(rbind, lapply(ai_cols, function(col) {
  
  data.frame(
    id = measurements$id,
    diagnosis = measurements$diagnosis,
    agent_prompt = col,
    response = measurements[[col]]
  )
  
}))

long <- long %>%
  filter(!is.na(response))

long$agent <- sub("_p.*", "", long$agent_prompt)
long$prompt <- paste0("P", sub(".*_p", "", long$agent_prompt))

head(long)

library(ggplot2)

#Compare prompts within each agent
ggplot(long,
       aes(x = prompt,
           fill = response)) +
  geom_bar(position = "fill") +
  facet_wrap(~agent) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()

#Accuracy against diagnosis
long$correct <- long$response == long$diagnosis

ggplot(long,
       aes(x = interaction(agent,prompt),
           fill = correct)) +
  geom_bar(position = "fill") +
  coord_flip() +
  scale_fill_manual(
    values = c(
      "TRUE" = "forestgreen",
      "FALSE" = "firebrick"
    )
  ) +
  theme_bw()


#Confusion matrix heatmap
conf <- as.data.frame(
  table(
    long$agent,
    long$prompt,
    long$diagnosis,
    long$response
  )
)

names(conf) <- c(
  "agent",
  "prompt",
  "diagnosis",
  "response",
  "n"
)

ggplot(conf,
       aes(x = diagnosis,
           y = response,
           fill = n)) +
  geom_tile() +
  geom_text(aes(label = n)) +
  facet_grid(agent ~ prompt) +
  scale_fill_gradient(
    low = "white",
    high = "darkblue"
  ) +
  theme_bw()