library(tidyverse)
x <- read.delim("data/predict-election-contest.txt", header = FALSE)|> 
  arrange(V1) |> 
  mutate(entry=1:n())
x |> 
  ggplot(aes(factor(entry), V1, ymin=V2, ymax=V3))+
  geom_errorbar() +
  geom_point() +
  geom_hline(yintercept = 51, lty=2) +
  ylim(c(42,55)) + 
  ylab("Democrat senators") +
  xlab("Entry") +
  labs(title="2022 Senate race predictions",
       subtitle="from Intro to Data Science students") +
  coord_flip() +
  theme_bw()

  