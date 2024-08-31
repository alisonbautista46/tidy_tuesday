
---
 
title: "Tidy Tuesday - Spam"
 
author: "Alison Bautista"
4
 
date: "2023-08-15"
5
 
output: html_document
6
 
---
7
 
8
 
```{r setup, include=FALSE}
9
 
knitr::opts_chunk$set(echo = TRUE)
10
 
11
 
library(tidyverse)
12
 
library(tidytuesdayR)
13
 
library(scales)
14
 
library(ggtext)
15
 
library(showtext)
16
 
library(tidytext)
17
 
library(data.table)
18
 
library(paletteer)
19
 
library(extrafont)
20
 
library(RColorBrewer)
21
 
library(cowplot)
22
 
library(magick)
23
 
library(usethis)
24
 
```
25
 
26
 
```{r}
27
 
tuesdata <- tidytuesdayR::tt_load(2023, week = 33)
28
 
29
 
spam <- tuesdata$spam
30
 
31
 
spam
32
 
#remove the first column
33
 
#spam <- spam[,-1]
34
 
35
 
```
36
 
```{r}
 
spam_data <- melt(spam, id.vars = "yesno", variable.factor = FALSE, value.factor = FALSE)
 
spam_data
 
```
40
 
```{r}
41
 
renamed_spam_data <- spam_data %>%
42
 
  factor(variable, 
43
 
    levels = c('crl.tot','dollar', 'bang', 'money', 'n000'), labels = c("Capitalized Letter", "$$", "!!", '"money"', '"000"'))
44
 
45
 
renamed_spam_data
46
 
```
47
 
48
 
49
 
50
 
```{r}
51
 
df <- spam_data %>% 
52
 
  filter(value!= 0) %>% 
53
 
  group_by(variable, yesno) %>%
54
 
  mutate(yesno = case_when(
55
 
    yesno == "y" ~ "Spam",
56
 
    yesno == "n" ~ "Not Spam"),
57
 
    variable = case_when(
58
 
      variable == "crl.tot" ~ "Capitalized Letter",
59
 
      variable == "dollar" ~"$$",
60
 
      variable == "bang" ~ "!!",
61
 
      variable == "money" ~ '"money"',
62
 
      variable == "n000" ~ '"000"',
63
 
      variable == "make" ~ '"make"'
64
 
    )) %>% 
65
 
  summarise(sum_value = sum(value), count = n()) %>%
66
 
  distinct() %>%
67
 
  mutate(perc_occurances = paste(round((count/sum(count)*100), 2), "%"),
68
 
         count_occurances = paste(count, "emails"),
69
 
         perc_emails = sum(count)/ nrow(spam))
70
 
71
 
df
72
 
```
73
 
74
 
75
 
76
 
77
 
```{r fig.height= 4, fig.width= 8}
78
 
79
 
spam_plot <- df %>%
80
 
  ggplot(aes(x = count, y = reorder(variable, -count), fill = yesno)) + 
81
 
  geom_col(color = 'grey') +
82
 
  scale_fill_manual(values=c('midnightblue', 'rosybrown2')) +
83
 
  geom_text(aes(group = yesno, 
84
 
                label = (ifelse(yesno == "Spam", count_occurances, ""))),
85
 
                hjust = 1.1, 
86
 
                nudge_x = -2,
87
 
                size = 2, 
88
 
                fontface = "bold") + 
89
 
  labs(title = "<span style='font-size:15pt'>you've got...</span>
90
 
       <span style='font-size:32pt'>SPAM</span>",
91
 
       subtitle = "Characters and words that cause emails to be flagged as Spam.\nTotal length of columns indicate how many emails out of the 4600 contained the specified characters, \nand how many emails which contain those characters are flagged as spam",
92
 
       caption = "Source: Vincent Arel-Bundock's Rdatasets- Spam Emails | Data Viz by Alison Bautista") +
93
 
  xlab("Total Number of emails") +
94
 
  ylab("") +
95
 
  theme(panel.background = element_rect(fill = 'midnightblue'),
96
 
        panel.grid.major = element_line(color = 'midnightblue'),
97
 
        panel.grid.minor = element_line(color = 'midnightblue'),
98
 
        plot.background = element_rect(fill = 'midnightblue'),
99
 
        legend.background = element_rect(fill = 'midnightblue'),
100
 
        legend.title = element_blank(),
101
 
        legend.text = element_text(color = "grey"),
102
 
        plot.title = element_markdown(family = "PT Serif", color = "gold", face = "bold"),
103
 
        plot.subtitle = element_text(color = 'grey', size = 10, family = "Spot Mono"),
104
 
        plot.caption = element_text(color = 'grey', size = 8, family = "Spot Mono"),
105
 
        axis.title.x = element_text(color = "gold", family = "PT Serif"),
106
 
        axis.text = element_text(color = 'grey', face = "bold", size = 9, family = "PT Serif")
107
 
        )
108
 
109
 
spam_plot
110
 
```
111
 
112
 
```{r}
113
 
ggsave(spam_plot, filename = "spam_plot.jpeg", device = jpeg)
114
 
```
115
 
116
 
