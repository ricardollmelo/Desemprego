if (!require("devtools")) install.packages("devtools")
devtools::install_github("PMassicotte/gtrendsR")

library(gtrendsR)
library(tidyverse)
library(lubridate)

data = gtrends(keyword = c("seguro desemprego", 'emprego'),
               geo = "BR", time='all', onlyInterest=TRUE)

seguro_desemprego = data$interest_over_time %>%
  filter(keyword == 'seguro desemprego') %>%
  mutate(mes = floor_date(date, "month")) %>%
  group_by(mes) %>%
  summarize(interesse = mean(hits)) %>%
  mutate(dates = as.Date(mes)) %>%
  select(dates, interesse)

ggplot(seguro_desemprego) + geom_line(aes(x = dates, y = interesse), size = 0.8) +
  theme_bw() +
  #    geom_rect(data=recessions, aes(xmin=Peak,
  #                                   xmax=Trough, ymin=-Inf, ymax=+Inf), fill='gray', alpha=0.2) +
  labs(x = "", y = "") + theme(aspect.ratio=1) +
  #ggtitle(countries[j]) +
  theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=24) )

