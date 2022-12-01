library(tidyverse)
library(hacksaw)

headerraw <- read.csv("M116.ssv", sep=" ", nrows = 2, header = F)

nohead <- read.csv("M116.ssv", sep=" ", skip = 2, header = F)

headers <- headerraw %>%
  add_column(none = "none",
             .after = "V37") %>%
  mutate(V66 = "none") %>%
  select(c(V1:V66))

colnames(nohead) <- headers[1,]

nohead <- nohead %>%
  select(!c(none))

save(headers, nohead, file = "M116.RData")