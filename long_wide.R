setwd("C:/Training/Academy/Statistics (Python)/Datasets/")
library(tidyverse)

quality <- read.csv("quality.csv", stringsAsFactors = T)

qual_long <- quality %>% 
                pivot_longer(-c(Sno), names_to = "category", values_to = "val")

quality2 <- read.csv("quality2.csv", stringsAsFactors = T)

qual2_long <- quality2 %>% 
  pivot_longer(-c(Id, Group), names_to = "category", values_to = "val")


qual_wide <- qual2_long %>% 
                pivot_wider(names_from = "category", values_from = "val")
