library(tidyverse)
library(haven)
library(sjlabelled)
library(labelled) 
library(surveytoolbox) # install with devtools::install_github("martinctc/surveytoolbox")

file_path <- "data/original/General Social Survey, 1991.SAV"
source_data_hv <- haven::read_sav(file_path)
source_data_hv$MARITAL %>% attr('labels')
source_data_hv$MARITAL %>% attr('label')

vct1 <- c("a", "b", "c")
vct2 <- c(one = "a", two = "b", three = "c")

lbld <- haven::labelled(x = vct1, labels = vct2, label = "test")

s1 <- labelled(c("M", "M", "F"), c(Male = "M", Female = "F"))
s2 <- labelled(c(1, 1, 2), c(Male = 1, Female = 2))

tdf <- tibble(var1 = lbld)
