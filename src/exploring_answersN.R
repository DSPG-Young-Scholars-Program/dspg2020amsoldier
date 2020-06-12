library(readxl)
library(tidyverse)
library(data.table)
library(stringi)


ans = read_xlsx("AMS032N_answers.xlsx")
cat("There are", nrow(ans), "responses to this survey.")

ans$age= factor(ans$R11,
                levels = c(1, 2, 3,4,5,6,7,0),
                labels = c("19-", "20", "21-24","25-27","28-29","30-34","35+","NA"))

age_barplot = ggplot(ans, aes(x=age)) +geom_bar(binwidth) +ggtitle('Barplot of Age Buckets')
age_barplot


ans$edu = factor(ans$R12,
                 levels = c(1, 2, 3,4,5,6,7,8,9,10,0),
                 labels = c("< 4TH GRADE","4TH GRADE", "5TH GRADE", "6TH GRADE",
                            "7TH GRADE", "8TH GRADE", "SOME HIGH/TRADE SCHOOL",
                            "HIGH SCHOOL", "SOME COLLEGE",
                            "COLLEGE", "NA"))
edu_barplot = ggplot(ans, aes(x=edu)) +geom_bar() +ggtitle('Barplot of Education Buckets')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
edu_barplot

edu_barplot1 = ggplot(ans, aes(x=edu, fill = age)) +geom_bar(position='fill') +ggtitle('Barplot of Education Buckets')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
edu_barplot1

ans$edu2 = factor(ans$R13,
                  levels = c(1, 2, 3,4,5,6,7,8,9,10,0),
                  labels = c("< 4TH GRADE","4TH GRADE", "5TH GRADE", "6TH GRADE",
                             "7TH GRADE", "8TH GRADE", "SOME HIGH/TRADE SCHOOL",
                             "HIGH SCHOOL", "SOME COLLEGE",
                             "COLLEGE", "NA"))
edu_barplot2 = ggplot(ans, aes(x=edu, fill = age)) +geom_bar(position='fill') +ggtitle('Barplot of Education Buckets')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
edu_barplot2

ans$enlist = factor(ans$R14,
                    levels = c(1,2,3,0),
                    labels = c("DRAFTED","VOLUNTEERED", "NATIONAL GUARD", "NA"))
enlist_barplot = ggplot(ans, aes(x=enlist)) +geom_bar(aes(y = ..prop.., group = 1)) +
  ggtitle('Barplot of How Soldiers were enlisted')
enlist_barplot
enlist_by_Age_barplot = ggplot(ans, aes(x=enlist)) +geom_bar(aes(y = ..prop.., group = 1)) +
  facet_wrap(~age)+
  ggtitle('Barplot of How Soldiers were enlisted')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
enlist_by_Age_barplot

enlist_age_prop_barplot = ggplot(ans, aes(x=enlist, fill = age)) +geom_bar(position = "fill") +
  ggtitle('Barplot of How Soldiers were enlisted')
enlist_age_prop_barplot

#age_barplot+facet_wrap(.~edu)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
