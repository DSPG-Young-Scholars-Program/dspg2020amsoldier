library(readxl)
library(tidyverse)
library(data.table)

ans = read_xlsx("AMS032W_answers.xlsx")
nrow(ans)
ans$age= factor(ans$`Q.1.`,
                             levels = c(1, 2, 3,4,5,6,7,0),
                labels = c("19-", "20", "21-24","25-27","28-29","30-34","35+","NA"))
age_barplot = ggplot(ans, aes(x=age)) +geom_bar() +ggtitle('Barplot of Age Buckets')
age_barplot

ans$edu = factor(ans$`Q.2.`,
                 levels = c(1, 2, 3,4,5,6,7,8,9,10,0),
                 labels = c("< 4TH GRADE","4TH GRADE", "5TH GRADE", "6TH GRADE",
                            "7TH GRADE", "8TH GRADE", "SOME HIGH/TRADE SCHOOL",
                            "HIGH SCHOOL", "SOME COLLEGE",
                            "COLLEGE", "NA"))
edu_barplot = ggplot(ans, aes(x=edu)) +geom_bar() +ggtitle('Barplot of Education Buckets')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
edu_barplot

ans$enlist = factor(ans$`Q.3.`,
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

age_barplot+facet_wrap(~edu)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
edu_barplot+facet_wrap(~age)
