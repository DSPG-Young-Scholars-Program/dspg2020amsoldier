library(readxl)
library(tidyverse)
library(data.table)
library(stringi)
library(urbnmapr)
library(usmap)


ans = read_xlsx("AMS032W_answers.xlsx")
cat("There are", nrow(ans), "responses to this survey.")

ans$age= factor(ans$`Q.1.`,
                             levels = c(1, 2, 3,4,5,6,7,0),
                labels = c("<=19", "20", "21-24","25-27","28-29","30-34","35+",NA))
age_barplot = ggplot(ans, aes(x=age)) +geom_bar() +ggtitle('Barplot of White Soldiers Age Buckets')
age_barplot

ans$edu = factor(ans$`Q.2.`,
                 levels = c(1, 2, 3,4,5,6,7,8,9,10,0),
                 labels = c("< 4TH GRADE","4TH GRADE", "5TH GRADE", "6TH GRADE",
                            "7TH GRADE", "8TH GRADE", "SOME HIGH/TRADE SCHOOL",
                            "HIGH SCHOOL", "SOME COLLEGE",
                            "COLLEGE", NA))
edu_barplot = ggplot(ans, aes(x=edu)) +geom_bar() +ggtitle('Barplot of White Soldiers Education Buckets')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
edu_barplot
edu_barplot2 = ggplot(ans, aes(x=edu, fill = age)) +geom_bar(position='fill') +ggtitle('Barplot of White Soldiers Education Buckets')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
edu_barplot2

ans$enlist = factor(ans$`Q.3.`,
                 levels = c(1,2,3,0),
                 labels = c("DRAFTED","VOLUNTEERED", "NATIONAL GUARD", NA))
enlist_barplot = ggplot(ans, aes(x=enlist)) +geom_bar(aes(y = ..prop.., group = 1)) +
  ggtitle('Barplot of How White Soldiers were enlisted')
enlist_barplot
enlist_by_Age_barplot = ggplot(ans, aes(x=enlist)) +geom_bar(aes(y = ..prop.., group = 1)) +
  facet_wrap(~age)+
  ggtitle('Barplot of How White Soldiers were enlisted')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
enlist_by_Age_barplot


enlist_age_prop_barplot = ggplot(ans, aes(x=enlist, fill = age)) +geom_bar(position = "fill") +
  ggtitle('Barplot of How White Soldiers were enlisted')
enlist_age_prop_barplot

# age_barplot+facet_wrap(~edu)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
#edu_barplot+facet_wrap(~age) not really helpful

#first need to conver q13 column into characters and then factor because R treats 00 as 0, 01 as 1,, etc.

#Q.63.A. is a finer breakdown of reasons why
ans$outfits = factor(ans$Q.63.,
                     levels = c( 0, 1:5),
                     labels = c(NA , "Seperated", "Together",
                                "Doesn't Matter", "Undecided", NA))
outfit_barplot = ggplot(ans, aes(x=outfits)) +geom_bar(aes(y = ..prop.., group = 1)) +
  ggtitle("Barplot of White Soldiers' Opinions on Outfits")
outfit_barplot
outfit_barplot+facet_grid(~age)+theme(axis.text.x = element_text(angle = 45, hjust = 1))

outfit_barplot+facet_grid(~edu)+theme(axis.text.x = element_text(angle = 45, hjust = 1))

outfit_barplot+facet_grid(~enlist)+theme(axis.text.x = element_text(angle = 45, hjust = 1))

outfit_barplot+facet_wrap(~state)+theme(axis.text.x = element_text(angle = 45, hjust = 1))



ggplot(ans, aes(x=outfits, fill=edu)) +geom_bar(position="fill") +
  ggtitle("Barplot of White Soldiers' Opinions on Outfits")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



string ="01. US UNSPECIFIED 02. FOREIGN ALLY 11. MAINE 12. NEW HAMPSHIRE 13. VERMONT 14. MASSACHUSETTS 15. RHODE ISLAND 16. CONNECTICUT 21. NEW YORK 22. NEW JERSEY 23. PENNSYLVANIA 31. OHIO 32. INDIANA 33. ILLINOIS 34. MICHIGAN 35. WISCONSIN 41. MINNESOTA 42. IOWA 43. MISSOURI 44. NORTH DAKOTA 45. SOUTH DAKOTA 46. NEBRASKA 47. KANSAS 51. DELAWARE 52. MARYLAND 53. DC. 54. VIRGINIA 55. WEST VIRGINIA 56. NORTH CAROLINA 57. SOUTH CAROLINA 58. GEORGIA 59. FLORIDA 61. KENTUCKY 62. TENNESSEE 63. ALABAMA 64. MISSISSIPPI 71. ARKANSAS 72. LOUISIANA 73. OKLAHOMA 74. TEXAS 81. MONTANA 82. IDAHO 83. WYOMING 84. COLORADO 85. NEW MEXICO 86. ARIZONA 87. UTAH 88. NEVADA 91. WASHINGTON 92. OREGON 93. CALIFORNIA 00. NA"

state_codes = unlist(stri_extract_all_regex(string, "[:digit:]+"))
state = unlist(stri_extract_all_regex(string, "[:alpha:]+.[:alpha:]+|NA|DC"))

ans$state = factor(ans$Q.13.,
                   levels = state_codes,
                   labels = state)

ans$community = factor(ans$Q.14.,
                       levels = c(0:5),
                       labels = c(NA,"Farm", "Small Town", "Town" ,"City", "Large City"))


ans %>%
  ggplot(aes(x=state)) +geom_bar() +
  ggtitle("Barplot of White Soldiers' Opinions on Outfits")+theme(axis.text.x = element_text(angle = 45, hjust = 1))

ans %>% count(state, sort =T) %>% na.omit() %>%
ggplot() + geom_bar(aes(reorder(state,-n),n), stat = 'identity')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
