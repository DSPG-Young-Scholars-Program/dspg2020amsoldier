library(readxl)
library(tidyverse)
library(data.table)
library(stringi)


ans_n = read_xlsx("AMS032N_answers.xlsx")
cat("There are", nrow(ans_n), "responses to this survey.")

ans_n$age= factor(ans_n$R11,
                levels = c(1, 2, 3,4,5,6,7,0),
                labels = c("<=19", "20", "21-24","25-27","28-29","30-34","35+",NA))

<<<<<<< Updated upstream
age_barplot = ggplot(ans_n, aes(x=age)) +geom_bar() +ggtitle('Barplot of Age Buckets Black Soldiers')
=======
age_barplot = ggplot(ans, aes(x=age)) +geom_bar() +ggtitle('Barplot of Age Buckets')
>>>>>>> Stashed changes
age_barplot


ans_n$edu = factor(ans_n$R12,
                 levels = c(1, 2, 3,4,5,6,7,8,9,10,0),
                 labels = c("< 4TH GRADE","4TH GRADE", "5TH GRADE", "6TH GRADE",
                            "7TH GRADE", "8TH GRADE", "SOME HIGH/TRADE SCHOOL",
                            "HIGH SCHOOL", "SOME COLLEGE",
                            "COLLEGE", NA))
edu_barplot = ggplot(ans_n, aes(x=edu)) +geom_bar() +ggtitle('Barplot of Black Soldiers Education Buckets')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
edu_barplot

edu_barplot1 = ggplot(ans_n, aes(x=edu, fill = age)) +geom_bar(position='fill') +ggtitle('Barplot of Education Buckets')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
edu_barplot1

ans_n$edu2 = factor(ans_n$R13,
                  levels = c(1, 2, 3,4,5,6,7,8,9,10,0),
                  labels = c("< 4TH GRADE","4TH GRADE", "5TH GRADE", "6TH GRADE",
                             "7TH GRADE", "8TH GRADE", "SOME HIGH/TRADE SCHOOL",
                             "HIGH SCHOOL", "SOME COLLEGE",
                             "COLLEGE", NA))
edu_barplot2 = ggplot(ans_n, aes(x=edu, fill = age)) +geom_bar(position='fill') +ggtitle('Barplot of Education Buckets')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
edu_barplot2

ans_n$enlist = factor(ans_n$R14,
                    levels = c(1,2,3,0),
                    labels = c("DRAFTED","VOLUNTEERED", "NATIONAL GUARD", NA))
enlist_barplot = ggplot(ans_n, aes(x=enlist)) +geom_bar(aes(y = ..prop.., group = 1)) +
  ggtitle('Barplot of How Soldiers were enlisted')
enlist_barplot
enlist_by_Age_barplot = ggplot(ans_n, aes(x=enlist)) +geom_bar(aes(y = ..prop.., group = 1)) +
  facet_wrap(~age)+
  ggtitle('Barplot of How Soldiers were enlisted')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
enlist_by_Age_barplot

enlist_age_prop_barplot = ggplot(ans_n, aes(x=enlist, fill = age)) +geom_bar(position = "fill") +
  ggtitle('Barplot of How Soldiers were enlisted')
enlist_age_prop_barplot

#age_barplot+facet_wrap(.~edu)+theme(axis.text.x = element_text(angle = 45, hjust = 1))
<<<<<<< Updated upstream

# R135 is a finer breakdown of reasons why
# ans$outfits = factor(ans$R135,
#                     levels = c( 0, 11:14, 21:24, 31:36, 41:44, 51,52),
#                     labels = c("NA" , rep("Seperated", 4), rep("Together",4),
#                                rep("Doesn't Matter",6), rep("Undecided",4),
#                                "Seperated", "NA"))

ans_n$outfits = factor(ans_n$R134,
                     levels = c( 0, 1:5),
                     labels = c("NA" , "Seperated", "Together",
                                "Doesn't Matter", "Undecided", NA))
outfit_barplot = ggplot(ans_n, aes(x=outfits)) +geom_bar(aes(y = ..prop.., group = 1)) +
  ggtitle("Barplot of Black Soldiers' Opinions on Outfits")
outfit_barplot

outfit_barplot+facet_grid(~age)+theme(axis.text.x = element_text(angle = 45, hjust = 1))

outfit_barplot+facet_grid(~edu)+theme(axis.text.x = element_text(angle = 45, hjust = 1))

outfit_barplot+facet_grid(~enlist)+theme(axis.text.x = element_text(angle = 45, hjust = 1))

string ="01. US UNSPECIFIED 02. FOREIGN ALLY 11. MAINE 12. NEW HAMPSHIRE 13. VERMONT 14. MASSACHUSETTS 15. RHODE ISLAND 16. CONNECTICUT 21. NEW YORK 22. NEW JERSEY 23. PENNSYLVANIA 31. OHIO 32. INDIANA 33. ILLINOIS 34. MICHIGAN 35. WISCONSIN 41. MINNESOTA 42. IOWA 43. MISSOURI 44. NORTH DAKOTA 45. SOUTH DAKOTA 46. NEBRASKA 47. KANSAS 51. DELAWARE 52. MARYLAND 53. DC. 54. VIRGINIA 55. WEST VIRGINIA 56. NORTH CAROLINA 57. SOUTH CAROLINA 58. GEORGIA 59. FLORIDA 61. KENTUCKY 62. TENNESSEE 63. ALABAMA 64. MISSISSIPPI 71. ARKANSAS 72. LOUISIANA 73. OKLAHOMA 74. TEXAS 81. MONTANA 82. IDAHO 83. WYOMING 84. COLORADO 85. NEW MEXICO 86. ARIZONA 87. UTAH 88. NEVADA 91. WASHINGTON 92. OREGON 93. CALIFORNIA 00. NA"

state_codes = unlist(stri_extract_all_regex(string, "[:digit:]+"))
state = unlist(stri_extract_all_regex(string, "[:alpha:]+.[:alpha:]+|NA|DC"))

ans_n$state = factor(ans_n$R47,
                   levels = state_codes,
                   labels = state)

ans_n$community = factor(ans_n$R48,
                       levels = c(0:5),
                       labels = c(NA,"Farm", "Small Town", "Town" ,"City", "Large City"))

ans_n %>% count(state, sort =T) %>% na.omit() %>%
  ggplot() + geom_bar(aes(reorder(state,-n),n), stat = 'identity')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
=======
ans$outfits = factor(ans$R135,
                    levels = c( 0, 11:14, 21:24, 31:36, 41:44, 51,52),
                    labels = c("NA" , rep("Seperated", 4), rep("Together",4),
                               rep("Doesn't Matter",6), rep("Undecided",4),
                               "Seperated", "NA"))
outfit_barplot = ggplot(ans, aes(x=outfits)) +geom_bar(aes(y = ..prop.., group = 1)) +
  ggtitle("Barplot of Black Soldiers' Opinions on Outfits")
outfit_barplot
>>>>>>> Stashed changes
