library(tidyverse)
library(ggthemes)

s195b <- read.csv("data/working/AMS195B_answers.csv")
s195c <- read.csv("data/working/AMS195C_answers.csv")
s35 <- read.csv("data/working/AMS0035_answers.csv")

colors <- c("#232d4b",
            # "#2c4f6b",
            "#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

my_cols = c("232d4b","2c4f6b","0e879c","60999a", "9bc0c0","d1e0bf", "ebf094", "d9e12b","e6ce3a","e6a01d","e57200","a35200","fdfdfd")
my_cols = paste0('#', my_cols)

# Women ----------------------------------------------------------

# 195B
# 1 - yes, 2 - no, 0 - no answer
# 1 - very strongly, 2 - somewhat strongly, 3 - not strongly, 0 - no answer
# 195C
# 1. NOT STRONGLY AT ALL 2. NOT SO STRONGLY 3. FAIRLY STRONGLY 4. VERY STRONGLY 0. NO ANSWER
# 1. AGREE 2. DISAGREE 3. UNDECIDED 0. NO ANSWER

# S195B.Q14A. IN YOUR OPINION IS IT NECESSARY FOR THE WAR EFFORT TO HAVE WOMEN IN THE ARMY? 1. YES 2. NO 0. NO ANSWER

s195b$Q.14A. <- recode_factor(s195b$Q.14A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b$Q.14B. <- recode_factor(s195b$Q.14B., `1` = "Very Strongly", `2` = "Fairly Strongly", `3` = "Not Strongly", `0` = "No Answer")

# s195b %>%
#   group_by(Q.14A., Q.14B.) %>%
#   count() %>%
#   ggplot(aes(x = Q.14A., y = n)) +
#   geom_bar(stat="identity", aes(fill = Q.14A.)) +
#   ggtitle("IN YOUR OPINION IS IT NECESSARY FOR THE \nWAR EFFORT TO HAVE WOMEN IN THE ARMY?") +
#   facet_wrap(~Q.14B.) +
#   ylab("Number of Responses") +
#   xlab("Responses") +
#   theme_minimal() +
#   scale_fill_manual(values = colors)

s195b %>%
  ggplot(aes(x=Q.14A.)) +
  geom_bar(position = "dodge", fill = 'darkslategray4') +
  labs(title="IN YOUR OPINION IS IT NECESSARY FOR THE \nWAR EFFORT TO HAVE WOMEN IN THE ARMY?", x="Answers", y = "Count")

s195b %>%
  ggplot( aes(x=Q.14A., fill = Q.14B.)) +geom_bar(position= "fill") +
  labs(title="IN YOUR OPINION IS IT NECESSARY FOR THE \nWAR EFFORT TO HAVE WOMEN IN THE ARMY?", x="Answers Level", y = "Proportion")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = rev(my_cols[1:4]))

# S195B.Q15A. SUPPOSE A GIRL FRIEND OF YOURS WAS CONSIDERING JOINING THE WAC, WOULD YOU ADVISE HER TO JOIN OR NOT TO JOIN? 1. YES 2. NO 0. NO ANSWER
s195b$Q.15A. <- recode_factor(s195b$Q.15A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b$Q.15B. <- recode_factor(s195b$Q.15B., `1` = "Very Strongly", `2` = "Fairly Strongly", `3` = "Not Strongly", `0` = "No Answer")

s195b %>%
  ggplot(aes(x=Q.15A.)) +
  geom_bar(position = "dodge", fill = 'darkslategray4') +
  labs(title="SUPPOSE A GIRL FRIEND OF YOURS WAS CONSIDERING JOINING THE\nWAC, WOULD YOU ADVISE HER TO JOIN OR NOT TO JOIN?", x="Answers", y = "Count")

s195b %>%
  ggplot( aes(x=Q.15A., fill = Q.15B.)) +geom_bar(position= "fill") +
  labs(title="SUPPOSE A GIRL FRIEND OF YOURS WAS CONSIDERING JOINING THE\nWAC, WOULD YOU ADVISE HER TO JOIN OR NOT TO JOIN?", x="Answers Level", y = "Proportion")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = rev(my_cols[1:4]))

# S195B.Q16A. IN YOUR OPINION ARE THE JOBS WHICH WOMEN IN THE WAC DO LESS IMPORTANT THAN THE JOBS WHICH ARE DONE BY MEN IN THE ARMY WHO ARE NOT ON COMBAT DUTY? 1. YES 2. NO 0. NO ANSWER
s195b$Q.16A. <- recode_factor(s195b$Q.16A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b$Q.16B. <- recode_factor(s195b$Q.16B., `1` = "Very Strongly", `2` = "Fairly Strongly", `3` = "Not Strongly", `0` = "No Answer")

s195b %>%
  ggplot( aes(x=Q.16A., fill = Q.16B.)) +geom_bar(position= "fill") +
  labs(title="IN YOUR OPINION ARE THE JOBS WHICH WOMEN IN THE WAC DO LESS IMPORTANT \nTHAN THE JOBS WHICH ARE DONE BY MEN IN THE ARMY WHO ARE NOT ON COMBAT DUTY? ", x="Answers Level", y = "Proportion")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = rev(my_cols[1:4]))

# S195B.Q17A. IF YOU HAD A SISTER, 21 YEARS OR OLDER, WOULD YOU LIKE TO SEE HER JOIN THE WAC OR NOT? 1. YES 2. NO 0. NO ANSWER
s195b$Q.17A. <- recode_factor(s195b$Q.17A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b$Q.17B. <- recode_factor(s195b$Q.17B., `1` = "Very Strongly", `2` = "Fairly Strongly", `3` = "Not Strongly", `0` = "No Answer")

s195b %>%
  ggplot( aes(x=Q.17A., fill = Q.17B.)) +geom_bar(position= "fill") +
  labs(title="IF YOU HAD A SISTER, 21 YEARS OR OLDER, WOULD YOU LIKE TO SEE HER JOIN THE WAC OR NOT?", x="Answers Level", y = "Proportion")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = rev(my_cols[1:4]))

# S195B.Q18A. CAN A WOMAN DO MORE FOR HER COUNTRY IN THE WAC THAN SHE CAN BY WORKING IN A WAR INDUSTRY? 1. YES 2. NO 0. NO ANSWER
s195b$Q.18A. <- recode_factor(s195b$Q.18A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b$Q.18B. <- recode_factor(s195b$Q.18B., `1` = "Very Strongly", `2` = "Fairly Strongly", `3` = "Not Strongly", `0` = "No Answer")

s195b %>%
  ggplot( aes(x=Q.18A., fill = Q.18B.)) +geom_bar(position= "fill") +
  labs(title="CAN A WOMAN DO MORE FOR HER COUNTRY IN THE WAC THAN SHE CAN BY WORKING IN A WAR INDUSTRY?", x="Answers Level", y = "Proportion")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = rev(my_cols[1:4]))

# S195B.Q19A. WILL THE TRAINING A WOMAN GETS IN THE WAC BE USEFUL IN CIVILIAN LIFE? 1. YES 2. NO 0. NO ANSWER
s195b$Q.19A. <- recode_factor(s195b$Q.19A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b$Q.19B. <- recode_factor(s195b$Q.19B., `1` = "Very Strongly", `2` = "Fairly Strongly", `3` = "Not Strongly", `0` = "No Answer")

s195b %>%
  ggplot( aes(x=Q.19A., fill = Q.19B.)) +geom_bar(position= "fill") +
  labs(title="WILL THE TRAINING A WOMAN GETS IN THE WAC BE USEFUL IN CIVILIAN LIFE? ", x="Answers Level", y = "Proportion")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = rev(my_cols[1:4]))

# S195B.Q20A. ARE MOST OF THE JOBS IN THE WAC INTERESTING AND AGREEABLE? 1. YES 2. NO 0. NO ANSWER
s195b$Q.20A. <- recode_factor(s195b$Q.20A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b$Q.20B. <- recode_factor(s195b$Q.20B., `1` = "Very Strongly", `2` = "Fairly Strongly", `3` = "Not Strongly", `0` = "No Answer")


s195b %>%
  ggplot( aes(x=Q.20A., fill = Q.20B.)) +geom_bar(position= "fill") +
  labs(title="ARE MOST OF THE JOBS IN THE WAC INTERESTING AND AGREEABLE?", x="Answers Level", y = "Proportion")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = rev(my_cols[1:4]))

# S195B.Q21A. IS BEING A WAC BAD FOR A GIRL'S REPUTATION? 1. YES 2. NO 0. NO ANSWER
s195b$Q.21A. <- recode_factor(s195b$Q.21A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b$Q.21B. <- recode_factor(s195b$Q.21B., `1` = "Very Strongly", `2` = "Fairly Strongly", `3` = "Not Strongly", `0` = "No Answer")

s195b %>%
  ggplot( aes(x=Q.21A., fill = Q.21B.)) +geom_bar(position= "fill") +
  labs(title="IS BEING A WAC BAD FOR A GIRL'S REPUTATION?", x="Answers Level", y = "Proportion")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = rev(my_cols[1:4]))

# S195B.Q22A. IS THE ARMY ANY PLACE FOR A GIRL TO BE? 1. YES 2. NO 0. NO ANSWER
s195b$Q.22A. <- recode_factor(s195b$Q.22A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b$Q.22B. <- recode_factor(s195b$Q.22B.,`1` = "Very Strongly", `2` = "Fairly Strongly", `3` = "Not Strongly", `0` = "No Answer")

s195b %>%
  ggplot( aes(x=Q.22A., fill = Q.22B.)) +geom_bar(position= "fill") +
  labs(title="IS THE ARMY ANY PLACE FOR A GIRL TO BE?", x="Answers Level", y = "Proportion")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = rev(my_cols[1:4]))

# S195B.Q23A. DO WAC OFFICERS DESERVE A SALUTE JUST THE SAME AS MEN OFFICERS? 1. YES 2. NO 0. NO ANSWER
s195b$Q.23A. <- recode_factor(s195b$Q.23A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b$Q.23B. <- recode_factor(s195b$Q.23B., `1` = "Very Strongly", `2` = "Fairly Strongly", `3` = "Not Strongly", `0` = "No Answer")

s195b %>%
  ggplot( aes(x=Q.23A., fill = Q.23B.)) +geom_bar(position= "fill") +
  labs(title="DO WAC OFFICERS DESERVE A SALUTE JUST THE SAME AS MEN OFFICERS? ", x="Answers Level", y = "Proportion")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = rev(my_cols[1:4]))

# S195B.Q24A. DO WAC'S GET RATINGS A LOT EASIER THAN MEN? 1. YES 2. NO 0. NO ANSWER
s195b$Q.24A. <- recode_factor(s195b$Q.24A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b$Q.24B. <- recode_factor(s195b$Q.24B., `1` = "Very Strongly", `2` = "Fairly Strongly", `3` = "Not Strongly", `0` = "No Answer")

s195b %>%
  ggplot( aes(x=Q.24A., fill = Q.24B.)) +geom_bar(position= "fill") +
  labs(title="DO WAC'S GET RATINGS A LOT EASIER THAN MEN?", x="Answers Level", y = "Proportion")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_manual(values = rev(my_cols[1:4]))

# this is the same survey to the same people, just different second round answers but same Qs
# S195C.Q4A. IN YOUR OPINION HOW NECESSARY IS IT FOR THE WAR EFFORT TO HAVE WOMEN IN THE ARMY?  1. VERY NECESSARY 2. PRETTY NECESSARY 3. NOT SO NECESSARY 4. NOT NECESSARY AT ALL 5. UNDECIDED 0. NO ANSWER
# S195C.Q5A. SUPPOSE A GIRL FRIEND OF YOURS WAS CONSIDERING JOINING THE WAC, WOULD YOU ADVISE HER TO JOIN OR NOT TO JOIN?  1. I WOULD ADVISE HER TO JOIN 2. I WOULD ADVISE HER NOT TO JOIN 3. UNDECIDED 0. NO ANSWER
# S195C.Q6A. IN YOUR OPINION ARE THE JOBS WHICH WOMEN IN THE WAC DO MORE IMPORTANT OR LESS IMPORTANT THAN THE JOBS WHICH ARE DONE BY MEN IN THE ARMY WHO ARE NOT ON COMBAT DUTY, OR ARE THEY EQUALLY IMPORTANT?  1. THE WAC DOES MORE IMPORTANT JOBS 2. THE WAC DOES LESS IMPORTANT JOBS 3. THE JOBS ARE EQUALLY IMPORTANT 0. NO ANSWER
# S195C.Q7A. IF YOU HAD A SISTER, 21 YEARS OR OLDER, WOULD YOU LIKE TO SEE HER JOIN THE WAC OR NOT?  1. YES 2. UNDECIDED 3. NO 0. NO ANSWER
# S195C.Q8A. A WOMAN CAN DO MORE FOR HER COUNTRY IN THE WAC THAN SHE CAN DO BY WORKING IN A WAR INDUSTRY.  1. AGREE 2. DISAGREE 3. UNDECIDED 0. NO ANSWER
# S195C.Q9A. THE TRAINING A WOMAN GETS IN THE WAC WILL BE USEFUL IN CIVILIAN LIFE.  1. AGREE 2. DISAGREE 3. UNDECIDED 0. NO ANSWER
# S195C.Q10A. MOST OF THE JOBS IN THE WAC ARE INTERESTING AND AGREEABLE.  1. AGREE 2. DISAGREE 3. UNDECIDED 0. NO ANSWER
# S195C.Q11A. BEING A WAC IS BAD FOR A GIRL'S REPUTATION.  1. AGREE 2. DISAGREE 3. UNDECIDED 0. NO ANSWER
# S195C.Q12A. THE ARMY IS NO PLACE FOR A GIRL.  1. AGREE 2. DISAGREE 3. UNDECIDED 0. NO ANSWER
# S195C.Q13A. WAC OFFICERS DESERVE A SALUTE JUST THE SAME AS MEN OFFICERS.  1. AGREE 2. DISAGREE 3. UNDECIDED 0. NO ANSWER
# S195C.Q14A. WAC'S GET RATINGS A LOT EASIER THAN MEN DO.  1. AGREE 2. DISAGREE 3. UNDECIDED 0. NO ANSWER
