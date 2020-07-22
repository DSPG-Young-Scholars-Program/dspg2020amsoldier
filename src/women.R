library(tidyverse)
library(ggthemes)

s195b <- read.csv("data/working/AMS195B_answers.csv")
s195c <- read.csv("data/working/AMS195C_answers.csv")
s35 <- read.csv("data/working/AMS0035_answers.csv")

colors <- c("#232d4b","#2c4f6b","#0e879c","#60999a","#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200","#fdfdfd")

# Women ----------------------------------------------------------

# 195B
# 1 - yes, 2 - no, 0 - no answer
# 1 - very strongly, 2 - somewhat strongly, 3 - not strongly, 0 - no answer
# 195C
# 1. NOT STRONGLY AT ALL 2. NOT SO STRONGLY 3. FAIRLY STRONGLY 4. VERY STRONGLY 0. NO ANSWER
# 1. AGREE 2. DISAGREE 3. UNDECIDED 0. NO ANSWER

# S195B.Q14A. IN YOUR OPINION IS IT NECESSARY FOR THE WAR EFFORT TO HAVE WOMEN IN THE ARMY? 1. YES 2. NO 0. NO ANSWER

s195b$Q.14A. <- recode_factor(s195b$Q.14A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b %>%
  group_by(Q.14A.) %>%
  count() %>%
  ggplot(aes(x = Q.14A., y = n)) +
  geom_bar(stat="identity", aes(fill = Q.14A.)) +
  ggtitle("IN YOUR OPINION IS IT NECESSARY FOR THE \nWAR EFFORT TO HAVE WOMEN IN THE ARMY?") +
  theme_minimal() +
  scale_fill_manual(values = colors)

# S195B.Q15A. SUPPOSE A GIRL FRIEND OF YOURS WAS CONSIDERING JOINING THE WAC, WOULD YOU ADVISE HER TO JOIN OR NOT TO JOIN? 1. YES 2. NO 0. NO ANSWER 
s195b$Q.15A. <- recode_factor(s195b$Q.15A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b %>%
  group_by(Q.15A.) %>%
  count() %>%
  ggplot(aes(x = Q.15A., y = n)) +
  geom_bar(stat="identity", aes(fill = Q.15A.))+
  ggtitle("SUPPOSE A GIRL FRIEND OF YOURS WAS CONSIDERING JOINING THE\nWAC, WOULD YOU ADVISE HER TO JOIN OR NOT TO JOIN?") +
  theme_minimal() +
  scale_fill_manual(values = colors)

# S195B.Q16A. IN YOUR OPINION ARE THE JOBS WHICH WOMEN IN THE WAC DO LESS IMPORTANT THAN THE JOBS WHICH ARE DONE BY MEN IN THE ARMY WHO ARE NOT ON COMBAT DUTY? 1. YES 2. NO 0. NO ANSWER
s195b$Q.16A. <- recode_factor(s195b$Q.16A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b %>%
  group_by(Q.16A.) %>%
  count() %>%
  ggplot(aes(x = Q.16A., y = n)) +
  geom_bar(stat="identity", aes(fill = Q.16A.))+
  ggtitle("IN YOUR OPINION ARE THE JOBS WHICH WOMEN IN THE WAC DO \nLESS IMPORTANT THAN THE JOBS WHICH ARE DONE BY \nMEN IN THE ARMY WHO ARE NOT ON COMBAT DUTY?") +
  theme_minimal() +
  scale_fill_manual(values = colors)

# S195B.Q17A. IF YOU HAD A SISTER, 21 YEARS OR OLDER, WOULD YOU LIKE TO SEE HER JOIN THE WAC OR NOT? 1. YES 2. NO 0. NO ANSWER
s195b$Q.17A. <- recode_factor(s195b$Q.17A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b %>%
  group_by(Q.17A.) %>%
  count() %>%
  ggplot(aes(x = Q.17A., y = n)) +
  geom_bar(stat="identity", aes(fill = Q.17A.))+
  ggtitle("IF YOU HAD A SISTER, 21 YEARS OR OLDER, WOULD \nYOU LIKE TO SEE HER JOIN THE WAC OR NOT?") +
  theme_minimal() +
  scale_fill_manual(values = colors)

# S195B.Q18A. CAN A WOMAN DO MORE FOR HER COUNTRY IN THE WAC THAN SHE CAN BY WORKING IN A WAR INDUSTRY? 1. YES 2. NO 0. NO ANSWER
s195b$Q.18A. <- recode_factor(s195b$Q.18A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b %>%
  group_by(Q.18A.) %>%
  count() %>%
  ggplot(aes(x = Q.18A., y = n)) +
  geom_bar(stat="identity", aes(fill = Q.18A.))+
  ggtitle("CAN A WOMAN DO MORE FOR HER COUNTRY IN THE \nWAC THAN SHE CAN BY WORKING IN A WAR INDUSTRY?") +
  theme_minimal() +
  scale_fill_manual(values = colors)


# S195B.Q19A. WILL THE TRAINING A WOMAN GETS IN THE WAC BE USEFUL IN CIVILIAN LIFE? 1. YES 2. NO 0. NO ANSWER
s195b$Q.19A. <- recode_factor(s195b$Q.19A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b %>%
  group_by(Q.19A.) %>%
  count() %>%
  ggplot(aes(x = Q.19A., y = n)) +
  geom_bar(stat="identity", aes(fill = Q.19A.))+
  ggtitle("WILL THE TRAINING A WOMAN GETS IN THE WAC \nBE USEFUL IN CIVILIAN LIFE?") +
  theme_minimal() +
  scale_fill_manual(values = colors)

# S195B.Q20A. ARE MOST OF THE JOBS IN THE WAC INTERESTING AND AGREEABLE? 1. YES 2. NO 0. NO ANSWER
s195b$Q.20A. <- recode_factor(s195b$Q.20A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b %>%
  group_by(Q.20A.) %>%
  count() %>%
  ggplot(aes(x = Q.20A., y = n)) +
  geom_bar(stat="identity", aes(fill = Q.20A.))+
  ggtitle("ARE MOST OF THE JOBS IN THE WAC INTERESTING AND AGREEABLE?") +
  theme_minimal() +
  scale_fill_manual(values = colors)

# S195B.Q21A. IS BEING A WAC BAD FOR A GIRL'S REPUTATION? 1. YES 2. NO 0. NO ANSWER
s195b$Q.21A. <- recode_factor(s195b$Q.21A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b %>%
  group_by(Q.21A.) %>%
  count() %>%
  ggplot(aes(x = Q.21A., y = n)) +
  geom_bar(stat="identity", aes(fill = Q.21A.))+
  ggtitle("IS BEING A WAC BAD FOR A GIRL'S REPUTATION?") +
  theme_minimal() +
  scale_fill_manual(values = colors)

# S195B.Q22A. IS THE ARMY ANY PLACE FOR A GIRL TO BE? 1. YES 2. NO 0. NO ANSWER
s195b$Q.22A. <- recode_factor(s195b$Q.22A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b %>%
  group_by(Q.22A.) %>%
  count() %>%
  ggplot(aes(x = Q.22A., y = n)) +
  geom_bar(stat="identity", aes(fill = Q.22A.))+
  ggtitle("IS THE ARMY ANY PLACE FOR A GIRL TO BE?") +
  theme_minimal() +
  scale_fill_manual(values = colors)

# S195B.Q23A. DO WAC OFFICERS DESERVE A SALUTE JUST THE SAME AS MEN OFFICERS? 1. YES 2. NO 0. NO ANSWER
s195b$Q.23A. <- recode_factor(s195b$Q.23A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b %>%
  group_by(Q.23A.) %>%
  count() %>%
  ggplot(aes(x = Q.23A., y = n)) +
  geom_bar(stat="identity", aes(fill = Q.23A.))+
  ggtitle("DO WAC OFFICERS DESERVE A SALUTE JUST THE SAME AS MEN OFFICERS?") +
  theme_minimal() +
  scale_fill_manual(values = colors)

# S195B.Q24A. DO WAC'S GET RATINGS A LOT EASIER THAN MEN? 1. YES 2. NO 0. NO ANSWER
s195b$Q.24A. <- recode_factor(s195b$Q.24A., `1` = "Yes", `2` = "No", `0` = "No Answer")
s195b %>%
  group_by(Q.24A.) %>%
  count() %>%
  ggplot(aes(x = Q.24A., y = n)) +
  geom_bar(stat="identity", aes(fill = Q.24A.))+
  ggtitle("DO WAC'S GET RATINGS A LOT EASIER THAN MEN?") +
  theme_minimal() +
  scale_fill_manual(values = colors)

# this is the same survey at a different camp I think
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
