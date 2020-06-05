# WWII Timeline ------------------------------------------
# install.packages("timelineS")
library(timelineS)
library(data.table)

# we need to create a df with:
# first column: event names
# second column: dates of date class

# Call events into df
df <- data.frame(read.csv("https://raw.githubusercontent.com/DSPG-Young-Scholars-Program/dspg2020amsoldier/master/data/dates_race_wwii.csv", sep = ","))

#make 'Date' and 'End' Date class
df$Date <- as.Date(df$Date, "%m/%d/%Y")
df$End <- as.Date(df$End, "%m/%d/%Y")


#DISPLAY THE TIMELINE! 
#note: won't display all events if 'End' column is passed to the function
#      only name and one date should be passed.
timelineS(df[,1:2], main = "Significant Events Surrounding 1943 Survey", buffer.days = 360, 
          label.position = c(2,2), label.length = c(0.2, 0.7, 0.7, 1), label.angle=45,
          label.cex = 0.7)

#label.position: 1= below dot, 2=left of dot, 3=above dot, 4=right of dot

#IDEA: add date for survey collection highlight in Red/do some color coding?
#IDEA#2: color code on the basis of events of oppression versus events of resistence? 
#        there may be some grey area in defining this



# Timeline alternatives
# use timeline pacakge which uses ggplot: https://rdrr.io/cran/timeline/man/timeline.html
# use ggplot2: https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/
# interactive with shiny: https://daattali.com/shiny/timevis-demo/
# with vistime: https://cran.r-project.org/web/packages/vistime/vignettes/vistime-vignette.html