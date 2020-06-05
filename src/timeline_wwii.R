# WWII Timeline ------------------------------------------
# install.packages("timelineS")
# install.packages("vistime")
library(timelineS)
library(data.table)
library(vistime)

# we need to create a df with:
# first column: event names
# second column: dates of date class

# Call events into df
df <- data.frame(read.csv("https://raw.githubusercontent.com/DSPG-Young-Scholars-Program/dspg2020amsoldier/master/data/dates_race_wwii.csv", sep = ","))
df <- df[-c(25:27),]
df$group <- c("Civil Rights", "Intersection", "Intersection", "Civil Rights", "Military",
              "Military", "Military", "Military", "Military", "Military", "Civil Rights",
              "Civil Rights", "Civil Rights", "Intersection", "Intersection",
              "Civil Rights", "Civil Rights","Civil Rights", "Intersection", "Civil Rights",
              "Intersection", "Civil Rights", "Intersection", "Intersection")
df$End <- df$Date

df[1,3] <- "06/22/1943"
df[2,3] <- "12/26/1944"
df[11,3] <- "08/02/1943"
df[15,3] <- "03/20/1946"
df[21,3] <- "04/06/1945"
df[23,3] <- "09/02/1945"


#make 'Date' and 'End' Date class
df$Date <- as.Date(df$Date, "%m/%d/%Y")
df$End <- as.Date(df$End, "%m/%d/%Y")


#DISPLAY THE TIMELINE! 
#note: won't display all events if 'End' column is passed to the function
#      only name and one date should be passed.
# timelineS(df[,1:2], main = "Significant Events Surrounding 1943 Survey", buffer.days = 360, 
          # label.position = c(2,2), label.length = c(0.2, 0.7, 0.7, 1), label.angle=45,
          # label.cex = 0.7)

#label.position: 1= below dot, 2=left of dot, 3=above dot, 4=right of dot

#IDEA: add date for survey collection highlight in Red/do some color coding?
#IDEA#2: color code on the basis of events of oppression versus events of resistence? 
#        there may be some grey area in defining this



# Timeline alternatives
# use timeline pacakge which uses ggplot: https://rdrr.io/cran/timeline/man/timeline.html
# use ggplot2: https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/
# interactive with shiny: https://daattali.com/shiny/timevis-demo/
# with vistime: https://cran.r-project.org/web/packages/vistime/vignettes/vistime-vignette.html

# Vistime Timeline --------------------------------------------

vistime(df, start = "Date", end = "End", events = "Event.Name", optimize_y = TRUE, 
        title = "Race and WWII", show_labels = FALSE, group = "group")

