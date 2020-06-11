# install.packages("shinythemes")
# install.packages("data.table")
# install.packages("shiny")
# install.packages("vistime")
# install.packages("plotly")

library(shiny)
library(data.table)
library(vistime)
library(shinythemes)
library(plotly)

# American Soldier Dashboard ------------------------------------


# Timeline Data ----------------------------------------------------------

df <- data.frame(read.csv("data/dates_race_wwii.csv", sep = ","))

# group the data by CR, Intersetcion, or Military Only
df$group <- c("Civil Rights", "Intersection", "Intersection", "Civil Rights", "Military",
              "Military", "Military", "Military", "Military", "Military", "Civil Rights",
              "Civil Rights", "Civil Rights", "Intersection", "Intersection",
              "Civil Rights", "Civil Rights","Civil Rights", "Intersection", "Civil Rights",
              "Intersection", "Civil Rights", "Intersection", "Intersection", "Intersection")

df$End <- df$Date

df$Date <- as.Date(df$Date, "%m/%d/%Y")
df$End <- as.Date(df$End, "%m/%d/%Y")

df[1,3] <- as.Date.character("06/22/1943", format = "%m/%d/%Y")
df[2,3] <- as.Date.character("12/26/1944", format = "%m/%d/%Y")
df[11,3] <- as.Date.character("08/02/1943", format = "%m/%d/%Y")
df[15,3] <- as.Date.character("03/20/1946", format = "%m/%d/%Y")
df[21,3] <- as.Date.character("04/06/1945", format = "%m/%d/%Y")
df[23,3] <- as.Date.character("09/02/1945", format = "%m/%d/%Y")
df[25,3] <- as.Date.character("03/31/1943", format = "%m/%d/%Y")

subset_df <- subset(df, Date >="1940-01-01" & Date <= "1942-01-01")

# Define UI ----------------------------------------------------
ui <- fluidPage(
  # theme
  theme = shinytheme("paper"),
  # title panel
  titlePanel("NEH American Soldier Dashboard"),
  # sidebar w date range, inpit box, and submit button
  sidebarLayout(position = "left",
    sidebarPanel(dateRangeInput("dates", h5("Date range")),
      textInput("question", label = "What research question suggestions do you have?"),
                 submitButton("Submit")),
  # the main panel of the whole shabang
  mainPanel(
    # main panel title
    h4("Interactive Timeline:")
    # the mf'n timeline
    )
  )
)
  
# Define server logic --------------------------------------------
server = function(input, output){
  ouput <- subset(df, Date >= input$date & Date <= input$date2)
  vistime(output, start = "Date", end = "End", events = "Event.Name", optimize_y = TRUE, 
          title = "Race and WWII", show_labels = FALSE, group = "group")
}
  

# Run the app ----------------------------------------------------
shinyApp(ui = ui, server = server)


