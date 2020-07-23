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

time <- vistime(df, start = "Date", end = "End", events = "Event.Name", optimize_y = FALSE, 
                title = "Race and WWII", show_labels = FALSE, group = "group")

# Define UI ----------------------------------------------------
ui <- fluidPage(
  # theme
  theme = shinytheme("paper"),
  # title panel
  titlePanel("NEH American Soldier Dashboard"),
  # sidebar w date range, inpit box, and submit button
  sidebarLayout(position = "left",
                sidebarPanel(dateRangeInput("dates", h5("Date range"), start = "1935-01-01", end = "1955-01-01",
                                            min = "1935-01-01",max="1955-01-01",
                                            format = "mm/dd/yy"),
                             actionButton("submitButton",'Submit')),
                # the main panel of the whole shabang
                mainPanel(
                  # main panel title
                  h4("Interactive Timeline:"),
                  plotlyOutput("myVistime")
                )
  )
)

# Define server logic --------------------------------------------
server = function(input, output){
  
  #input (all of the elements that put in the UI)
  observe({
    print(input$dates) #A vector of two elements (statdate, enddate)
  })
  daterange <- reactive({input$dates})
  windowTime <- reactive({
    subset_df <- df[df$Date >= daterange()[1] & df$End <= daterange()[2],]
    subset_df
  })
  
  observeEvent(input$submitButton,{
    print('--Current Selected Window in Days--')
    print(windowTime())
    print('--')
  })
  
  output$myVistime <- renderPlotly({
    vistime(reactive({subset_df}), start = "Date", end = "End", events = "Event.Name", optimize_y = FALSE, 
            title = "Race and WWII", show_labels = FALSE, group = "group")
  })
  # take in a input$date and an input$date2
  # then subsets the data that we have to that range
  # then creates an output that is then fed into ui or does it move to the server?
}


# Run the app ----------------------------------------------------
shinyApp(ui = ui, server = server)


