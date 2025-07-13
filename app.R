#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# available at <https://raynaharrisphd-periodictable.share.connect.posit.cloud/>

library(shiny)
library(tidyverse)
library(readxl)
library(viridis)

# create file with dependenies for hosting
#library(rsconnect)
#rsconnect::writeManifest()

df <- read_excel("Lab.XX_DataAnalysisofAtoms.xlsx") %>%
  mutate(NumberofValence = as.factor(NumberofValence),
         Year = as.factor(Year)) %>%
  filter(AtomicRadius > 0)

colsofinterest <- c("Density","Electronegativity", "Metal" ,
                    "NumberOfIsotopes", "NumberofValence", "Phase" ,  "Radioactive", "Type")

colsofinterest

labelsofinterest <- c("Electronegativity","Number of Valence Electrons", "Phase" ,  "Radioactivity", "Type")

colsofinterest


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Physical Properties of Elements on the Periodic Table"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("colsofinterest", label = "Element Properties", choices = colsofinterest)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("radiobutton"),
           plotOutput("radiobutton2"),
           plotOutput("radiobutton3"),
           tableOutput('table')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$radiobutton <- renderPlot({

      df2 <- df %>%
        select(Group, Period, Symbol, NumberofProtons, AtomicRadius, input$colsofinterest) %>%
        rename("Variable" = input$colsofinterest)

      p <- ggplot(df2, aes(x = Group, y = Period, 
                           label = Symbol, color = Variable)) +
        geom_point(aes(size = AtomicRadius)) +
        geom_text(check_overlap = TRUE, nudge_y = 0.25,
                  size = 3) +
        scale_y_reverse(breaks= c(1,2,3,4,5,6,7)) +
        scale_x_continuous(breaks = c(2,4,6,8,10,12,14,16,18)) +
        theme_bw() +
        labs(color = input$colsofinterest, 
             title = "Periodic Table of Elements",
             subtitle = input$colsofinterest) +
        theme(legend.position = "bottom")
      print(p)
    })
    
    
    output$radiobutton2 <- renderPlot({
      
      df2 <- df %>%
        select(AtomicNumber, Group, Period, Symbol,NumberofProtons, NumberofNeutrons, AtomicMass, AtomicRadius, input$colsofinterest) %>%
        rename("Variable" = input$colsofinterest)

      p <- ggplot(df2, aes(x = AtomicNumber, y = AtomicMass, 
                           label = Symbol, color = Variable)) +
        geom_line(aes(x = AtomicNumber, y = AtomicNumber * 2), color = "black", linetype = 2) +
        geom_point(aes(size = AtomicRadius)) +
        geom_text(check_overlap = TRUE, nudge_y = 5,
                  size = 3) +
        theme_bw() +
        labs(color = input$colsofinterest, 
             title = "Elements Organized by Atomic Number",
             subtitle = input$colsofinterest) +
        theme(legend.position = "bottom")
      print(p)
    })
    
    output$radiobutton3 <- renderPlot({
      
      df2 <- df %>%
        select(Group, Period, Symbol,AtomicNumber, NumberofNeutrons, AtomicMass, AtomicRadius, input$colsofinterest) %>%
        rename("Variable" = input$colsofinterest)
      
      p <- ggplot(df2, aes(x = AtomicNumber, y = Variable, 
                           label = Symbol, color = Variable)) +
        geom_point(aes(size = AtomicRadius)) +
        theme_bw() +
        labs(y = input$colsofinterest,
             color = input$colsofinterest, 
             title = "Elements Organized by Atomic Number",
             subtitle = input$colsofinterest) +
        theme(legend.position = "none")
      print(p)
    })
    
    output$table <- renderTable({
      
      df2 <- df %>%
        filter(NumberofProtons < 15) %>%
        mutate(AtomicNumber = as.factor(AtomicNumber),
               Group = as.factor(Group),
               Period = as.factor(Period)) %>%
        select(Symbol, Element, AtomicNumber, AtomicMass, Group, Period, AtomicRadius, input$colsofinterest) 
      df2
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
