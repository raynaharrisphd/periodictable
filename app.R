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
library(bslib)

# create file with dependenies for hosting
#library(rsconnect)
#rsconnect::writeManifest()

df <- read_excel("Lab.XX_DataAnalysisofAtoms.xlsx") %>%
  filter(AtomicRadius > 0)  %>%
  mutate(NumberofValence = as.factor(NumberofValence),
         Type = as.factor(Type),
         Year = as.factor(Year)) %>%
  rename("NumIsotopes" = "NumberOfIsotopes",
         "Mass" = "AtomicMass",
         "AtomicNum" = "AtomicNumber",
         "ValenceNum" = "NumberofValence",
         "Radius"= "AtomicRadius")
head(df)

colsofinterest <- c("ValenceNum", "Density", "Electronegativity", "Metal" , "NumIsotopes",
                      "Phase" ,  "Radioactive", "Type")

colsofinterest

types <- c("Alkali Metal", "Alkaline Earth Metal", "Halogen",  
           "Lanthanide", "Metal", "Metalloid", "Noble Gas",
           "Nonmetal",  "Transition Metal" )

colsofinterest


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Plot Physical Properties of Elements on the Periodic Table"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            radioButtons("colsofinterest", label = "Plot Physical Properties", choices = colsofinterest),
            br(),
            br(),
            br(),
            radioButtons("types", label = "Explore the Dataset by Type", choices = types),
        ),
    

         #Show a plot of the generated distribution
        mainPanel(
           plotOutput("radiobutton"),
           #plotOutput("radiobutton2"),
           #plotOutput("radiobutton3"),
           tableOutput('table')
        )
        
      
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$radiobutton <- renderPlot({

      df2 <- df %>%
        select(AtomicNum, Mass, Group, Period, Symbol, NumberofProtons, Radius, input$colsofinterest) %>%
        rename("Variable" = input$colsofinterest) %>%
        mutate(NewLabel = paste(AtomicNum, Symbol, sep = "\n"))
      head(df2$NewLabel)

      p <- ggplot(df2, aes(x = Group, y = Period, 
                           label = NewLabel, 
                           color = Variable)) +
        geom_point(aes(size = Radius)) +
        geom_text(check_overlap = TRUE, nudge_y = 0.5,
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
        select(Group, Period, Symbol,AtomicNum, NumberofNeutrons, Mass, Radius, input$colsofinterest) %>%
        rename("Variable" = input$colsofinterest)
      
      p <- ggplot(df2, aes(x = AtomicNum, y = Variable, 
                           label = Symbol, color = Variable)) +
        geom_point(aes(size = Radius)) +
        theme_bw() +
        labs(y = input$colsofinterest,
             color = input$colsofinterest, 
             title = "Elements Organized by Atomic Number",
             subtitle = input$colsofinterest) +
        theme(legend.position = "bottom")
      print(p)
    })
    
    output$table <- renderTable({
      
      df2 <- df %>%
        filter(Type == input$types) %>%
        mutate(AtomicNum = as.factor(AtomicNum),
               Group = as.factor(Group),
               Period = as.factor(Period)) %>%
        select(AtomicNum, Symbol, Element, Mass, Group, Period, Radius, input$colsofinterest) 
      df2
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
