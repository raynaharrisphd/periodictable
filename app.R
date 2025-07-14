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


myorder <- c("Alkali Metal", "Alkaline Earth Metal", "Transition Metal" ,
             "Lanthanide" ,   "Metalloid", "Metal",  "Nonmetal" , 
             "Halogen"  , "Noble Gas" )

colsofinterest <- c("Type", "Density", "Electronegativity", "Metal" , "NumIsotopes",
                    "Phase" ,  "Radioactive", "Radius", "ValenceNum", "Mass" )

quals <- c("Type","Metal" , "Phase",  "Radioactive", "ValenceNum" )
quants <- c("Density", "Electronegativity", "Mass", "NumIsotopes","Radius" )

df <- read_excel("Lab.XX_DataAnalysisofAtoms.xlsx") %>%
  filter(AtomicRadius > 0)  %>%
  mutate(NumberofValence = as.factor(NumberofValence),
         Type = as.factor(Type),
         Year = as.factor(Year)) %>%
  rename("NumIsotopes" = "NumberOfIsotopes",
         "Mass" = "AtomicMass",
         "AtomicNum" = "AtomicNumber",
         "ValenceNum" = "NumberofValence",
         "Radius"= "AtomicRadius") %>%
  select(AtomicNum, Group, Period, Symbol, Element, NumberofProtons, all_of(colsofinterest)) %>%
  mutate(NewLabel = paste(AtomicNum, Symbol, sep = "\n"))

types <- levels(df$Type)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Physical Properties of Elements on the Periodic Table"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #radioButtons("colsofinterest", label = "Visualize Qualitative Properties", choices = colsofinterest),
            h4("Customize the Periodic Table"),
            radioButtons("quals", label = "Color: Qualitative Properties", choices = quals),
            radioButtons("quants", label = "Scale: Quantitative Prorties", choices = quants),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            br(),
            h4("Explore the Dataset"),
            radioButtons("types", label = "Filter the Elements by Type", choices = types),
        ),

        mainPanel(
          br(),
           plotOutput("radiobutton"),
           plotOutput("radiobutton2"),
           br(),
          br(),
           p("Table of Elments"),
           tableOutput('table')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$radiobutton <- renderPlot({
      
      df2 <- df %>%
        mutate(Type = factor(Type, levels = myorder)) %>%
        rename("Variable" = input$quals,
               "Measure" = input$quants)
    
      p <- ggplot(df2, aes(x = Group, y = Period, 
                           label = NewLabel, 
                           color = Variable)) +
        geom_point(aes(size = Measure)) +
        geom_text(check_overlap = TRUE, nudge_y = 0.5,
                  size = 4) +
        scale_y_reverse(breaks= c(1,2,3,4,5,6,7)) +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,11,10,12,13,14,15,16,17,18)) +
        theme_classic() +
        labs(color = input$quals, size = input$quants,
             title = "Periodic Table of Elements",
             subtitle = input$colsofinterest) +
        theme(legend.position = "bottom")
      print(p)
    })
    
  
    
    output$radiobutton2 <- renderPlot({
      
      df2 <- df %>%
        #filter(Type == input$types) %>%
        mutate(Type = factor(Type, levels = myorder)) %>%
        rename("Variable" = input$quals,
               "Measure" = input$quants) 
        
      mytitle = paste0("Graph of ", input$quants, " by Element ", input$quals, sep = "" )  
      
      p <- ggplot(df2, aes(x = Variable, y = Measure, 
                           color = Variable)) +
        geom_boxplot() +
        geom_jitter() +
        theme_classic() +
        labs(y = input$quants,
             title = mytitle) +
        theme(legend.position = "none", 
              ) 
      print(p)
    })
    
    output$table <- renderTable({
      
      df2 <- df %>%
        filter(Type == input$types) %>%
        mutate(AtomicNum = as.factor(AtomicNum),
               Group = as.factor(Group),
               Period = as.factor(Period)) %>%
        select(AtomicNum, Symbol, Element, Group, Period, input$quals, input$quants) 
      df2
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
