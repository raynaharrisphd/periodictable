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
library(bslib)
library(ggpubr)
library(rstatix)
library(forcats)

# create file with dependencies for hosting
#library(rsconnect)
#rsconnect::writeManifest()

####### Lists #######

# inputs
myorder <- c("Alkali Metal", "Alkaline Earth Metal", "Lanthanide" ,
             "Actinide" , "Transactinide", "Transition Metal" ,
              "Metal",  "Metalloid", "Nonmetal" , 
             "Halogen"  , "Noble Gas" )

displayfirst <- c("Alkali Metal", "Alkaline Earth Metal", "Metal",  
                 "Metalloid", "Nonmetal", 
                 "Halogen"  , "Noble Gas" )

colsofinterest <- c("Type", "Density", "Electronegativity",  "NumIsotopes",
                    "Phase" ,  "Radioactive", "Radius", "ValenceNum", "Mass" )

quals <- c("Type",  "Phase", "Radioactive", "ValenceNum" )
quants <- c( "Mass", "Density", "Radius",  "Electronegativity",  "NumIsotopes" )

####### Data wrangle #######
df <- read_excel("Lab.XX_DataAnalysisofAtoms.xlsx") %>%
  mutate(NumberofValence = as.factor(NumberofValence),
         Type = as.factor(Type),
         Year = as.factor(Year),
         Phase = as.factor(Phase),
         Radioactive = as.factor(Radioactive), 
         NumberofValence = as.factor(NumberofValence),
         Element = as.factor(Element)
         ) %>%
  rename("NumIsotopes" = "NumberOfIsotopes",
         "Mass" = "AtomicMass",
         "AtomicNum" = "AtomicNumber",
         "ValenceNum" = "NumberofValence",
         "Radius"= "AtomicRadius",) %>%
  select(AtomicNum, Group, Period, Symbol, Element, NumberofProtons, all_of(colsofinterest)) %>%
  mutate(NewLabel = paste(AtomicNum, Symbol, sep = "\n"),
         Radioactive = fct_na_value_to_level(Radioactive, "no")) %>%
  filter(Type %in% myorder) %>%
  mutate(Type = factor(Type, levels = myorder)) 

types <- levels(df$Type)
elements <- levels(df$Element)

mybreaks <- df %>%
  filter(Group == 1) %>%
  pull(AtomicNum)

mybreaks[8] <- 119

####### Shiny  UI #######

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Physical Properties of Elements on the Periodic Table"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #radioButtons("colsofinterest", label = "Visualize Qualitative Properties", choices = colsofinterest),
            h4("Customize the Periodic Table"),
            radioButtons("quals", label = "Color: Qualitative Properties", 
                         choices = quals),
            br(),
            radioButtons("quants", label = "Scale: Quantitative Prorties", 
                         choices = quants),
            br(),
            checkboxGroupInput("types", label = "Filter: Element Type", 
                               choices = types, selected = displayfirst)
        ),

        mainPanel(
           br(),
           plotOutput("periodictable"),
           br(),
           plotOutput("barplot"),
           br(),
           plotOutput("boxplot"),
           br(),
           p("Data Averaged across Elements with Similar Properties"),
           tableOutput('summary'),
           br(),
           p("Data for Elements in the Graphs Above"),
           tableOutput('table')
        )
    )
)


####### Shiny Server #######

# Define server logic 
server <- function(input, output) {

  
#### periodictable #### 
  
    output$periodictable <- renderPlot({
      
      df2 <- df %>%
        mutate(Type = factor(Type, levels = myorder)) %>%
        filter(Type %in% input$types) %>%
        rename("Variable" = input$quals,
               "Measure" = input$quants) %>%
        select(Group, Period, NewLabel, Variable, Measure) %>%
        drop_na()
    
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
             subtitle = paste0("Color: ", input$quals, ", Size: ", input$quants, sep = "")) 
      print(p)
    })
    
  
##### barplot ####
    
    output$barplot <- renderPlot({
      
      df2 <- df %>%
        mutate(Type = factor(Type, levels = myorder)) %>%
        filter(Type %in% input$types) %>%
        rename("Variable" = input$quals,
               "Measure" = input$quants)  %>%
        select(AtomicNum, Symbol, Variable, Measure)  %>%
        drop_na()
      
      mytitle = paste0("Barplot of Increasing ", input$quants, sep = "" )  

      p <- ggplot(df2, aes(x = AtomicNum , y = Measure, 
                           fill = Variable, label = Symbol)) +
        geom_bar(stat = "identity") +
        geom_text(check_overlap = TRUE, 
                  label.padding = 0.5) +
        theme_classic() +
        labs(fill = input$quals,
             x = "Atomic Number",
             y = input$quants,
             title = mytitle) +
        scale_x_continuous(breaks = mybreaks)
      print(p)
      
      
    })
    
    
##### boxplot ####  
    
    output$boxplot <- renderPlot({
      
      df2 <- df %>%
        mutate(Type = factor(Type, levels = myorder)) %>%
        filter(Type %in% input$types) %>%
        rename("Variable" = input$quals,
               "Measure" = input$quants)  %>%
        select(Variable, Measure)  %>%
        drop_na()
        
      mytitle = paste0("Boxplot of ", input$quants, 
                       " by ", input$quals,  sep = "" )  
      
      p <- ggplot(df2, aes(x = Variable, y = Measure, 
                           color = Variable)) +
        geom_boxplot() +
        geom_jitter() +
        theme_classic() +
        labs(x = input$quals,
             y = input$quants,
             title = mytitle,
             color = NULL)  
        
      print(p)
    })
    
    output$table <- renderTable({
      
      df2 <- df %>%
        filter(Type %in% input$types) %>%
        mutate(AtomicNum = as.factor(AtomicNum),
               Group = as.factor(Group),
               Period = as.factor(Period)) %>%
        select(AtomicNum, Symbol, Element, Group, Period, input$quals, input$quants) 
      print(df2)
    })
    
  
    
    output$summary <- renderTable({
      
      df2 <- df %>%
        select(input$quals, input$quants) 
      newcolname <- colnames(df2) 
    
      df3 <- df %>%
        mutate(Type = factor(Type, levels = myorder)) %>%
        filter(Type %in% input$types) %>%
        mutate(AtomicNum = as.factor(AtomicNum),
               Group = as.factor(Group),
               Period = as.factor(Period),
               Type = as.factor(Type)) %>%
        rename("Groups" = input$quals,
               "Measure" = input$quants)  %>%
        select(Groups, Measure)  %>%
        #drop_na() %>%
        group_by(Groups) %>%
        summarise(Mean = mean(Measure, na.rm = T)) 
      
      colnames(df3) <- newcolname
      print(df3)
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
