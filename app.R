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
# rerun if adding new packages
#library(rsconnect)
#rsconnect::writeManifest()

####### Lists #######

# inputs
types <- c("Alkali metal", "Alkaline earth metal", "Lanthanide" ,
             "Actinide" ,  "Transition metal" ,
              "Post-transition metal",  "Metalloid", "Nonmetal" , 
             "Halogen"  , "Noble gas" )

displayfirst <- c("Alkali metal", "Alkaline earth metal", "Post-transition metal",  
                 "Metalloid", "Nonmetal", 
                 "Halogen"  , "Noble gas" )

phaselevels <- c( "Solid", "Liquid" , "Gas" )
natural <- phaselevels

quals <- c("Block",  "Phase", "Natural","Radioactive" , "Type" )

quants <- c("AtomicMass", "AtomicRadius", "Density",  "ElectronAffinity", "Electronegativity", "IonizationEnergy", 
            "NumberOfIsotopes" ,"NumberofNeutrons", "NumberofProtons", 
            "ThermalConductivity","VanDerWaalsRadius")

colsofinterest <- c(quals, quants)

####### Data wrangle #######

df <- read_csv("elements.csv") %>%
  mutate(#AtomicNumber = as.factor(AtomicNumber),
         #Group = as.factor(Group),
         #Period = as.factor(Period),
         Block = as.factor(Block),
         Type = as.factor(Type),
         Phase = as.factor(Phase),
         Element = as.factor(Element))

elements <- levels(df$Element)
phases <- levels(df$Phase)


mybreaks <- df %>%
  filter(Group == 18) %>%
  pull(AtomicNumber)

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
                         choices = quals, selected = "Type"),
            br(),
            radioButtons("quants", label = "Scale: Quantitative Prorties", 
                         choices = quants, selected = "Electronegativity"),
                        br(),
            checkboxGroupInput("phases", label = "Select: Phase", 
                               choices = phases, selected = natural),
            br(),
            checkboxGroupInput("types", label = "Select: Element Type", 
                               choices = types, selected = displayfirst),
            br(),
            downloadButton("downloadTable", "Download the table displayed"),
            br(),
            br(),
            downloadButton("downloadDataSet", "Download the full dataset")
        ),

        mainPanel(
           br(),
           plotOutput("periodictable"),
           br(),
           plotOutput("barplot"),
           br(),
           br(),
           textOutput('printme'),
           textOutput('printme2'),
           br(),
           plotOutput("boxplot"),
           br(),
           textOutput('printme3'),
           textOutput('printme4'),
           br(),
           h5("Table 1: Element Data"),
           p("This table contains all the data for the elements in the graphs above, arranges from smallest to largest for the selected quantitative variable."),
           tableOutput('table'),
           br(),
           h5("Dataset 1: Periodic Table Data Preview"),
           p("This table provides a preview (first 5 elelements) of a dataset containg 20 descriptive, qualitative, and quantitaitve variables for 117 elements. Scoll down to view all the variables. (Note, data rotated 90 degrees for easy viewing.) Click the download button above to save and open the full dataset. "),
           tableOutput('myTable')
           
        )
    )
)


####### Shiny Server #######

# Define server logic 
server <- function(input, output) {

#### periodictable #### 
  
    output$periodictable <- renderPlot({
      
      df2 <- df %>%
        mutate(Type = factor(Type, levels = types)) %>%
        filter(Type %in% input$types,
               Phase %in% input$phases) %>%
        rename("Variable" = input$quals,
               "Measure" = input$quants) %>%
        mutate(NumSymbol = paste0(AtomicNumber, Symbol, sep = '\n')) %>%
        select(Group, Period, Symbol, Variable, Measure) 
        
    
      p <- ggplot(df2, aes(x = Group, y = Period, 
                           label = Symbol, 
                           color = Variable)) +
        geom_point(aes(size = Measure)) +
        geom_text(check_overlap = TRUE, nudge_y = 0.5,
                  size = 4) +
        scale_y_reverse(breaks= c(1,2,3,4,5,6,7,8)) +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,11,10,12,13,14,15,16,17,18)) +
        theme_classic() +
        labs(color = input$quals, size = input$quants,
             title = "Periodic Table of Elements",
             subtitle = paste0("Color: ", input$quals, ", Size: ", input$quants, " (Not to Scale)",sep = "")) 
      print(p)
    })
    
  
##### barplot ####
    
    output$barplot <- renderPlot({
      
      df2 <- df %>%
        mutate(Type = factor(Type, levels = types)) %>%
        filter(Type %in% input$types) %>%
        rename("Variable" = input$quals,
               "Measure" = input$quants)  %>%
        select(AtomicNumber, Symbol, Variable, Measure)  
      
      mytitle = paste0("Barplot of ", input$quants, " by Atomic Number", sep = "" )  

      p <- ggplot(df2, aes(x = AtomicNumber , y = Measure, 
                           fill = Variable, label = Symbol)) +
        geom_bar(stat = "identity") +
        geom_text(check_overlap = TRUE) +
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
        mutate(Type = factor(Type, levels = types)) %>%
        filter(Type %in% input$types,
               Phase %in% input$phases) %>%
        rename("Variable" = input$quals,
               "Measure" = input$quants)  %>%
        select(Variable, Measure)  %>%
        drop_na()
        
      mytitle = paste0("Boxplot of ", input$quants, 
                       " by ", input$quals,  sep = "" )  
      
      p <- ggplot(df2, aes(x = Variable, y = Measure, 
                           color = Variable)) +
        geom_boxplot(outliers = FALSE) +
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
        filter(Type %in% input$types,
               Phase %in% input$phases) %>%
        mutate(AtomicNumber = as.factor(AtomicNumber),
               Group = as.factor(Group),
               Period = as.factor(Period)) %>%
        select(AtomicNumber, Symbol, Element, Period, Group,  input$quals, input$quants) %>% 
        arrange(desc(.[[7]])) %>%
        drop_na()
      print(df2)
    })
    
###### unused stats #####  
    
    output$summary <- renderTable({
      
      df2 <- df %>%
        select(input$quals, input$quants) 
      newcolname <- colnames(df2) 
    
      df3 <- df %>%
        mutate(Type = factor(Type, levels = types)) %>%
        filter(Type %in% input$types) %>%
        mutate(AtomicNumber = as.factor(AtomicNumber),
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
    
    
    output$stats <- renderTable({
      
      df2 <- df %>%
        filter(Type %in% input$types) %>%
        rename("Groups" = input$quals,
               "Measure" = input$quants)  %>%
        select(Groups, Measure) 
      
      tukey_result <- data.frame(TukeyHSD(aov(Measure ~ Groups, data = df2))[[1]]) %>%
        mutate(comparison = row.names(.),
               " " = word(comparison, 1, sep = "-"),
               "  " = word(comparison, 2, sep = "-"),
              "   " = stars.pval(p.adj)) %>%
        select(" ", "  ", diff, p.adj, "   ") %>%
        arrange(p.adj) %>%
        rename("Difference" = diff,
               "p-value" = p.adj) 
      print(head(tukey_result, 5))
  
      
    })
    
    
##### print statements ####    
    
    output$printme <- renderText({
      
      df2 <- df %>%
        filter(Type %in% input$types,
               Phase %in% input$phases) %>%
        select(AtomicNumber, Symbol, Element, input$quals, input$quants) %>% 
        arrange(desc(.[[5]])) 
      
      df3 <- head(df2, 1) %>%
        pull(Element)
      print(paste0(df3, " has the higest ",  input$quants, " of the elelments displayed above.", sep = ""))
    })
    
    
    output$printme2 <- renderText({
      
      df2 <- df %>%
        filter(Type %in% input$types,
               Phase %in% input$phases) %>%
        select(AtomicNumber, Symbol, Element, input$quals, input$quants) %>% 
        arrange(.[[5]])  
      
      df3 <- head(df2, 1) %>%
        pull(Element)
      print(paste0(df3, " has the lowest ",  input$quants,  " of the elelments displayed above.", sep = ""))
    })
    
    
    output$printme3 <- renderText({
      
      df2 <- df %>%
        filter(Type %in% input$types,
               Phase %in% input$phases) %>%
        rename("Variable" = input$quals,
               "Measure" = input$quants)  %>%
        select(AtomicNumber, Symbol, Element, Variable, Measure) %>% 
        group_by(Variable)   %>%
        summarize(Mean = mean(Measure, na.rm = T)) %>%
        arrange(desc(Mean))
      
      df3 <- head(df2, 1) %>%
        pull(Variable)
      print(paste0(df3, "s have the highest group average ", input$quants, "of the elements displayed above.", sep = ""))
    })
    
    output$printme4 <- renderText({
      
      df2 <- df %>%
        filter(Type %in% input$types,
               Phase %in% input$phases) %>%
        rename("Variable" = input$quals,
               "Measure" = input$quants)  %>%
        select(AtomicNumber, Symbol, Element, Variable, Measure) %>% 
        group_by(Variable)   %>%
        summarize(Mean = mean(Measure, na.rm = TRUE)) %>%
        arrange(Mean)
      
      df3 <- head(df2, 1) %>%
        pull(Variable)
      print(paste0(df3, "s have the lowest group average ", input$quants, "of the elements displayed above.", sep = ""))
    })
    

##### save table #####
    
    # Generate a sample table
    output$myTable <- renderTable({
      
      df2 <- df %>%
        select("AtomicNumber","Element", "Symbol", "AtomicMass" , 
               "NumberofProtons", "NumberofNeutrons" , 
               "ElectronConfig", "ElectronConfig2", "Period" , "Group" , 
               "Phase" ,"Radioactive", "Type" , 
               everything()) %>%
        mutate(AtomicNumber = as.factor(AtomicNumber)) %>%
        head(., 5)
      
      df3 <- as.tibble(t(df2)) 
      colnames(df3) <- df3[3,]
      df3 <- as.data.frame(df3)
      rownames(df3) <- colnames(df2)
      df3 <- df3 %>%
        mutate(" " = rownames(.)) %>%
        select(" ", everything())
      return(df3)

    })
    
    # Handle the download
    output$downloadDataSet <- downloadHandler(
      filename = function() {
        "periodictabledata.csv"
      },
      content = function(file) {
        
        df2 <- df %>%
          select("AtomicNumber","Element", "Symbol", "AtomicMass" , 
                 "NumberofProtons", "NumberofNeutrons" , 
                 "ElectronConfig", "Period" , "Group" , 
                 "Phase" ,"Radioactive", "Type" , 
                 everything())
          
        write.csv(df2, file, row.names = FALSE)
      }
    )
    
    # Handle the download
    output$downloadTable <- downloadHandler(
      filename = function() {
        myfilename <- paste0("periodictabledata-", input$quals, "-", input$quants, ".csv")
      },
      
      content = function(file) {
        
         df2 <- df %>%
          filter(Type %in% input$types,
                 Phase %in% input$phases) %>%
          mutate(AtomicNum = as.factor(AtomicNumber),
                 Group = as.factor(Group),
                 Period = as.factor(Period)) %>%
          select(AtomicNumber, Symbol, Element, Period, Group,  input$quals, input$quants) %>% 
          arrange(desc(.[[7]])) %>%
          drop_na()
        
        write.csv(df2, file, row.names = FALSE)
      }
    )
    
    
###### closing ####    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
