#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# available at <https://raynaharrisphd-periodictable.share.connect.posit.cloud/>

# data from three sources. see json2csv.R

library(shiny)
library(tidyverse)
library(stats)

# create file with dependencies for hosting
# rerun if adding new packages
#library(rsconnect)
#rsconnect::writeManifest()


###### Functions. #####

myfunction <- function(x,y) {
  
  m1 <- df %>%
    select(all_of(x))
  colnames(m1) = "Measure1"
  
  m2 <- df %>%
    select(all_of(y))  
  colnames(m2) = "Measure2"
  
  df4 <- cbind(m1, m2)
  
  res <- cor.test(df4$Measure1, df4$Measure2)
  print(res)
  
  isisgnif <-ifelse(res$p.value == 0, ". Self-comparison ", 
                    ifelse(res$p.value < 0.05 && res$estimate < -0.69,  ". Inversely correlated ",
                           ifelse(res$p.value < 0.05 && res$estimate > 0.69, ". Positively correlated ",". Not correlated")))
    
  
  p <- paste0(x, " ~ ", y, ":   R^2 = ", round(res$estimate,3), ", p-value = ", signif(res$p.value, 3), isisgnif, sep = "" )
  print(p)
}


####### Lists #######

source("mylists.R")


####### Data import #######

df <- read_csv("elements.csv") %>%
  mutate(Block = as.factor(Block),
         Type = as.factor(Type),
         Element = as.factor(Element),
         NumberofValence = as.factor(NumberofValence)) %>%
  mutate(Type = factor(Type, levels = types),
         Block = factor(Block, levels = blocks),
         Natural = factor(Natural, levels = yesnolevels),
         Radioactive = factor(Radioactive, levels = yesnolevels)) %>%
  mutate(Phase = as.factor(Phase))

elements <- df %>% pull(Element)

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
            h4("Customize the Graphs and Tables"),
            radioButtons("quants", label = "Scale: Quantitative Prorties", 
                         choices = quants, selected = "AtomicMass"),
            br(),
            radioButtons("quals", label = "Color: Qualitative Properties", 
                         choices = quals, selected = "Type"),
            br(),
            checkboxGroupInput("phases", label = "Select: Phase", 
                               choices = phases, selected = phases),
            br(),
            checkboxGroupInput("types", label = "Select: Type", 
                               choices = types, selected = displayfirst),
             br(),
             br(),
            downloadButton("downloadTable", "Download the table displayed"),
            br(),
            br(),
            downloadButton("downloadDataSet", "Download the full dataset") ,
            br(),
            br(),
            htmlOutput('html'),
            br(),
            br(),
            checkboxGroupInput("elements", label = "Select Elements to Display in a Table Preview and as Lables on Plot", 
                               choices = elements, selected = displayElements),
            br()
        ),

        mainPanel(
           br(),
           plotOutput("periodictable"),
           br(),
           plotOutput("barplot"),
           br(),
           textOutput('printme'),
           textOutput('printme2'),
           br(),
           plotOutput("scatterplot"),
           textOutput("corAtomicRadius"),
           textOutput("corElectronegativity"),
           textOutput("corIonizationEnergy"),
           textOutput("corNumberofProtons"),
           br(),
           #plotOutput("boxplot"),
           #br(),
           #textOutput('printme3'),
           #textOutput('printme4'),
           br(),
           h5("Table 1: Element Data"),
           p("This partial dataset contains information only for the elements with the selected characteristics, arranges from smallest to largest for the selected quantitative variable. This is a preview based on the elements with checked boxes. Click the download button above to save the partial dataset for all the elements in the graphs."),
           tableOutput('table'),
           br(),
           h5("Table 2: Periodic Table Data"),
           p("This complete dataset contains 20 descriptive, qualitative, and quantitaitve variables for 117 elements. Scoll down to view all the variables. This is a preview based on the elements with checked boxes.Click the download button above to save and open the full dataset. (Note: Data rotated for easy viewing.)"),
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
        filter(Type %in% input$types,
               Phase %in% input$phases
               ) %>%
        rename("Variable" = input$quals,
               "Measure" = input$quants) %>%
        mutate(NumSymbol = paste(AtomicNumber, Symbol, sep = "\n")) %>%
        select(Group, Period, Symbol, Variable, Measure, xpos, ypos, NumSymbol) %>% 
        drop_na()
        
      p <- ggplot(df2, aes(x = xpos, y = ypos, 
                           label = NumSymbol, 
                           color = Variable)) +
        geom_point(aes(size = Measure), shape = 15) +
        geom_text(check_overlap = TRUE, nudge_y = 0.5,
                  size = 3.5) +
        scale_y_reverse(breaks= c(1,2,3,4,5,6,7),
                        limits = c(9,0.5)) +
        scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,11,10,12,13,14,15,16,17,18),
                           limits = c(1,18)) +
        theme_classic() +
        labs(color = input$quals, 
             size = input$quants,
             x = "Group", y = "Period",
             title = "Periodic Table of Elements",
             subtitle = paste0("Color: ", input$quals, ", Size: ", input$quants, " (Not to Scale)",sep = "")) 
      return(p)
    })
    
  
##### barplot ####
    
    output$barplot <- renderPlot({
      
      df2 <- df %>%
        mutate(Type = factor(Type, levels = types)) %>%
        filter(Type %in% input$types) %>%
        filter(Phase %in% input$phases) %>%
        rename("Variable" = input$quals,
               "Measure" = input$quants)  %>%
        select(Element, AtomicNumber, Symbol, Variable, Measure) %>%
        mutate(NewLabel = ifelse(Element %in% input$elements, Symbol, ""))
      
      mytitle = paste0("Barplot of ", input$quants,  sep = "" )  
      mysubtitle = paste0("Which elements have the highest and lowest ", 
                          input$quants, "?", sep = "" )  
      

      p <- ggplot(df2, aes(x = AtomicNumber , y = Measure, 
                           fill = Variable, label = NewLabel)) +
        geom_bar(stat = "identity") +
        geom_text(check_overlap = F, size = 3.5) +
        theme_classic() +
        labs(fill = input$quals,
             x = "Atomic Number",
             y = input$quants,
             title = mytitle, subtitle = mysubtitle) +
        scale_x_continuous(breaks = mybreaks)
      return(p)
      
      
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
      mysubtitle = paste0("Does ", input$quants, 
                       " differ between ", input$quals, "s?", sep = "" ) 
      
      p <- ggplot(df2, aes(x = Variable, y = Measure, 
                           color = Variable)) +
        geom_boxplot(outliers = FALSE) +
        geom_jitter() +
        #theme_classic( ) +
        labs(x = input$quals,
             y = input$quants,
             title = mytitle,
             subtitle = mysubtitle,
             color = NULL)  
        
      return(p)
    })
    
##### scatter plots ####
    
    output$scatterplot <- renderPlot({
      
      temp <- df %>%
        filter(Type %in% input$types,
               Phase %in% input$phases) %>%
        select(AtomicNumber:Element, input$quants) %>% 
        rename("Measure" = input$quants)
      
      mysubtitle = paste0("Is ", input$quants, " correlated with other quantitative traits?", 
                          sep = "")
      mytitle = paste0("Scatter Plots of ", input$quants,  " and Other Quantitative Traits",sep = "")
      
      dflong <- df %>%
        filter(Type %in% input$types,
               Phase %in% input$phases) %>%
        rename("Variable" = input$quals)  %>%
        select(AtomicNumber:Element, Variable, all_of(newquants),  input$quants) %>%
        pivot_longer(cols = c(newquants), 
                     names_to = "Measures", values_to = "Value") %>%
        left_join(., temp) %>%
        mutate(NewLabel = ifelse(Element %in% input$elements, Symbol, ""))
      head(dflong)  
      
      p <- ggplot(dflong, aes(x = Value, y = Measure, label = NewLabel)) +
        #geom_smooth(method = "lm", color = "darkgrey", se = F) +
        geom_point(aes(color = Variable)) +
        facet_wrap(~Measures, scales = "free_x", switch = "x") +
        theme_classic( ) + 
        labs(y = input$quants, subtitle = mysubtitle, title = mytitle) +
        geom_text(check_overlap = F, size = 3.5) 
      return(p)  
      
      
    })
    
    
##### correlation stats #####
    
    output$corAtomicRadius <- renderText({ myfunction(input$quants, "AtomicRadius")})
    output$corNumberofProtons <- renderText({ myfunction(input$quants, "NumberofProtons")})
    output$corElectronegativity <- renderText({ myfunction(input$quants, "Electronegativity")})
    output$corIonizationEnergy <- renderText({ myfunction(input$quants, "IonizationEnergy")})

    
    
##### print statements ####    
    
    output$printme <- renderText({
      
      df2 <- df %>%
        filter(Type %in% input$types,
               Phase %in% input$phases) %>%
        select(AtomicNumber, Symbol, Element, input$quals, input$quants) %>% 
        arrange(desc(.[[5]])) 
      
      df3 <- head(df2, 1) %>%
        pull(Element)
      
      df4 <- head(df2, 1) %>%
        pull(input$quants)
      
      return(paste0(df3, " has the higest ",  input$quants, " of the elelments displayed above at ", df4, sep = ""))
    })
    
    
    output$printme2 <- renderText({
      
      df2 <- df %>%
        filter(Type %in% input$types,
               Phase %in% input$phases) %>%
        select(AtomicNumber, Symbol, Element, input$quals, input$quants) %>% 
        arrange(desc(AtomicNumber)) %>%
        arrange(.[[5]])  
      
      df3 <- head(df2, 1) %>%
        pull(Element)
      
      df4 <- head(df2, 1) %>%
        pull(input$quants)
      
      return(paste0(df3, " has the lowest ",  input$quants,  " of the elelments displayed above ",  df4, sep = ""))
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
      
      return(paste0(df3, " has the highest group average ", input$quants, " of the elements displayed above.", sep = ""))
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
      
      return(paste0(df3, " has the lowest group average ", input$quants, " of the elements displayed above.", sep = ""))
    })
    

##### save table #####
    
    
    # Generate a sample table
    output$table <- renderTable({
      
      myelements <- input$elements
      
      df2 <- df %>%
        filter(Type %in% input$types,
               Phase %in% input$phases) %>%
        mutate(AtomicNumber = as.factor(AtomicNumber),
               Group = as.factor(Group),
               Period = as.factor(Period)) %>%
        select(AtomicNumber, Symbol, Element, Period, Group,  input$quals, input$quants) %>% 
        #arrange(desc(.[[7]])) %>%
        filter(Element %in% myelements)
      
      return(df2)
    })
    
    
    
    # Generate a sample table
    output$myTable <- renderTable({
      
      myelements <- input$elements
      
      df2 <- df %>%
        select("AtomicNumber", "Symbol", "Element", "AtomicMass" , 
               "NumberofProtons", "NumberofNeutrons" , NumberofElectrons,
               "ElectronConfig", "NobleGasConfig", 
               NumberofValence, NumberofShells,
               "Period" , "Group" , "Block",
               "Type" , "Phase" ,"Radioactive",  
               everything(), -Appearance) %>%
        mutate(AtomicNumber = as.factor(AtomicNumber)) %>%
        filter(Element %in% myelements)
      
      df3 <- as_tibble(t(df2)) 
      colnames(df3) <- df3[3,]
      df3 <- as.data.frame(df3)
      rownames(df3) <- colnames(df2)
      df3 <- df3 %>%
        mutate(" " = rownames(.)) %>%
        select(" ", everything())
      return(df3)

    })
    
    
    
    output$selectTable <- renderTable({
      
      df2 <- df %>%
        select("AtomicNumber", "Symbol", "Element", "AtomicMass" , 
               "NumberofProtons", "NumberofNeutrons" , NumberofElectrons,
               "ElectronConfig", "NobleGasConfig", 
               NumberofValence, NumberofShells,
               "Period" , "Group" , "Block",
               "Type" , "Phase" ,"Radioactive",  
               everything(), -Appearance, -DiscoveredBy, -xpos, -ypos) %>%
        mutate(AtomicNumber = as.factor(AtomicNumber)) %>%
        filter( Element %in% input$elements)
      
      df3 <- as_tibble(t(df2)) 
      colnames(df3) <- df3[3,]
      df3 <- as.data.frame(df3)
      rownames(df3) <- colnames(df2)
      df3 <- df3 %>%
        mutate(" " = rownames(.)) %>%
        select(" ", everything())
      return(df2)
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
          select(AtomicNumber, Symbol, Element, Period, Group,  input$quals, input$quants, all_of(newquants)) %>% 
          arrange(desc(.[[7]])) %>%
          drop_na()
        
        write.csv(df2, file, row.names = FALSE)
      }
    )
    
    # html output 
    
    output$html <- renderText({
      
      printme <- '<br>Source data from <a href="https://github.com/Bowserinator/Periodic-Table-JSON/blob/master/PeriodicTableCSV.csv">Bowserinator GitHub</a>, <a href="https://github.com/Bluegrams/periodic-table-data/blob/master/Periodica.Data/Data/ElementData.csv">Bluegrams GitHub</a>, 
      <a href="https://pubchem.ncbi.nlm.nih.gov/periodic-table/">PubChem</a>, and <a href="https://gist.github.com/GoodmanSciences/c2dd862cd38f21b0ad36b8f96b4bf1ee#file-periodic-table-of-elements-csv">GoodmanSciences GitHub</a>. <br><br> Source code available at  <a href="https://raynaharris.com/github/periodictable"> Rayna\'s GitHub</a>.'
      
        return(printme)
    })
    
    
    
    
###### closing ####    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)



###### the end #####


