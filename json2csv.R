# setup

library(tidyjson)
library(dplyr)
library(naniar)
library(forcats)

### Import lots of data

### datset 1 from https://github.com/Bowserinator/Periodic-Table-JSON/blob/master/PeriodicTableCSV.csv
### dataset 2 from https://github.com/Bluegrams/periodic-table-data/blob/master/Periodica.Data/Data/ElementData.csv
### dataset 3 from PubChem https://pubchem.ncbi.nlm.nih.gov/periodic-table/
### dataset 4 fis from Github https://gist.github.com/GoodmanSciences/c2dd862cd38f21b0ad36b8f96b4bf1ee#file-periodic-table-of-elements-csv 

df1 <- read_csv("PeriodicTableCSV.csv")
df2 <- read_csv("ElementData.csv")
df3 <- read_csv("PubChemElements_all.csv")
df4 <- read.csv(url("https://gist.githubusercontent.com/GoodmanSciences/c2dd862cd38f21b0ad36b8f96b4bf1ee/raw/1d92663004489a5b6926e944c1b3d9ec5c40900e/Periodic%2520Table%2520of%2520Elements.csv"))


newnamesdf1 <- c("Element", "Appearance", "AtomicMass", "BoilingPoint", "Category", 
              "Density", "DiscoveredBy", "MeltingPoint", "MolarHeat",
              "NamedBy", "AtomicNumber", "Period", "Group", "Phase", "Source",
              "SKIPME", "SKIPME2", "SKIPME3", "Summary", "Symbol", 
              "xpos", "ypos", "wxpos", "wypos", "Shells", 
              "ElectronConfig", "ElectronConfig2", "ElectronAffinity",
              "Electonegativity", "IonizationEnergy", "SKIPME4", "SKIPME5",  "SKIPME6","SKIPME7", "SKIPME8")
colnames(df1) <- newnamesdf1

temp1 <- df1 %>%  
  select(AtomicNumber, Symbol, Element, Appearance, ElectronConfig, ElectronConfig2) %>%
  filter(Element != "Ununennium")

temp2 <- df2  %>%
  select(AtomicNumber, Symbol, Group, Period, Block,
         AtomicMass, Density, HeatCapacity:DiscoveredBy)

temp3 <- df3 %>% 
  select(AtomicNumber, Symbol, StandardState, GroupBlock)
colnames(temp3)[4] <- "Type"
colnames(temp3)[3] <- "Phase"

temp4 <- df4 %>%
  select(AtomicNumber, Symbol, NumberofNeutrons, NumberofProtons, NumberofElectrons, Natural) 


tail(temp4)


elements <- full_join(temp1, temp2, by = c("AtomicNumber", "Symbol"))  %>%
  full_join(., temp3, by = c("AtomicNumber", "Symbol"))  %>%
  left_join(., temp4, by = c("AtomicNumber", "Symbol"))  %>%
  select(AtomicNumber, Symbol, Element, Type, 
         Natural, Phase, Radioactive, Group, Period, 
         
         Block, 
         Appearance,
         everything())   %>%
  mutate(across(c("Discovery"),~ifelse(.x<1600,NA,.x))) %>%
  mutate(across(where(~ all(.x %in% 0:1)), factor, labels = c("No", "Yes"))) %>%
  mutate(Phase = as.factor(Phase))  %>%
  mutate(Phase = fct_collapse(Phase,
                         Solid = "Solid",
                         Liquid = "Liquid",
                         Gas = "Gas",
                         "<NA>" = c("Expected to be a Gas","Expected to be a Solid"))) %>% replace_with_na_all(~.x == "<NA>") %>% 
replace_with_na_all(~.x == "") 

write_csv(elements, "elements.csv", col_names = TRUE)



