# setup

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
df4 <- read_csv("periodictable.csv")

names(df1)
newnamesdf1 <- c("Element", "Appearance", "AtomicMass", "BoilingPoint", "Category", 
              "Density", "DiscoveredBy", "MeltingPoint", "MolarHeat",
              "NamedBy", "AtomicNumber", "Period", "Group", "Phase", "Source",
              "SKIPME", "SKIPME2", "SKIPME3", "Summary", "Symbol", 
              "xpos", "ypos", "wxpos", "wypos", "Shells", 
              "ElectronConfig", "ElectronConfig2", "ElectronAffinity",
              "Electonegativity", "IonizationEnergy", "SKIPME4", "SKIPME5",  
              "SKIPME6","SKIPME7", "SKIPME8")
colnames(df1) <- newnamesdf1

temp1 <- df1 %>%  
  select(AtomicNumber, Symbol, Element, Appearance, ElectronConfig,
         "xpos", "ypos") %>%
  mutate(ypos = ifelse(Symbol == "La", 6, 
                       ifelse(Symbol == "Ac", 7, ypos))) %>%
  filter(Element != "Ununennium")

temp2 <- df2  %>%
  select(AtomicNumber, Symbol, Group, Period, Block,
         AtomicMass, Density, HeatCapacity:Discovery, 
         DiscoveredBy, Configuration)
colnames(temp2)[23] <- "NobleGasConfig"
head(temp2)

temp3 <- df3 %>% 
  select(AtomicNumber, Symbol, StandardState, GroupBlock)
colnames(temp3)[4] <- "Type"
colnames(temp3)[3] <- "Phase"

temp4 <- df4 %>%
  select(AtomicNumber, Symbol, NumberofNeutrons, NumberofProtons, 
         NumberofElectrons, Natural, NumberofShells, NumberofValence, 
         Radioactive) 

elements <- full_join(temp1, temp2, by = c("AtomicNumber", "Symbol"))  %>%
  full_join(., temp3, by = c("AtomicNumber", "Symbol"))  %>%
  left_join(., temp4, by = c("AtomicNumber", "Symbol"))  %>%
  select(AtomicNumber, Symbol, Element, Type, 
         Natural, Phase, Radioactive, Group, Period, 
         Block, ElectronConfig, NobleGasConfig,
         NumberofValence, NumberofShells,
         everything(), -AbundanceCrust, -AbundanceUniverse)   %>%
         replace_with_na_all(~.x == "")  %>%
  mutate(across(c("Discovery"),~ifelse(.x<1600,NA,.x)),
         Phase = as.factor(Phase),
         Phase = fct_collapse(Phase,
                         Solid = c("Solid",
                                   "Expected to be a Solid"),
                         Liquid = c("Liquid"),
                         Gas = c("Gas",
                                 "Expected to be a Gas"))) %>% 
  mutate(Natural = str_to_title(Natural),
         Radioactive = str_to_title(Radioactive),
         Appearance = str_to_title(Appearance)) %>%
  mutate(Period = ifelse(AtomicNumber >57 & AtomicNumber < 72, 6,
                    ifelse(AtomicNumber >89 & AtomicNumber < 104, 7, Period)),
         Group = ifelse(AtomicNumber >57 & AtomicNumber < 72, 3,
                         ifelse(AtomicNumber >89 & AtomicNumber < 104, 3, Group)),
         Radioactive = replace_na_with( Radioactive, "No"),
         Natural = replace_na_with( Natural, "No"))

write_csv(elements, "elements.csv", col_names = TRUE)
write_csv(elements, "../elements/elements.csv", col_names = TRUE)

