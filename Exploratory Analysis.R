library(data.table)
library(ggplot2)
library(tidyverse)

county_data <- fread("Data/Pavements & LOSTs - Sheet 1.csv")
summary(county_data)

summary(lm(`PCI 2020` ~ `PCI 2008`, data = county_data))
summary(lm(`PCI 2020` ~ `LOST Active between 08-20`, data = county_data))

summary(lm(`PCI 2020` ~ `PCI 2008` + `LOST Active between 08-20`, data = county_data))

summary(lm(`PCI 2020` ~ `PCI 2008` + `LOST Active between 08-20` + `Centerline Miles`, data = county_data))

summary(lm(`PCI 2020` ~ `PCI 2008` + `LOST Active between 08-20` + `Centerline Miles` + Population +
             `Measure expired and replaced?` + `Period Active` + `LOST Rate`, data = county_data))

summary(lm(`PCI 2020` ~ `PCI 2008` + `Period Active`, data = county_data))



summary(lm(`Change in PCI` ~ `LOST Active between 08-20`, data = county_data))

summary(lm(`Change in PCI` ~ `PCI 2008` + `LOST Active between 08-20`, data = county_data))

summary(lm(`Change in PCI` ~ `PCI 2008` + `LOST Active between 08-20` + Population, data = county_data))



summary(lm(`Change in PCI` ~ `PCI 2008` + `Period Active`, data = county_data))

summary(lm(`Change in PCI` ~ `PCI 2008` + `Period Active` + `LOST Active between 08-20`, data = county_data))
# I don't know why this works the way it does and I hate it

summary(lm(`Change in PCI` ~ `PCI 2008`
           + `Period Active` 
           + `Did the PCI start below 60?` * `Did the PCI fall below 60?`
           , data = county_data))
# PCI falling below 60 does not seem to have triggered a sudden rush of pavement repair.


summary(lm(`Change in PCI` ~ `PCI 2008`
           + `LOST Active between 08-20` 
           + `Period Active`
           + `Pop per lane miles`
           , data = county_data))
# What the fuck is this
# What the fuck does this mean
# What the fuck


summary(lm(`Change in PCI` ~ `PCI 2008`
           #+ `Democrat %` 
           + `Republican %`
           #+ `Streets Metric` 
           + `Local Return Metric`
           #+ Population
           #+ `Centerline Miles`
           + `Area (sy)`
           #+ CZP2
           #+ CZP4
           + `Pop per lane miles`
           + `Did the PCI start below 60?`
           #+ `Multiple LOSTS Active at same time?`
           , data = county_data))$adj.r.squared

