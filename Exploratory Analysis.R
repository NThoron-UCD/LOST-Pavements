library(data.table)
library(ggplot2)
library(tidyverse)
library(MASS)
library(broom)
library(mgcv)

county_data <- fread("Data/Pavements & LOSTs - Sheet 1.csv")
county_data <- subset(county_data, !is.na(CZP1))
county_data$`Most Common Climate Zone` <- as.factor(county_data$`Most Common Climate Zone`)
county_data[is.na(county_data$`Average Streets & Roads Percent`)]$`Average Streets & Roads Percent` <- 0
county_data[is.na(county_data$`Average Local Return Percent`)]$`Average Local Return Percent` <- 0
county_data$`Pop per centerline miles` <- county_data$Population/county_data$`Centerline Miles`
summary(county_data)

county_data <- county_data %>% mutate(
  `Most Common Climate Zone` = as.factor(`Most Common Climate Zone`),
  `Average Streets & Roads Percent` = ifelse(is.na(`Average Streets & Roads Percent`), 0, `Average Streets & Roads Percent`),
  `Average Local Return Percent` = ifelse(is.na(`Average Local Return Percent`), 0, `Average Local Return Percent`),
  `Improve or Decline?` = ifelse(`Change in PCI` >= 0 , 1, 0),
  PercChange = `Change in PCI` / `PCI 2008`
)

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


summary(lm(`Change in PCI` ~ 
           #+ `Average Streets & Roads Percent`
           + `Streets Metric` * Population
           + `Most Common Climate Zone`
           , data = county_data))

# Stepwise regression
full.model <- lm(`Change in PCI` ~ `Centerline Miles` + `Lane-miles` + `Area (sy)` + `Area (sm)`
                 + `PCI 2008` + Population 
                 + `LOST Active between 08-20` 
                 + `Measure expired and replaced?` 
                 + `Period Active` + `LOST Rate` + `Multiple LOSTS Active at same time?` 
                 + `Average Streets & Roads Percent` + `Average Local Return Percent` 
                 + `Streets Metric` + `Local Return Metric` 
                 + `Did the PCI start below 60?` + `Did the PCI fall below 60?` 
                 + `Pop per lane miles` 
                 + `Pop per centerline miles`
                 + `Most Common Climate Zone` 
                 + `Climate Zone Percentage` 
                 + CZP1 + CZP2 + CZP3 + CZP4 
                 + CZP5 + CZP6 + CZP7 + CZP8 
                 + CZP9 + CZP10 + CZP11 + CZP12 
                 + CZP13 + CZP14 + CZP15 + CZP16 
                 + `Democrat %` + `Republican %`
                 , data = county_data)
step.model <- stepAIC(full.model, direction = "both", 
                      trace = TRUE)
summary(step.model)
tidy.step.model <- tidy(step.model)
write.csv(tidy.step.model, "Stepmodel Summary.csv")




log.model <- glm(`Improve or Decline?` ~ 
                    #Population + 
                    #`LOST Active between 08-20` +
                    `Average Streets & Roads Percent`
                    #`Streets Metric` 
                  , data = county_data, family = "binomial")
summary(log.model)

county_data$`PCI 2008 sqr` <- county_data$`PCI 2008`^2


my.model<- lm(`Change in PCI` ~ `Streets Metric`
           +  `Local Return Metric`
           + `PCI 2008`
           #+ `Area (sy)`
           + `Centerline Miles`
           , data = county_data)
summary(my.model)
write.csv(tidy(my.model), "My Model Summary.csv")



# Using Perc change
perc.full.model <- lm(PercChange ~ `Centerline Miles` + `Lane-miles` + `Area (sy)` + `Area (sm)` + `PCI 2008` + Population + `LOST Active between 08-20` + `Measure expired and replaced?` + `Period Active` + `LOST Rate` + `Multiple LOSTS Active at same time?` + `Average Streets & Roads Percent` + `Average Local Return Percent` + `Streets Metric` + `Local Return Metric` + `Did the PCI start below 60?` + `Did the PCI fall below 60?` + `Pop per lane miles` + `Most Common Climate Zone` + `Climate Zone Percentage` + CZP1 + CZP2 + CZP3 + CZP4 + CZP5 + CZP6 + CZP7 + CZP8 + CZP9 + CZP10 + CZP11 + CZP12 + CZP13 + CZP14 + CZP15 + CZP16, data = county_data)
perc.step.model <- stepAIC(perc.full.model, direction = "both", 
                      trace = FALSE)
summary(perc.step.model)

summary(lm(PercChange ~ `Streets Metric`
   + `Local Return Metric`
   + `PCI 2008`
   + `Centerline Miles`
   , data = county_data))
#doesn't improve it imo


summary(lm(`Change in PCI` ~ `PCI 2008`
           + `Democrat %` 
           #+ `Republican %`
           #+ `Streets Metric` 
           #+ `LOST Active between 08-20`
           #+ `Average Streets & Roads Percent`
           + `Average Local Return Percent`
           #+ `Local Return Metric`
           #+ Population
           #+ `Centerline Miles`
           + `Area (sy)`
           #+ CZP2
           #+ CZP4
           #+ `Pop per lane miles`
           + `Pop per centerline miles`
           + `Did the PCI start below 60?`
           #+ `Did the PCI fall below 60?`
           #+ `Multiple LOSTS Active at same time?`
           , data = county_data))#$adj.r.squared

test.model <- lm(`Change in PCI` ~ `PCI 2008` + `Democrat %` + `Average Local Return Percent` + `Area (sy)` + `Pop per centerline miles` + `Did the PCI start below 60?`
   , data = county_data)
county_data$Prediction <- predict.lm(test.model,county_data)

plot(county_data$`Change in PCI`, county_data$Prediction)
