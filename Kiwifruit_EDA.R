# Packages
library(ggplot2)
library(tigerstats)
library(dplyr)

# Year Per indicator

par(mar = c(5.1, 4.1, 4.1, 14), xpd = T)
with(kf, plot(Year, Value))
with(subset(kf, Indicator == "Ecosystem composition"), points(Year, Value, col = "blue")) # 2005, 2007, 2010
with(subset(kf, Indicator == "Agricultural pests"), points(Year, Value, col = "green"))   # 2005, 2007, 2010
with(subset(kf, Indicator == "Soil status"), points(Year, Value, col = "red"))  #2004, 2006, 2009
title(main = "Enviromental Indicators 2004-2010")
legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA,pch = 19, col = c("blue", "green", "red"), legend = c("Ecosystem Comp", "Agricultural pests", "Soil status"))

barplot(kf$Indicator, kf$Value)

eco <- subset(kf, Indicator == "Ecosystem composition")
pests <- subset(kf, Indicator == "Agricultural pests")
soil <- subset(kf, Indicator == "Soil status")

par(mfrow = c(1,3))
plot(Value ~ Year, data = eco, main = "Ecosystem composition Values", col = "blue")
plot(Value ~ Year, data = pests, main = "Agricultural pests values", col = "green")
plot(Value ~ Year, data = soil, main = "Soil status", col = "red")

# Using ggplot
qplot(Indicator, Value, data = kf, color = Attribute)
qplot(Value, data = kf, facets = .~ Indicator)

ggplot(kf, aes(Value, Indicator)) +
  geom_point(aes(color = Attribute))

#ggplot(kf, aes(Year, fill =Indicator)) +
#geom_bar()

ggplot(kf, aes(Value, fill =Indicator)) +
  geom_histogram() +
  facet_wrap(~ Attribute)

ggplot(eco, aes(Value, fill =Indicator)) +
  geom_histogram(fill = "green4") +
  facet_wrap(~ Attribute)

ggplot(pests, aes(Value, fill =Indicator)) +
  geom_histogram() 
#facet_wrap(~ Attribute)

hist(pests$Value)

ggplot(soil, aes(Value, fill =Indicator)) +
  geom_histogram(fill = "blue") +
  facet_wrap(~ Attribute)

table(kf$Attribute)

favstats(kf$Attribute)

# Group by attributes - summaries statistics
attribute <- group_by(kf, Attribute)

#Summarise: summarise(df, newcolname = ) To generate summary statistics of different columns
names(attribute)
table <- summarise(attribute, mean = mean(Value), median = median(Value), sd = sd(Value), min = min(Value), max = max(Value))
write.csv(table, file = "summary.csv")

# Group by for pests
pestsMgmt <- group_by(pests, Mgmt)

summarise(pestsMgmt, mean = mean(Value))

pestsProperty <- group_by(pests, Property)
summarise(pestsProperty, mean = mean(Value))

pestsCluster <- group_by(pests, Cluster)
summarise(pestsCluster, mean = mean(Value))

## Year Per attribute

with(kf, plot(Year, Value))
with(subset(kf, Attribute == "pests"), points(Year, Value, col = "green"))
with(subset(kf, Attribute == "Ecosystem composition"), points(Year, Value, col = "blue"))
with(subset(kf, Indicator == "Agricultural pests"), points(Year, Value, col = "green"))
with(subset(kf, Indicator == "Soil status"), points(Year, Value, col = "red"))

## Orchard Mgmt system
barplot(table(kf$Value))
with(kf, barplot(Mgmt, table(kf$Value)))