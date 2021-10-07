library(data.table)
library(lubridate)
library(scales)
library(ggthemes)
library(ggplot2)
library(ggrepel)
library(plyr)
library(dplyr)
df <- fread("R_datafiles//Covid Data - World.csv", na.strings=c(NA, ""))

colnames(df)
head(df)
dim(df)
str(df)
summary(df)

colSums(is.na(df))


#make top 10 list, because all countries is too many for graph

top10C <- df[with(df,order(-'Total Cases')),]
top10C <- df[1:10,]

top10C <- transform(top10C, top10C$`Country, Other` = reorder(top10C$`Country, Other`,))

country <- as.factor(top10C$`Country, Other`)
country
cases <- as.numeric(gsub(",","",top10C$`Total Cases`))
cases

max_y <- round_any(max(cases), 50000000, ceiling)

ggplot(top10C, aes(x = reorder(country, cases, sum), y = cases)) +
  geom_bar(stat = "identity", color = "darkblue", fill = "lightblue") +
  coord_flip() +
  labs(title = "Total Covid Cases by Country(Top 10)", x = "Country", y = "Total Cases") + 
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = scales::comma(cases)), hjust =-0.1, size = 4) +
  scale_y_continuous(labels = comma, breaks = seq(0, max_y, by = 10000000), limits = c(0, max_y)) 



pop <- as.numeric(gsub(",","",top10C$Population))
pop
  
ggplot(top10C, aes(x = reorder(country, cases, sum), y = cases)) +
  geom_bar(stat = "identity", color = "darkblue", fill = "lightblue") +
  coord_flip() +
  labs(title = "Total Covid Cases by Country(Top 10)", x = "Country", y = "Total Cases") + 
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = scales::comma(cases)), hjust =-0.1, size = 4) +
  scale_y_continuous(labels = comma, breaks = seq(0, max_y, by = 10000000), limits = c(0, max_y))+
  geom_line(inherit.aes = FALSE, data = top10C, aes(x = country, y = pop))








  
  
