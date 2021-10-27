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

#top10C <- transform(top10C, top10C$`Country, Other`= reorder(top10C$`Country, Other`,))

top10C$`Country, Other` <- as.factor(top10C$`Country, Other`)
top10C$`Total Cases` <- as.numeric(gsub(",","",top10C$`Total Cases`))
str(top10C)
max_y <- round_any(max(top10C$`Total Cases`), 50000000, ceiling)

fig <-ggplot(top10C, aes(x = reorder(top10C$`Country, Other`, `Total Cases`, sum), y = `Total Cases`)) +
  geom_bar(stat = "identity", color = "black", fill = "darkblue") +
  coord_flip() +
  labs(title = "Total Covid Cases by Country(Top 10)", x = "Country", y = "Total Cases") + 
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = scales::comma(cases)), hjust =-0.1, size = 4) +
  scale_y_continuous(labels = comma, breaks = seq(0, max_y, by = 10000000), limits = c(0, max_y)) 
fig

# ------------------------------------------------------ Visual 2 ------------------------------------------------------

top10C$`Total Deaths` <- as.numeric(gsub(",","",top10C$`Total Deaths`))
top10C$Population <- as.numeric(gsub(",","", top10C$Population))
top10C$`Deaths / 1M Population` <- as.numeric(gsub(",","", top10C$`Deaths / 1M Population`))


library(plotly)


fig1 <- plot_ly() %>%
  layout(title = "Deaths/1M of Top 10 Countries in Total cases of COVID") %>%
  add_trace(
    data = top10C,
    labels = ~`Country, Other`,
    values = ~`Deaths / 1M Population`,
    type = "pie",
    textposition = "outside",
    hovertemplate = "Country: %{label}<br>Percent:%{percent}<br>Deaths/1M: %{value}<extra></extra>"
  )
fig1


#----------------------------------------------viz 3 ------------------------------------------------------------
top10C$`Active Cases` <- as.numeric(gsub(",","", top10C$`Active Cases`))
top10C$`Total Recovered` <- as.numeric(gsub(",","", top10C$`Total Recovered`))
top10C$`Total Tests` <- as.numeric(gsub(",","", top10C$`Total Tests`))
top10C$`Serious / Critical Condition` <- as.numeric(gsub(",","", top10C$`Serious / Critical Condition`))

USdf <- top10C[1,]
Idf <- top10C[2,]

fig2 <- plot_ly(hole = 0.4) %>%
  layout(title = "USA Statistics") %>%
  layout(annotations = list(text = paste0("TotalCases: \n",
                                          scales::comma(USdf$`Total Cases`),"\n","\n", 
                                          "Population: \n",
                                          scales::comma(USdf$Population),"\n","\n",
                                          "Total Tests: \n",
                                          scales::comma(USdf$`Total Tests`)),
                                          "showarrow" = F)) %>%
  add_trace(
    data = USdf,
    values = c(USdf$`Active Cases`,USdf$`Total Recovered`,USdf$`Total Deaths`),
    labels = c("Active Cases", "Recovered","Deaths"),
    type = "pie",
    textposition = "inside",
    hovertemplate = "Country: USA<br>%{label}<br>Percent: %{percent}<br>Total: %{value}<extra></extra>"
  )
fig2

fig3 <- plot_ly(hole = 0.4) %>%
  layout(title = "India Statistics") %>%
  layout(annotations = list(text = paste0("TotalCases: \n",
                                          scales::comma(Idf$`Total Cases`),"\n","\n", 
                                          "Population: \n",
                                          scales::comma(Idf$Population),"\n","\n",
                                          "Total Tests: \n",
                                          scales::comma(Idf$`Total Tests`)),
                            "showarrow" = F)) %>%
  add_trace(
    data = Idf,
    values = c(Idf$`Active Cases`,Idf$`Total Recovered`,Idf$`Total Deaths`),
    labels = c("Active Cases", "Recovered","Deaths"),
    type = "pie",
    textposition = "inside",
    hovertemplate = "Country: India<br>%{label}<br>Percent: %{percent}<br>Total: %{value}<extra></extra>"
  )
fig3

max_y <- round_any(max(top10C$`Total Tests`), 700000000, ceiling)

fig4 <-ggplot(top10C, aes(x = reorder(top10C$`Country, Other`, `Total Cases`, sum), y = `Total Tests`)) +
  geom_bar(stat = "identity", color = "black", fill = "darkgreen") +
  labs(title = "Total Covid Tests by Country(Top 10)", x = "Country", y = "Total Tests") + 
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_text(aes(label = scales::comma(`Total Tests`)), vjust =-0.5, size = 4) +
  scale_y_continuous(labels = comma, breaks = seq(0, max_y, by = 100000000), limits = c(0, max_y))
fig4

library(flexdashboard)

summary(top10C)  
