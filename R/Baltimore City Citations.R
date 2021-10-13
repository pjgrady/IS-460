fileURL <- "https://ucc169b6bbeb629e7eb2399d23a3.dl.dropboxusercontent.com/cd/0/inline/BX5frTAQ8D1TFiyB9y7Txl1Og_r9_yPlE8uoub7wXhdLZpkL7_SIKEP37q6F_Wk900_fnvYDIjh8lEiigLoL7BCg2xYV4P3hxWq1CUr92zVN5m5KLeyqG4QthVG39FLBkLfHTTTc9z65G7x-Nn9AXw3y/file#"

download.file(fileURL, "R_datafiles//Baltimore_Traffic_Citations.csv")



library(data.table)

filename <- "R_datafiles//Baltimore_Traffic_Citations.csv"
df <- fread(filename, na.strings=c(NA, ""))

colnames(df)
head(df)
dim(df)
str(df)
summary(df)

colSums(is.na(df))

na.omit(df$Tag)
unique(na.omit(df$Tag))
length(unique(na.omit(df$Tag)))

library(dplyr)
count(df, Tag)
carcount <- data.frame(count(df, Tag))
carcount 
carcount <- carcount[order(carcount$n, decreasing = TRUE), ]
carcount

df$Tag %in% c(NA, "NO TAGST", "NOTAGS T")
top3 <- df[df$Tag %in% c(NA, "NO TAGST", "NOTAGS T") ,"Description"]
top3
df_top3 <- count(top3, Description)
df_top3 <- df_top3[order(df_top3$n, decreasing = TRUE),]
df_top3

carcount

library(DescTools)
NoTagRows <- which(carcount$Tag %like any% c("%TAG%"))
NoTagRows

NARows <- which(is.na(carcount$Tag))
NARows

NA_and_NoTagRows <- c(NoTagRows, NARows)
NA_and_NoTagRows

carcount[NA_and_NoTagRows, "Tag"]

keep <- c("SCRTAGT", "TAGDAT","LITAG8R", "TAG DAT", "DATAGUY", " TAG ART", "TAG944", "VINTAGE", "WTTAGW1")
keep
keeprows <- which(carcount$Tag %in% keep)
keeprows

NA_and_NoTagRows

rows_to_drop <- setdiff(NA_and_NoTagRows, keeprows)
rows_to_drop

BadTotal <- sum(carcount[rows_to_drop, "n"])
BadTotal

carcount <- carcount[-rows_to_drop,]
head(carcount)

carcount <- rbind(c("No Tags", BadTotal), carcount)
head(carcount)

rownames(carcount) <- c(1:nrow(carcount))
head(carcount )

# Beginning of data visualization

library(ggplot2)

head(carcount, 11)
str(carcount)
carcount$n <- as.numeric(carcount$n)
str(carcount)

ggplot(carcount[2:11,], aes(x = reorder(Tag, -n), y = n)) +
  geom_bar(colour="black", fill = "gray76", stat="identity") +
  labs(title = "Number of Citations by Tag (Top 10)", x = "Car License Plate", y = "Citation Count") +
  theme(plot.title = element_text(hjust = 0.5))

head(df$ViolDate)
library(lubridate)
df$year <- year(mdy_hms(df$ViolDate))
length(unique(df$year))

library(scales)
p <- ggplot(df, aes(x=year)) +
  geom_histogram(bins = 8, color= "darkblue", fill = "lightblue") +
  labs(title = "Historgram of Citation by Year", x = "Year", y = "Count of Citations") +
  scale_y_continuous(labels = comma)+
  stat_bin(binwidth = 1, geom='text', color ='black', aes(label = scales::comma(..count..)), vjust=-0.5)
p

x_axis_labels <- min(df$year) : max(df$year)

p1 <- p1 + scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels)
p1


#------------------Data set up for stacked bar charts-------------------------

df_reasons <- count(df, Description)
head(df_reasons)
df_reasons <- df_reasons[order(df_reasons$n, decreasing = TRUE),]

top_reasons <- df_reasons$Description[1:10]
top_reasons

df$Description %in% top_reasons
table(df$Description %in% top_reasons)

library(dplyr)
new_df <- df %>% 
  filter(Description %in% top_reasons) %>%
  select(ViolDate, Description) %>%
  mutate(year = year(mdy_hms(ViolDate))) %>%
  group_by(Description, year) %>%
  summarise(n = length(Description), .groups = 'keep') %>%
  data.frame()

head(new_df)
new_df

other_df <- df %>%
  filter(!Description %in% top_reasons) %>%
  select(ViolDate) %>%
  mutate(year=year(mdy_hms(ViolDate)), Description = "Other") %>%
  group_by(Description, year) %>%
  summarise(n = length(Description), .groups = 'keep') %>%
  data.frame()
head(other_df)  
other_df  

new_df <- rbind(new_df, other_df)
new_df

agg_tot <- new_df %>%
  select(Description, n) %>%
  group_by(Description) %>%
  summarise(tot = sum(n), .groups = 'keep') %>%
  data.frame()
agg_tot

fines_df <- df %>%
  filter(Description %in% top_reasons) %>%
  select(Description, ViolFine) %>%
  group_by(Description) %>%
  summarise(totfines = sum(ViolFine)) %>%
  data.frame()

fines_df

#---------------- Stacked Bar Plot with Labels ----------------------------
new_df
agg_tot

library(ggplot2)
library(scales)
library(RColorBrewer)
library(ggthemes)
library(plyr)

str(new_df)
new_df$year <- as.factor(new_df$year)

max_y <- round_any(max(agg_tot$tot), 250000,  ceiling)
p3 <- ggplot(new_df, aes(x = reorder(Description, n, sum), y = n, fill = year)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  labs(title = "Citation Count by Citation Type", x = "", y = "Citation Count", fill = "Year") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Paired", guide = guide_legend(reverse = TRUE)) +
  geom_text(data = agg_tot, aes(x = Description, y = tot, label = scales::comma(tot), fill = NULL), hjust = -0.1, size = 4) +
  scale_y_continuous(labels = comma, 
                     breaks = seq(0, max_y, by = 250000),
                     limits = c(0, max_y))
p3
#--------------- Dual Axis on a stacked Bar Chart --------------------------------------------------------------------------------

fines_df
other_sum <- sum(df[!df$Description %in% top_reasons, "ViolFine"])
other_sum
fines_df <- rbind(fines_df, c("Other", other_sum))
fines_df
str(fines_df)
fines_df$totfines <- as.numeric(fines_df$totfines)

ylab <- seq(0, max(fines_df$totfines)/1e6, 10)
ylab
my_labels <- paste0("$", ylab, "M")
my_labels

ggplot(new_df, aes( x = reorder(Description, n, sum), y = n , fill = year)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  theme_light() +
  labs(title = "Citation Count and Total Fines", x = "", y = "Citation Count", fill = "Year") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Spectral", guide = guide_legend(reverse = TRUE)) +
  geom_line(inherit.aes = FALSE, data = fines_df, 
            aes(x = Description, y = totfines/20, colour = "Total Fines", group = 1), size = 1) +
  scale_color_manual(NULL, values = "black") + 
  scale_y_continuous(labels = comma, 
                     sec.axis = sec_axis(~. *20, name = "Total Fines", labels = my_labels,
                                         breaks = ylab*10^6)) +
  geom_point(inherit.aes = FALSE, data = fines_df, 
             aes(x = Description, y = totfines/20, group = 1),
                 size = 3, shape = 21, fill = "white", color = "black") +
  theme(legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent", colour = NA),
        legend.spacing = unit(-1, "lines"))

#------------------ Citation Count by Hour (line plots with highlights and labels) --------------------------------

library(lubridate)
library(dplyr)
library(scales)
library(ggthemes)
library(ggplot2)
library(ggrepel)

hours_df <- df %>%
  select(ViolDate) %>%
  dplyr::mutate(hour24 = hour(mdy_hms(ViolDate))) %>%
  group_by(hour24) %>%
  dplyr::summarise(n = length(ViolDate), .groups = 'keep') %>%
  data.frame()
hours_df
str(hours_df)  

x_axis_labels = min(hours_df$hour24):max(hours_df$hour24)
x_axis_labels

hi_lo <- hours_df %>%
  filter(n == min(n) | n == max(n)) %>%
  data.frame()
hi_lo

p1 <- ggplot(hours_df, aes(x = hour24, y = n)) + 
  geom_line(color = 'black', size = 1) +
  geom_point(shape=21,size=4,color='red', fill='white') +
  labs(x="Hour", y = "Citation Count", title = "Citations by Hour", caption = "Source: Balitmore City Website: www.xyz.com") +
  scale_y_continuous(labels = comma) +
  theme_light() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels, minor_breaks = NULL) +
  geom_point(data = hi_lo, aes(x = hour24, y = n), shape=21, size = 4, fill= 'red', color = 'red') +
  geom_label_repel(aes(label = ifelse(n == max(n) | n == min(n), scales::comma(n), "")), 
                   box.padding = 1, 
                   point.padding = 1, 
                   size = 4, 
                   color='Grey50', 
                   segment.color = 'darkblue')
p1
#-------------------------- Citations by Day of the Week (line plots by year) -----------------------------------------  

library(ggplot2)
library(lubridate)  
library(dplyr)
library(scales)
library(ggthemes)
library(RColorBrewer)

days_df <- df %>%
  select(ViolDate) %>%
  dplyr::mutate(year = year(mdy_hms(ViolDate)),
         dayoftheweek = weekdays(mdy_hms(ViolDate), abbreviate = TRUE)) %>%
  group_by(year, dayoftheweek) %>%
  dplyr::summarise(n = length(ViolDate), .groups = 'keep') %>%
  data.frame()
days_df

str(days_df)
days_df$year <- as.factor(days_df$year)

day_order <- factor(days_df$dayoftheweek, level = c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'))
day_order

p2 <- ggplot(days_df, aes(x = day_order, y = n, group = year)) +
  geom_line(aes(color = year), size = 3) +
  labs(title = "Citations by Day and by Year", x = "Days of the Week", y = "Citation Count") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(shape= 21, size = 5, color = "black", fill = "white") +
  scale_y_continuous(labels = comma) + 
  scale_color_brewer(palette = "Paired", name = "Year", guide = guide_legend(reverse = TRUE))
p2
# -------------------- Citations by Month(multiple bar plots) -------------------------------

library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)
library(ggthemes)
library(RColorBrewer)

months_df <- df %>%
  select(ViolDate) %>%
  dplyr::mutate(months = months(mdy_hms(ViolDate), abbreviate = TRUE), 
         year = year(mdy_hms(ViolDate))) %>%
  group_by(year, months) %>%
  dplyr::summarise(n = length(ViolDate), .groups = 'keep') %>%
  data.frame()
months_df

str(months_df)
months_df$year <- factor(months_df$year)

mymonths <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
month_order <- factor(months_df$months, level = mymonths)

x = min(as.numeric(levels(months_df$year)))
x
y = max(as.numeric(levels(months_df$year)))
y

months_df$year <- factor(months_df$year, levels = seq(y, x, by = -1))

ggplot(months_df, aes(x = month_order, y = n, fill = year)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Multiple Bar Charts - Total Citations by Month by Year",
       x = "Months of the Year",
       y = "Citation Count",
       fill = "Year") +
  scale_fill_brewer(palette = "Set2") +
  facet_wrap(~year, ncol = 4, nrow = 2)


# --------------------- Pie Charts showing Citations by State ---------------------------
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggthemes)

length(unique(df$State))
top_states <- dplyr::count(df, State)
top_states <- top_states[order(-n),]
top_states

top_states[top_states$State %in% c("MD", "VA"),"n"] / sum(top_states$n)

state_df <- df %>%
  select(State, ViolDate) %>%
  dplyr::mutate(year = year(mdy_hms(ViolDate)),
                myState = ifelse(State == "MD", "MD", ifelse(State == "VA", "VA", "Other"))) %>%
  group_by(year, myState) %>%
  dplyr::summarise(n = length(myState), .groups = "keep") %>%
  group_by(year) %>%
  dplyr::mutate(percent_of_total = round(100 * n/sum(n),1)) %>%
  ungroup() %>%
  data.frame()
state_df

state_df[state_df$year == 2013,]

str(state_df)

state_df$myState = factor(state_df$myState, levels = c("MD", "VA", "Other"))
ggplot(data = state_df, aes(x = "", y = n, fill = myState)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_polar(theta = "y", start = 0) + 
  labs(fill = "States", x = NULL, y = NULL, 
       title = "Citation Count by Year and by State",
       caption = "Slices under 5% are not labeled") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  facet_wrap(~year, ncol = 3, nrow = 3) +
  scale_fill_brewer(palette = "Reds") +
  geom_text(aes(x = 1.7, label = ifelse(percent_of_total>5,paste0(percent_of_total, "%"), "")),
            size = 4, 
            position = position_fill(vjust = 0.5))

# ------------------------------    Pie Charts Using Plotly   ------------------------------------------------------  

library(plotly)

state_df

plot_ly(state_df, labels = ~myState, values = ~n, type = "pie",
        textposition = "outside", textinfo = "label + percent") %>%
  layout(title = "Citations by State of Vehicle Registration (2013-2020)")

state_df %>%
  plot_ly(., labels = ~myState, values = ~n, type = "pie",
          textposition = "outside", textinfo = "label + percent") %>%
  layout(title = "Citations by State of Vehicle Registration (2013-2020)")

plot_ly(state_df[state_df$year == 2013,], labels = ~myState, values = ~n, type = "pie",
        textposition = "outside", textinfo = "label + percent") %>%
  layout(title = "Citations by State of Vehicle Registration (2013)")  

plot_ly(state_df, labels = ~myState, values = ~n) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Citations by State of Vehicle Registration (2013-2020)") %>%
  layout(annotations = list(text = paste0("Total Citation Count: \n", 
                            scales::comma(sum(state_df$n))),
                            "showarrow" = F))

# ------------------------- Nested Pie Charts Using Plotly ------------------------------------

library(plotly)

fig <- plot_ly(hole = 0.7) %>%
  layout(title = "Traffic Citations (2013-2015)") %>%
  add_trace(data = state_df[state_df$year == 2015,],
            labels = ~myState, 
            values = ~state_df[state_df$year == 2015, "n"],
            type = "pie",
            textposition = "inside",
            hovertemplate = "Year: 2015<br>State:%{label}<br>Percent:%{percent}<br>Citation Count: %{value}<extra></extra>") %>%
  add_trace(data = state_df[state_df$year == 2014,],
            labels = ~myState, 
            values = ~state_df[state_df$year == 2014, "n"],
            type = "pie",
            textposition = "inside",
            hovertemplate = "Year: 2014<br>State:%{label}<br>Percent:%{percent}<br>Citation Count: %{value}<extra></extra>",
            domain = list(
              x = c(0.16, 0.84),
              y = c(0.16, 0.84))) %>%
  add_trace(data = state_df[state_df$year == 2013,],
            labels = ~myState, 
            values = ~state_df[state_df$year == 2013, "n"],
            type = "pie",
            textposition = "inside",
            hovertemplate = "Year: 2013<br>State:%{label}<br>Percent:%{percent}<br>Citation Count: %{value}<extra></extra>",
            domain = list(
              x = c(0.27, 0.73),
              y = c(0.27, 0.73)))

fig

htmlwidgets::saveWidget(fig, "Nested Pie Chart (2013-2015).html")  

# ---------------------------- Trellis chart in Plotly using Annual Pie Charts ----------------------------------

library(plotly)

fig1 <- plot_ly(textposition = "inside",labels = ~myState, values = ~n) %>%
  add_pie(data = state_df[state_df == 2013,],
          name = "2013", title = "2013", domain = list(row = 0, column = 0)) %>%
  add_pie(data = state_df[state_df == 2014,],
          name = "2014", title = "2014", domain = list(row = 0, column = 1)) %>%
  add_pie(data = state_df[state_df == 2015,],
          name = "2015", title = "2015", domain = list(row = 1, column = 0)) %>%
  add_pie(data = state_df[state_df == 2016,],
          name = "2016", title = "2016", domain = list(row = 1, column = 1)) %>%
  layout(title = "Trellis Chart: Citation Count by Year", showlegend = TRUE,
         grid = list(rows = 2, columns = 2))

htmlwidgets::saveWidget(fig1, "Trellis Chart in Plotly (2013-2016).html")

# --------------------- HeatMaps -----------------------------------

days_df
str(days_df)

mylevels <- c('Mon','Tue','Wed','Thu', 'Fri', 'Sat', 'Sun')
days_df$dayoftheweek <- factor(days_df$dayoftheweek, levels = mylevels)

breaks <- c(seq(0, max(days_df$n), by = 25000))
breaks
g <- ggplot(days_df, aes(x = year, y = dayoftheweek, fill = n)) +
  geom_tile(color = "black") +
  geom_text(aes(label = comma(n))) +
  coord_equal(ratio = 1) +
  labs(title = "Heatmap: Citations by Day of the Week",
       x = "Year",
       y = "Days of the Week",
       fill = "Citation Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_discrete(limits = rev(levels(days_df$dayoftheweek))) +
  scale_fill_continuous(low = "white", high = "red", breaks = breaks) +
  guides(fill = guide_legend(reverse = TRUE, override.aes = list(colour = "black")))


g
p4 <- g
library(plotly)

gg <- ggplotly(g, tooltip = c("n", "year", "dayoftheweek")) %>%
  style(hoverlabel = list(bgcolor = "white"))


htmlwidgets::saveWidget(gg, "Heatmaps in Plotly (Citations by Day of the Week).html")

# ------------------------------ Dashboards ---------------------------------------------------

p1
p2
p3
p4

library(cowplot)
plot_grid(p1, p2, p3, p4, nrow = 2)

library(ggpubr)
ggarrange(p1, p2, p3, p4, nrow = 2, ncol = 2)

library(flexdashboard)

# -------------------------- String Manipulation (lat, long) -----------------------------------

library(stringr)
library(tidyr)

c1 <- '   (31.1  ,      -21.1)'
c2 <- '(     27.6,   -11.1)'
c3 <- '(99.9, -41.1)'

df_lat_long <- data.frame(my_col = c(c1,c2,c3))
df_lat_long

df_lat_long <- separate(df_lat_long, my_col, c("lat", "long"), sep = ",", remove = TRUE)
df_lat_long

df_lat_long$long <- trimws(df_lat_long$long, which = "both")
df_lat_long$lat <- trimws(df_lat_long$lat, which = "both")
df_lat_long

df_lat_long$lat <- as.numeric(substring(df_lat_long$lat, 2,))
df_lat_long
df_lat_long$long <- as.numeric(substring(df_lat_long$long, 1, nchar(df_lat_long$long)-1))
df_lat_long
str(df_lat_long)



