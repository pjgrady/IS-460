fileURL <- "https://uc130af0baae5a3413e2077698eb.dl.dropboxusercontent.com/cd/0/inline/BXgh_O-YIvcNyoa2sfHonEKrWe9O-JxYEmb0gPvLFH2VnRncXkMXenQPPfV2uvHF8dTfz6w7XUuCI2do2iIL0GzN6dsNp51rxPJ7i30BX1fwhqTPG5uNctM76NOcPLgjoSkvsOEH4aH9dBqhkHKyBVQQ/file#"

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
p1 <- ggplot(df, aes(x=year)) +
  geom_histogram(bins = 8, color= "darkblue", fill = "lightblue") +
  labs(title = "Historgram of Citation by Year", x = "Year", y = "Count of Citations") +
  scale_y_continuous(labels = comma)+
  stat_bin(binwidth = 1, geom='text', color ='black', aes(label = scales::comma(..count..)), vjust=-0.5)
p1

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
ggplot(new_df, aes(x = reorder(Description, n, sum), y = n, fill = year)) +
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

ggplot(hours_df, aes(x = hour24, y = n)) + 
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

ggplot(days_df, aes(x = day_order, y = n, group = year)) +
  geom_line(aes(color = year), size = 3) +
  labs(title = "Citations by Day and by Year", x = "Days of the Week", y = "Citation Count") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_point(shape= 21, size = 5, color = "black", fill = "white") +
  scale_y_continuous(labels = comma) + 
  scale_color_brewer(palette = "Paired", name = "Year", guide = guide_legend(reverse = TRUE))



