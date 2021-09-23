fileURL <- "https://uc86a9739bc21dfefad54b46ec78.dl.dropboxusercontent.com/cd/0/inline/BWMkdQ0AEPSUpXK-vv6y8kKOq6B1DzciVuC6wgYjDVjt9Frx1jQPbQgpbVSfMW3QQTigss33GuZxpRBMHrd9O45E_uA94piyi6zqlx436iWrzgiCFdfH63AGIYyUSajlmHCn8N-_pk5Q-b_hu2yTOP1W/file#"

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
