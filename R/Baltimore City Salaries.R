#https://data.baltimorecity.gov/datasets/baltimore-employee-salaries/explore?showTable=true

getwd()
setwd("/Users/paulgrady/Documents/GitHub/IS-460/R")
getwd()

if(!file.exists("R_datafiles")) dir.create("R_datafiles")

fileURL <- "https://ucb28a8205c9f07d7e788e05e4b0.dl.dropboxusercontent.com/cd/0/inline/BVyhSkW7qwwoydszDIPxA5BIkfTQMW8bxgAbGj7PoK3_AQ1UIlRyidzRp7Jnux-gFeSueo38_HQk5A4bDFG4iLtyh9GrAndoz3QQD4E4S3y9KYIIwFfSyHgfde6TYkN8OAooR6cZ2ysNZqv1f_VOAWsq/file#"

download.file(fileURL,"R_datafiles//BaltimorCitySalaries.csv", method = "curl")

df <- read.csv("R_datafiles//BaltimorCitySalaries.csv", row.names = "ID")
colnames(df)
head(df)
tail(df)

str(df)
summary(df)

length(unique(df$FiscalYear))

apply(df, 2, function(x) length (unique(x)))

colSums(is.na(df))
table(df$GrossPay <= 0)

df <- df[!is.na(df$GrossPay) & df$GrossPay > 0,]
nrow(df)

ColsToDrop <- c("AgencyID", "HireDate")
df <- df[, !names(df) %in% ColsToDrop]
dim(df)
colnames(df)

library(data.table)
unique(df$AgencyName)
df$AgencyName %ilike% "Library"
table(df$AgencyName %ilike% "Library")



