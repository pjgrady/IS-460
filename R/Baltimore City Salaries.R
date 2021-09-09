#https://data.baltimorecity.gov/datasets/baltimore-employee-salaries/explore?showTable=true

getwd()
setwd("/Users/paulgrady/Documents/GitHub/IS-460/R")
getwd()

if(!file.exists("R_datafiles")) dir.create("R_datafiles")

fileURL <- "https://uc60b2c9f1d5100bc12716290dd6.dl.dropboxusercontent.com/cd/0/inline/BV0L2TtYLOzNP4QyI8_vVQPHNu9MMB9DlMehhb1ZyP5Ys-oAr5C4FRyFHjavNE6ucD3vcaCfdEHpv-2zT4LgDDG9EXexyzv_dpcdtq1WYrEJf_jR1Ht7RPtOdCpr7y3_VuycZt1RtqmQnr68DlRYVZUD/file#"

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



