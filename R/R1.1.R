set.seed(1)

col1 <- sample(1:10, replace = TRUE)
col1

col2 <- sample(10:100, 10, replace = FALSE)
col2

col3 <- sample(LETTERS, 10, replace = TRUE)
col3

col4 <- seq(from=1, to=28, by=3)
col4

col5 <- c(1,2,3,4,5,6,"hi", "bye", 9,10)
col5

df <- data.frame(col1,col2,col3,col4,col5)
df

df$newcol <- c(2,4,6,8,10,2,4,6,8,10)
df

row.names(df) <- letters[(1:nrow(df))]
df

class(col3)
class(df$newcol)
class(df)

head(df, 2)
tail(df, 3)

dim(df)
summary(df)
str(df)

nrow(df)
ncol(df)

df[5,1]
df[1,]
df[,1]
df["d",]
df[,"col2"]
df$col4
df[c(1,2,5),c(1,2)]
df[c("a","b","e"),c("col1", "col2")]


#-------------------------------------------

df$newcol == 10
df[df$newcol == 10,]
df[df$newcol == 10, "newcol"]
df[df$newcol == 10, "newcol"] <- 100
df

df$col3[c(1,3,5)] <- NA
df

sum(is.na(df))

sum(!is.na(df))

colSums(is.na(df))
colSums(!is.na(df))

rowSums(is.na(df))
rowSums(!is.na(df))

df$col2 >= 50
df[df$col2 >= 50,]
row.names(df[df$col2 >= 50,])
which(df$col2 >= 50)

df[df$col2 >= 50 & df$col2 <= 90,]

(df$col2 >= 50 & df$col2 <= 90) & (df$col3 >= "T" | df$col3 <= "Y") & !is.na(df$col3)
which((df$col2 >= 50 & df$col2 <= 90) & (df$col3 >= "T" | df$col3 <= "Y") & !is.na(df$col3))
df[(df$col2 >= 50 & df$col2 <= 90) & (df$col3 >= "T" | df$col3 <= "Y") & !is.na(df$col3),]
nrow(df[(df$col2 >= 50 & df$col2 <= 90) & (df$col3 >= "T" | df$col3 <= "Y") & !is.na(df$col3),])

df$col1 + df$col2
with(df, col1 + col2)
df$result <- df$col1 +df$col2
df
df$result <- ifelse(is.na(df$col3), df$col1 +df$col2, 0)
df

sort(df$col1, decreasing = TRUE)
df
order(df$col1, decreasing = FALSE)
df
order(-df$col1, -df$col2)
order(df$col1, df$col2, decreasing = c(TRUE, FALSE))
df

df2 <- df[order(-df$col1, -df$col2),]
df2
