getwd()

file1 <- "R_datafiles//Employee Master.csv"
file2 <- "R_datafiles//Timesheets.csv"

library(data.table)
df1 <- fread(file1)
df2 <- fread(file2)
df1
df2

library(sqldf)
df_inner <- sqldf(" select *
                    from df1 inner join df2 using(employeeid)
                  ")
df_inner

df_left <- sqldf("select *
                  from df1 left join df2 using(employeeid)
                 ")

df_left

df_leftNull <- sqldf("select *
                  from df1 left join df2 using(employeeid)
                  where df2.employeeid IS null
                 ")

df_leftNull

df3 <- merge(x=df1, y=df2, by=NULL)
df3

library(plyr)
join(df1,df2,by=c("EmployeeID"), type="full")

df4 <- merge(x=df1, y=df2, by = "EmployeeID", all=TRUE)
df4

df5 <- merge(x=df1, y=df2, by = "EmployeeID", all.x =TRUE)
df5

df6 <- merge(x=df1, y=df2, by = "EmployeeID", all.y=TRUE)
df6

library(dplyr)
df1$EmployeeID <- as.numeric(df1$EmployeeID)
df2$EmployeeID <- as.numeric(df2$EmployeeID)

inner_join(df1,df2)
left_join(df1,df2)
right_join(df1,df2)
full_join(df1,df2)


