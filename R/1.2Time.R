# Using Dates and Tiemes
library(lubridate)
date_time = c("2021-01-01 11:18:59 AM",
              "2021-11-28 11:10:22 AM",
              "2020-07-26 11:18:33 PM")
df<- data.frame(date_time)
df

x<- ymd_hms(df$date_time)
class(x)

df$year <- year(x)
df$qtr <- quarters(x)

df$month <- month(x)
df$monthName <- months(x, abbreviate = TRUE)

df$yday <- yday(x)
df$mday <- mday(x)
df$daynum <- wday(x)
df$day <- weekdays(x, abbreviate = TRUE)
df$weekend <- wday(x) %in% c(1,7)

df$hr <- hour(x)
df$min <- minute(x)
df$second <- second(x)

df

mytime <- strptime(df$date_time, "%Y-%m-%d %I:%M:%S %p")
mytime
hour(mytime)

strftime(df$date_time, "%a")
df$AM_or_PM <- substring(df$date_time, nchar(as.character(df$date_time))-1)
df
