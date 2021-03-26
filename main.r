library(ggplot2)
library(gtools)
setwd("~/github_repositories/vaccine-tracker")

df = read.csv(file="data-2021-03-25.csv", skip=2, header=T)
df = df[df$Date.Type == "Admin", ]
df = df[df$Program == "US", ]

# cumulative first doses
df = df[,c("Date", "People.Receiving.1.or.More.Doses.Cumulative")]
colnames(df)[1] <- "date"
colnames(df)[2] <- "amount"

df$date <- as.Date(df$date, format="%Y-%m-%d")
df$amount <- as.numeric(df$amount)
df = df[!is.na(df$date), ]
df = df[!is.na(df$amount), ]

df = df[order(df$date),]
start.date = df[1,1]
df$days = as.numeric(difftime(df$date, start.date, units="days"))

population.over.18 = 267000000
df$percent = df$amount/population.over.18

n = dim(df)[1]
df = df[1:(n-5), ]

july.4 = 202
year.1 = 364

milestone.1.date = july.4
proportion.adults = 0.7
df[nrow(df) + 1,] = c(NA, population.over.18 * proportion.adults, milestone.1.date, 1*proportion.adults)
df$date[nrow(df)] = start.date + milestone.1.date;

milestone.2.date = year.1
proportion.adults = 0.9
# df[nrow(df) + 1,] = c(NA, population.over.18 * proportion.adults, milestone.2.date, 1*proportion.adults)
# df$date[nrow(df)] = start.date + milestone.2.date;

fit <- nls(percent ~ SSlogis(days, phi1, phi2, phi3), data=df)

fit.df <- data.frame(days=seq(0,milestone.2.date))
fit.df$date <- start.date + fit.df$days
fit.df$percent <- predict(fit, new=fit.df)
fit.df$amount <- fit.df$percent * population.over.18
fit.df$delta <- fit.df$amount
fit.df$delta[1] <- NA
for (i in 2:nrow(fit.df)) {
  fit.df$delta[i] <- fit.df$amount[i] - fit.df$amount[i - 1]
}

ggplot(df, aes(x=days, y=amount)) +
  geom_point() +
  geom_line(data=fit.df) +
  xlim(0, milestone.2.date)

ggplot(fit.df, aes(x=date, y=delta)) + geom_line()

peak.day = coef(fit)[2]
peak.date = start.date + peak.day
# fit.df$percent[peak.day]
peak.first.doses = fit.df$delta[peak.day]
