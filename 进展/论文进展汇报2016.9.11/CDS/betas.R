library(data.table)
library(stringr)
setwd("~/Desktop/CDS/")
CDS <- readxl::read_excel('daily_CDS.xlsx')
setDT(CDS)
CDS.melt <- melt(CDS, id.vars = "Date")
CDS.melt[, value := value/100]
CDS.melt[, term:= as.numeric(str_extract(variable, "\\d+"))]
CDS.melt[, term:= ifelse(term==6, 0.5,term)]
CDS.melt[, term:= term*12]

CDS.melt[, X1 := (1 - exp(-0.0609*term))/(0.0609*term)]
CDS.melt[, X2 := (X1 - exp(-0.0609*term))]
CDS.melt[, Date := as.Date(Date)]

CDS.test <- CDS.melt[Date==as.Date("2016-03-29")]

ggplot(data = CDS.test)+
  geom_line(aes(x = term, y = X1), colour = "darkblue", size = 1.5, linetype="dotdash")+
  geom_line(aes(x = term, y = X2), colour = "darkred", size = 1.5, linetype = 2)+
  geom_hline(yintercept = 1, size = 1.5)+
  annotate("text",x = 25, y = 0.95, label="beta[1] * 'Loading(Level)'",size = 5, parse = TRUE)+
annotate("text",x = 23, y = 0.5, label="beta[2] * 'Loading(Slope)'",size = 5, parse = TRUE)+
  annotate("text",x = 25, y = 0.2, label="beta[3] * 'Loading(Curvature)'",size = 5, parse = TRUE)+
  labs(title = "N-S Model Three Factor", x = "Maturity(months)", y = "Yield/Spread")+
  ggthemes::theme_base()
ggsave("betas_loading.png")

Betas <-  CDS.melt[, (as.list(coef(lm(value~X1 + X2)))), by = Date]
names(Betas)[2:4] <- paste0("beta",1:3)
write.csv(Betas,"betas_daily.csv", quote = FALSE, row.names = FALSE)

Betas.melt = copy(Betas)
Betas.melt[,index:=1:.N]
Betas.melt = melt(Betas.melt, id.vars = c("Date", "index"))

day_len <- length(unique(Betas.melt$index))
length.out <- ifelse(day_len>=10, 10, day_len)
breaks <- round(seq(1, day_len, length.out = length.out))
labels =format(unique(Betas.melt$Date)[breaks],"%Y-%m")

ggplot(Betas.melt)+
  geom_line(aes(x = index, y = value, group = variable))+
  facet_grid(variable~., scales = "free_y")+
  labs(x = "Date", title = "Factors Trend(daily)")+
  scale_x_continuous(breaks = breaks,labels = labels)+
  ggthemes::theme_base()
# CDS.test <- CDS.melt[Date==as.Date("2016-04-29")]
# lm(data = CDS.test, formula = value~X1+X2)%>%coef()
ggsave("Factor_trend_daily.png")

# Weekly
CDS.weekly <- copy(CDS)
CDS.weekly[, Date:= as.Date(Date)]
CDS.weekly[, `:=`(Year =  year(Date),
                  week.No = format(Date, "%W"),
               weekday= format(Date, "%w")
          )]
maxWeekDay <- CDS.weekly[, max(weekday), by = .(Year, week.No)]

maxWeekDate <- maxWeekDay[, {pasteDate <- str_c(Year,week.No, V1, sep = "-")
  as.Date(pasteDate, format = "%Y-%W-%w") 
}]

CDS.weekly <- CDS.weekly[Date%in%maxWeekDate]
CDS.weekly.melt <- melt(CDS.weekly, 
                        id.vars = c("Date", "Year", "week.No", "weekday"))

maxWeekDate <- maxWeekDate%>>%
  data.table()%>>%setnames("Date")
maxWeekDate[, index :=1:.N]
#CDS.weekly.melt <- merge(CDS.weekly.melt, maxWeekDate, by = "Date")
CDS.weekly.melt[, value := value/100]
CDS.weekly.melt[, term:= as.numeric(str_extract(variable, "\\d+"))]
CDS.weekly.melt[, term:= ifelse(term==6, 0.5,term)]
CDS.weekly.melt[, term:= term*12]
CDS.weekly.melt[, X1 := (1 - exp(-0.0609*term))/(0.0609*term)]
CDS.weekly.melt[, X2 := (X1 - exp(-0.0609*term))]
CDS.weekly.melt[, Date := as.Date(Date)]


Betas.weekly <-  CDS.weekly.melt[, (as.list(coef(lm(value~X1 + X2)))), by = Date]
names(Betas.weekly)[2:4] <- paste0("beta",1:3)

write.csv(Betas.weekly,"betas_weekly.csv", quote = FALSE, row.names = FALSE)


Betas.weekly.melt = melt(Betas.weekly, id.vars = "Date")
Betas.weekly.melt <- merge(Betas.weekly.melt, maxWeekDate, by = "Date")

day_len <- length(unique(Betas.weekly.melt$index))
length.out <- ifelse(day_len>=10, 10, day_len)
breaks <- round(seq(1, day_len, length.out =  length.out))
labels =format(unique(Betas.weekly.melt$Date)[breaks],"%Y-%m")

ggplot(Betas.weekly.melt)+
  geom_line(aes(x = index, y = value, group = variable))+
  facet_grid(variable~., scales = "free_y")+
  labs(x = "Date", title = "Factors Trend(Weekly)")+
  scale_x_continuous(breaks = breaks,labels = labels)+
  ggthemes::theme_base()

ggsave("Factor_trend_weekly.png")

# Monthly
CDS.monthly <- copy(CDS)
CDS.monthly[, Date:= as.Date(Date)]
CDS.monthly[, `:=`(Year =  year(Date),
                  month.No = month(Date),
                  mday = mday(Date)
)]

maxMonthDate <- CDS.monthly[, max(mday), by = .(Year, month.No)]

maxMonthDate  <- maxMonthDate[, {pasteDate <- str_c(Year,month.No, V1, sep = "-")
as.Date(pasteDate, format = "%Y-%m-%d") 
}]

CDS.monthly <- CDS.monthly[Date%in%maxMonthDate]
CDS.monthly.melt <- melt(CDS.monthly, 
                        id.vars = c("Date", "Year", "month.No", "mday"))

maxMonthDate <- maxMonthDate%>>%
  data.table()%>>%setnames("Date")
maxMonthDate[, index :=1:.N]
#CDS.monthly.melt <- merge(CDS.monthly.melt, maxWeekDate, by = "Date")
CDS.monthly.melt[, value := value/100]
CDS.monthly.melt[, term:= as.numeric(str_extract(variable, "\\d+"))]
CDS.monthly.melt[, term:= ifelse(term==6, 0.5,term)]
CDS.monthly.melt[, term:= term*12]
CDS.monthly.melt[, X1 := (1 - exp(-0.0609*term))/(0.0609*term)]
CDS.monthly.melt[, X2 := (X1 - exp(-0.0609*term))]
CDS.monthly.melt[, Date := as.Date(Date)]


Betas.monthly <-  CDS.monthly.melt[, (as.list(coef(lm(value~X1 + X2)))), by = Date]
names(Betas.monthly)[2:4] <- paste0("beta",1:3)
write.csv(Betas.monthly,"betas_monthly.csv", quote = FALSE, row.names = FALSE)

Betas.monthly.melt = melt(Betas.monthly, id.vars = "Date")
Betas.monthly.melt <- merge(Betas.monthly.melt, maxMonthDate, by = "Date")

day_len <- length(unique(Betas.monthly.melt$index))
length.out <- ifelse(day_len>=10, 10, day_len)
breaks <- round(seq(1, day_len, length.out =  length.out))
labels =format(unique(Betas.monthly.melt$Date)[breaks],"%Y-%m")

ggplot(Betas.monthly.melt)+
  geom_line(aes(x = index, y = value, group = variable))+
  facet_grid(variable~., scales = "free_y")+
  labs(x = "Date", title = "Factors Trend(Monthly)")+
  scale_x_continuous(breaks = breaks,labels = labels)+
  ggthemes::theme_base()
ggsave("Factor_trend_monthly.png")


