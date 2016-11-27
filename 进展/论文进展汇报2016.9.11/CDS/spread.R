library(ggplot2)
library(data.table)
library(pipeR)
setwd("~/Desktop/CDS/")
xls.files <- list.files(pattern = ".+Y.xls")
xls.list <- lapply(xls.files, readxl::read_excel)

spot_1Y <- xls.list[[1]]%>>% #"中债国债即期收益率.xls" 
  setDT()%>>%
  setnames(c("Date", "treasury_1Y_spot", "local_gov_1Y_spot", "CT_1Y_spot"))

spot_1Y[,`:=`(Date = as.Date.character(Date),
              local_spread = local_gov_1Y_spot - treasury_1Y_spot,
              CT_spread = CT_1Y_spot - treasury_1Y_spot
)]

spread_1Y_melt <- spot_1Y[,.(Date, local_spread, CT_spread)]
spread_1Y_melt[,index:=1:.N]
spread_1Y_melt <- melt(spread_1Y_melt, id.vars = c("Date", "index"))
day_len <- spread_1Y_melt$Date%>>%unique()%>>%length()
length.out <- ifelse(day_len>=10, 10, day_len)
breaks <- round(seq(1, day_len, length.out =  length.out))
labels =format(unique(spread_1Y_melt$Date)[breaks],"%Y-%m")

ggplot(spread_1Y_melt)+
  geom_line(aes(x = index, y = value, colour = variable))+
  #facet_grid(variable~., scales = "free_y")+
  labs(x = "Date", title = "Spread(1Y)")+
  scale_x_continuous(breaks = breaks,labels = labels)+
  ggthemes::theme_base()

ggsave("spread1Y.png", width = 12, height = 8)



spot_5Y <- xls.list[[2]]%>>% #"中债国债即期收益率.xls" 
  setDT()%>>%
  setnames(c("Date", "treasury_5Y_spot", "local_gov_5Y_spot", "CT_5Y_spot"))

spot_5Y[,`:=`(Date = as.Date.character(Date),
              local_spread = local_gov_5Y_spot - treasury_5Y_spot,
              CT_spread = CT_5Y_spot - treasury_5Y_spot
)]

spread_5Y_melt <- spot_5Y[,.(Date, local_spread, CT_spread)]
spread_5Y_melt[,index:=1:.N]
spread_5Y_melt <- melt(spread_5Y_melt, id.vars = c("Date", "index"))
day_len <- spread_5Y_melt$Date%>>%unique()%>>%length()
length.out <- ifelse(day_len>=10, 10, day_len)
breaks <- round(seq(1, day_len, length.out =  length.out))
labels =format(unique(spread_5Y_melt$Date)[breaks],"%Y-%m")

ggplot(spread_5Y_melt)+
  geom_line(aes(x = index, y = value, colour = variable))+
  #facet_grid(variable~., scales = "free_y")+
  labs(x = "Date", title = "Spread(5Y)")+
  scale_x_continuous(breaks = breaks,labels = labels)+
  ggthemes::theme_base()


ggsave("spread5Y.png", width = 12, height = 8)

