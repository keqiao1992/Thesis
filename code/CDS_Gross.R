# @Author :KEqiao
# Plot
# CDS_Notional_Outstanding.csv
options(stringsAsFactors = FALSE)
library(data.table)
require(bit64)
library(lubridate)
library(magrittr)
library(ggplot2)
library(ggthemes)

CDS_Outstanding <- fread("~/Downloads/CDS_Notional_Outstanding.csv")

str(CDS_Outstanding)
CDS_Outstanding[,Date := as.Date(Date)]

CDS_Outstanding[,`:=`(year=year(Date),
                      quarter = sprintf("%s-%s", year(Date),quarter(Date)) 
                      )]

CDS_Outstanding_by_year <- CDS_Outstanding[,.(Gross_Notional = last(Gross_Notional)/1e9,
                   Net_Notional = last(Gross_Notional)/1e9,
                   Trade_Counts = sum(Trade_Counts)
                   ),by = year]

CDS_Outstanding_by_quarter <- CDS_Outstanding[,.(
  Date = as.Date(last(Date)),
  Gross_Notional = last(Gross_Notional)/1e9,
                                              Net_Notional = last(Gross_Notional)/1e9,
                                              Trade_Counts = sum(Trade_Counts)
),by = quarter]


ggplot(data = CDS_Outstanding_by_year, aes(x = factor(year), y = Gross_Notional))+
  geom_bar(stat = "identity", width = 0.5)+
  theme_bw(base_size = 12, base_family = "STZhongsong")+
  labs(list(x= "年份", y = "CDS名义总量" ))+
  theme(axis.text.x=element_text(family = "Times", face="bold", size = 12),
        axis.text.y = element_text(size = 12, face = "bold", family = "Times"),
        axis.line = element_line(colour="black", size = 1.2))+
  scale_y_continuous(breaks = seq(0, 30000, 5000))

ggplot(data = CDS_Outstanding_by_year, aes(x = factor(year), y = Trade_Counts))+
  geom_bar(stat = "identity", width = 0.5)




ggplot(data = CDS_Outstanding_by_quarter, aes(x = format(Date,"%Y-%m"), y = Gross_Notional))+
  geom_bar(stat = "identity", width = 0.5)+
  theme_bw(base_size = 12, base_family = "STZhongsong")+
  labs(list(x= "季度", y = "CDS名义总量" ))+
  theme(axis.text.x=element_text(family = "Times",angle=30, face="italic", size = 10),
        axis.text.y = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0, 30000, 5000))

CDS_Outstanding_by_year

