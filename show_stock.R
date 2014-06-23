decompose_stock <- function(stock)
{
library(quantmod)
from.date <- as.Date("01/01/09", format="%m/%d/%y")
to.date <- as.Date("01/01/14", format="%m/%d/%y")
stock_data <- getSymbols(stock, src="yahoo", from = from.date, to = to.date,auto.assign=FALSE)

stock_data <- to.period(stock_data)
open_stock_data <- Op(stock_data)
ts1 <- ts(open_stock_data, frequency=12)
decom_ts1 <- decompose(ts1)

core <- data.frame(cbind(time(open_stock_data), coredata(open_stock_data)))

gain<- core$stock_data.Open[length(core$stock_data.Open)] / core$stock_data.Open[1]
gain <- ((gain^0.2)-1)*100

gainlist <- numeric()
index <- 1

for(i in seq(from = 12, to = 60,by = 12))
{
  gainlist[index] <- (core$stock_data.Open[i]/core$stock_data.Open[i-11] - 1)*100
  index <- index + 1
}

gainstr <- paste("Stock return of ", round(gain,digits=2), "% and Standard Deviation of ",round(sd(gainlist),2), "%") 

final_plot <- plot(decom_ts1,xlab=gainstr)

final_plot
}

stock_cor <- function(stock1, stock2)
{
  library(quantmod)
  library(PerformanceAnalytics)
  
  from.date <- as.Date("01/01/09", format="%m/%d/%y")
  to.date <- as.Date("01/01/14", format="%m/%d/%y")

  stock_data1 <- getSymbols(stock1, src="yahoo", from = from.date, to = to.date,auto.assign=FALSE)
  stock_data1 <- to.period(stock_data1)
  open_stock_data1 <- Op(stock_data1)
  ts1 <- ts(open_stock_data1, frequency=12)
  ts1 <- Return.calculate(ts1, method = "discrete")
  
  stock_data2 <- getSymbols(stock2, src="yahoo", from = from.date, to = to.date,auto.assign=FALSE)
  stock_data2 <- to.period(stock_data2)
  open_stock_data2 <- Op(stock_data2)
  ts2 <- ts(open_stock_data2, frequency=12)
  ts2 <- Return.calculate(ts2, method = "discrete")
  
  correlation <- cor(ts1,ts2,use="pairwise.complete.obs")
  correlation
}

perm_portfolio <- function(estr,bstr,cstr,ewt,bwt,cwt)
{
  library(quantmod)
  from.date <- as.Date("01/01/09", format="%m/%d/%y")
  to.date <- as.Date("01/01/14", format="%m/%d/%y")
  equity_data <- getSymbols(estr, src="yahoo", from = from.date, to = to.date,auto.assign=FALSE)
  bond_data <- getSymbols(bstr, src="yahoo", from = from.date, to = to.date,auto.assign=FALSE)
  comm_data <- getSymbols(cstr, src="yahoo", from = from.date, to = to.date,auto.assign=FALSE)
  
  equity_data <- to.period(equity_data)
  equity_data <- Op(equity_data)
  
  bond_data <- to.period(bond_data)
  bond_data <- Op(bond_data)
  
  comm_data <- to.period(comm_data)
  comm_data <- Op(comm_data)

  core <- data.frame(cbind(time(equity_data), coredata(equity_data), coredata(bond_data),coredata(comm_data)))
  
  colnames(core) <- c("time", "ep", "bp","cp")
  
  allocation <- 100000
  
  for(i in seq(from = 1, to = 60,by = 12))
  {
    n <- i+11
    for(j in seq(i,i+11))
    {
    core$ea[j] <- allocation * ewt / core$ep[i]
    core$ba[j] <- allocation * bwt / core$bp[i]
    core$ca[j] <- allocation * cwt / core$cp[i]
    core$total[j] <- core$ea[j]*core$ep[j]+core$ba[j]*core$bp[j]+core$ca[j]*core$cp[j]
    }

    allocation <- core$total[i+11]-100
  }
  
  ts1 <- ts(core$total, frequency=12)
  decom_ts1 <- decompose(ts1)
  
  gain<- core$total[length(core$total)] / core$total[1]
  gain <- ((gain^0.2)-1)*100
  
  gainlist <- numeric()
  index <- 1
  
  for(i in seq(from = 12, to = 60,by = 12))
  {
    gainlist[index] <- (core$total[i]/core$total[i-11] - 1)*100
    index <- index + 1
  }
  
  gainstr <- paste("Permanent Portfolio has return of ", round(gain,digits=2), "% and Standard Deviation of ",round(sd(gainlist),2), "%")
  final_plot <- plot(decom_ts1,xlab=gainstr)
  final_plot
}