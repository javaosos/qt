# library("quantmod")
# library("PerformanceAnalytics")
# 
# backtestStartDate = as.Date("2012-01-02") #Starting date for the backtest
# getSymbols("SPY", src = "yahoo", from = backtestStartDate)
# getSymbols("FXI", src = "yahoo", from = backtestStartDate)
# 
# 
# 
# plot(coredata(Ad(SPY)),col="red",ylab="SPY",type='l',lty=2,ylim=c(0,250))
# lines(coredata(Ad(FXI)),col="blue",type='l')




require(downloader)
require(PerformanceAnalytics)


simulateTrading <- function(ETFlist, weightlist, longshortlist, startdate){
  retList = zoo()
  getSymbols("SPY", from=startdate)
  SPYrets <- Return.calculate(Ad(SPY))
  for (i in 1:length(ETFlist)){
    getSymbols(ETFlist[i], from=startdate)
    ret <- longshortlist[i]*Return.calculate(Ad(eval(parse(text=ETFlist[i]))))
    retList = cbind(retList, ret)
  }

  stratRets <- Return.portfolio(R = retList, weights = weightlist, rebalance_on="weeks")
  charts.PerformanceSummary(stratRets)
  compare <- merge(stratRets, SPYrets, join='inner')
  charts.PerformanceSummary(compare)
  #simulateTrading = retList
}

etflist = c("SPXU","TMV")
startdate = as.Date("2013-12-29")
weightlist = c(0.5,0.5)
longshortlist = c(-1,-1)


# etflist = c("SPXL","ZIV","TMF","TVIX")
# startdate = as.Date("2011-01-01")
# weightlist = c(0.4, 0.2, 0.35, 0.05)
# longshortlist = c(1, 1, 1, 1)

simulateTrading(etflist, weightlist, longshortlist, startdate)





# backtestStartDate = as.Date("2014-05-29") #Starting date for the backtest
# getSymbols("SPY", from=backtestStartDate)
# 
# 
# getSymbols("TMV", from=backtestStartDate)
# TMVrets <- Return.calculate(Ad(TMV))
# 
# 
# getSymbols("TVIX", from=backtestStartDate)
# TVIXrets <- Return.calculate(Ad(TVIX))
# 
# components <- cbind(TMVrets*-1, TVIXrets*-1)
# stratRets <- Return.portfolio(R = components, weights = c(0.75, 0.25), rebalance_on="weeks")
# charts.PerformanceSummary(stratRets)
# SPYrets <- Return.calculate(Ad(SPY))
# compare <- merge(stratRets, SPYrets, join='inner')
# charts.PerformanceSummary(compare["2014::"])