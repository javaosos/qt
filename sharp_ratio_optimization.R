require(quantmod)
require(PerformanceAnalytics)
require(TTR)
library(PortfolioAnalytics)



optimization <- function(R){
  funds <- colnames(R)
  
  #' Construct initial portfolio with basic constraints.
  init.portf <- portfolio.spec(assets=funds)
  init.portf <- add.constraint(portfolio=init.portf, type="full_investment")
  init.portf <- add.constraint(portfolio=init.portf, type="long_only")
  init.portf <- add.objective(portfolio=init.portf, type="return", name="mean")
  #init.portf <- add.objective(portfolio=init.portf, type="risk", name="StdDev")
  #maxSR.lo.ROI <- optimize.portfolio(R=R, portfolio=init.portf,optimize_method="ROI",maxSR = TRUE,trace=TRUE)
  maxSR.lo.ROI <- optimize.portfolio(R=R, portfolio=init.portf,optimize_method="ROI", trace=TRUE)
  
  maxSR.lo.ROI$weights
}


symbols <- c("SPY", #small cap
             "MDY", #emerging bond
             "EFA", #emerging markets
             "EEM", #intermediate investment grade
             "TLT") #long term treasury (cash)

getSymbols(symbols, from="2010-01-01")
prices <- list()
for(i in 1:length(symbols)) {
  prices[[i]] <- Ad(get(symbols[i]))  
}
prices <- do.call(cbind, prices)
colnames(prices) <- gsub("\\.[A-z]*", "", colnames(prices))

R = Return.calculate(prices)

#remove every line with NA in it.
R<-R[-which(apply(R,1,function(x)all(is.na(x)))),]


monthsEnds <- endpoints(R, on="months")
if (monthsEnds[1]==0){
  monthsEnds = monthsEnds[2:length(monthsEnds)]
}


period = 90

finalpos = R[monthsEnds]*0+1/length(symbols)

for (i in 1:length(monthsEnds)){
  if(monthsEnds[i]<period){
    tmp = R[1:monthsEnds[i],]
    finalpos[i,] = optimization(tmp)
    #finalpos[i,] = rep(1/length(symbols),length(symbols))
  }
  else{
    tmp = R[(monthsEnds[i]-period):monthsEnds[i],]
    finalpos[i,] = optimization(tmp)
  }
}



stratRets <- Return.portfolio(R, finalpos)
table.AnnualizedReturns(stratRets)
maxDrawdown(stratRets)
charts.PerformanceSummary(stratRets)
plot(log(cumprod(1+stratRets)))
SPYrets <- Return.calculate(Ad(SPY))
compare <- merge(stratRets, SPYrets, join='inner')
charts.PerformanceSummary(compare["2010::"])
