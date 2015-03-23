require(quantmod)
require(PerformanceAnalytics)
require(TTR)

#get our data from yahoo, use adjusted prices
symbols <- c("SPY", #small cap
             "MDY", #emerging bond
             "EFA", #emerging markets
             "EEM", #intermediate investment grade
             "TLT") #long term treasury (cash)

getSymbols(symbols, from="2012-01-01")
prices <- list()
for(i in 1:length(symbols)) {
  prices[[i]] <- Ad(get(symbols[i]))  
}
prices <- do.call(cbind, prices)
colnames(prices) <- gsub("\\.[A-z]*", "", colnames(prices))

#define our cash asset and keep track of which column it is
cashAsset <- "TLT"
cashCol <- grep(cashAsset, colnames(prices))

#start our data off on the security with the least data (VGSIX in this case)
#prices <- prices[!is.na(prices[,7]),] 

#cash is not a formal asset in our ranking
cashPrices <- prices[, cashCol]
#prices <- prices[, -cashCol]

#define our parameters
nShort <- 20
nLong <- 105
nMonthSMA <- 3

#compute momentums                      
rocShort <- prices/lag(prices, nShort) - 1
rocLong <- prices/lag(prices, nLong) - 1


#take the endpoints of quarter start/end
quarterlyEps <- endpoints(prices, on="months")
monthlyEps <- endpoints(prices, on = "months")

#take the prices at quarterly endpoints
quarterlyPrices <- prices[quarterlyEps,]

#short momentum at quarterly endpoints (20 day)
rocShortQtrs <- rocShort[quarterlyEps,]

#long momentum at quarterly endpoints (105 day)
rocLongQtrs <- rocLong[quarterlyEps,]


#rank short momentum, best highest rank
rocSrank <- t(apply(rocShortQtrs, 1, rank))

#rank long momentum, best highest rank
rocLrank <- t(apply(rocLongQtrs, 1, rank))

#total rank, long slightly higher than short, sum them
totalRank <- 1.01*rocLrank + rocSrank 

#$$$$$$$
totalRank <- t(apply(totalRank, 1, rank))
topNRank <- function(rankRow){
  
  return((rankRow<=1)*1/1)
}
rankPos <- t(apply(totalRank, 1, topNRank))
#$$$$$$$

#function that takes 100% position in highest ranked security
maxRank <- function(rankRow) {
  return(rankRow==max(rankRow))
}

#apply above function to our quarterly ranks every quarter
#rankPos <- t(apply(totalRank, 1, maxRank))
finalPos = rankPos + 0

#SMA of securities, only use monthly endpoints
#subset to quarters
#then filter
#monthlyPrices <- prices[monthlyEps,]
#monthlySMAs <- xts(apply(monthlyPrices, 2, SMA, n=nMonthSMA), order.by=index(monthlyPrices))
#quarterlySMAs <- monthlySMAs[index(quarterlyPrices),]
#smaFilter <- quarterlyPrices > quarterlySMAs


#finalPos <- rankPos*smaFilter
#finalPos <- finalPos[!is.na(rocLongQtrs[,1]),]
#cash <- xts(1-rowSums(finalPos), order.by=index(finalPos))
#finalPos <- merge(finalPos, cash, join='inner')


prices <- merge(prices, cashPrices, join='inner')
returns <- Return.calculate(prices)
stratRets <- Return.portfolio(returns, finalPos)
table.AnnualizedReturns(stratRets)
maxDrawdown(stratRets)
charts.PerformanceSummary(stratRets)
plot(log(cumprod(1+stratRets)))
SPYrets <- Return.calculate(Ad(SPY))
compare <- merge(stratRets, SPYrets, join='inner')
charts.PerformanceSummary(compare["2004::"])


