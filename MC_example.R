require("quantmod");require("lubridate");require("pbapply")

ticker <- "SPY"
stock <- getSymbols(ticker,auto.assign = FALSE)
tmp <- getQuote(ticker)
stock <- rbind(stock, xts(cbind(tmp$Open,tmp$High,tmp$Low,tmp$Last,tmp$Volume,tmp$Last), order.by = Sys.Date()))
tmp <- Ad(stock)
rets <- ROC(tmp,type="discrete")
rets[is.na(rets)]<-0
mean(rets)
sd(rets)

stk_ret = function(STK_PRC, N, MEAN, STDEV)
{
  delta_t = 1/N # for 1 period
  for(i in seq(N))
  {
    epsilon <- runif(n=1,min = 0,max=1) # random probs.
    STK_PRC <- STK_PRC * (1 + qnorm(epsilon, MEAN*delta_t, STDEV*sqrt(delta_t)))
  }
  
  STK_PRC

}

last(tmp)
simulations <- 1000
N = 20
STK_PRC <- as.numeric(coredata(tmp[Sys.Date() - days(20)]))
MEAN = mean(rets)
STDEV = sd(rets)


stock_prices <- c()
for(i in seq(simulations))
{
  stock_prices <- c(stock_prices,stk_ret(STK_PRC = STK_PRC, N=N,MEAN=MEAN,STDEV=STDEV))
}

quantile(stock_prices)


EXPIRY <- tmp[options.expiry(tmp)]
EXPIRY <- EXPIRY["2007::"]
IDX <- index(EXPIRY)
NEXT.EXPIRY <- as.Date("2020-06-19")
IDX<- c(IDX,NEXT.EXPIRY)

MEAN = function(calculateUNTIL)
{
  tmp <- tmp[paste0("::",calculateUNTIL)]
  tmp <- ROC(tmp,type="discrete")
  tmp[is.na(tmp)]<-0
  mean(tmp)
}
STDEV = function(calculateUNTIL)
{
  tmp <- tmp[paste0("::",calculateUNTIL)]
  tmp <- ROC(tmp,type="discrete")
  tmp[is.na(tmp)]<-0
  sd(tmp)
}

means <- do.call(rbind,lapply(as.list(IDX), MEAN))
stdevs <- do.call(rbind,lapply(as.list(IDX), STDEV))
days = as.numeric(diff(IDX))


MONTE.CARLO = function(sim,iter,LastIter)
{
  simulations <- sim
  N <- days[iter]
  STK_PRC <- as.numeric(EXPIRY[iter])
  MEAN <- means[iter]
  STDEV <- stdevs[iter]
  stock_prices <- c()
  for(i in seq(simulations))
  {
    stock_prices <- c(stock_prices, stk_ret(STK_PRC = STK_PRC,N=N,MEAN=MEAN,STDEV = STDEV))
  }
  
  START <- as.data.frame(round(STK_PRC,2))
  START.DATE = index(EXPIRY[iter])
  PROBS = as.data.frame(t(round(quantile(stock_prices,probs = seq(0,1,0.05)),2)))
  
  if(iter == LastIter)
  {
    END <- as.data.frame(NA)
    END.DATE = as.data.frame(NA)
  }else{
    END <- as.data.frame(as.numeric(round(EXPIRY[iter+1],2)))
    END.DATE = index(EXPIRY[iter+1])
  }
  all <- cbind(START,START.DATE,PROBS,END,END.DATE)
  colnames(all) <- c("START.PRC","START.DATE","0%","5%","10%","15%","20%","25%","30%","35%","40%","45%","50%","55%",
                     "60%","65%","70%","75%","80%","85%","90%","95%","100%","END.PRC","END.DATE")
  
  all
  
}

p <- pblapply(as.list(1:length(days)), function(x){
  MONTE.CARLO(sim=10000,iter = x, LastIter = length(days))
})
p <- do.call(rbind,p)

plot(p$END.PRC, type="l")
lines(p$`0%`, col='red')
lines(p$`100%`,col='green')

# number of months
nMo <- nrow(p)

# numbers of times it closes above 100%
sum(as.numeric(na.omit(ifelse(p$END.PRC > p$`100%`,1,0))))/nMo

# numbers of times it closes below 0%
sum(as.numeric(na.omit(ifelse(p$END.PRC < p$`0%`,1,0))))/nMo

write.csv(p,"SPY.csv")





