can = read.csv("CANBK.NS.csv")
BB = read.csv("BANKBARODA.csv")
axis = read.csv("AXISBANK.NS.csv")
hdfc = read.csv("HDFCBANK.NS.csv")
boi= read.csv("BANKINDIA.NS.csv")
#bandhan = read.csv("BANDHANBNK.NS.csv")
indus= read.csv("INDUSINDBK.NS.csv")
pnb= read.csv("PNB.NS.csv")
idbi = read.csv("IDBI.NS.csv")
cen = read.csv("CENTRALBK.NS.csv")
yes= read.csv("YESBANK.NS.csv")
icici=read.csv("ICICIBANK.NS.csv")
kotak = read.csv("KOTAKBANK.NS.csv")
#rbl = read.csv("RBLBANK.NS.csv")
sbi = read.csv("SBIN.NS.csv")
nifty = read.csv("NSEI.csv")
#niftyup=read.csv("NSEIup.csv")
rfr <- 3


dropped_n<-grep("null",nifty$Adj.Close)
#dropped_nup<-grep("null",niftyup$Adj.Close)

#niftyup<-niftyup[-dropped_nup,]
nifty<-nifty[-dropped_n,]


clean_null <- function(x,dropped_n)
{
  x<-x[-dropped_n,]
}

bank<- function(bank_name)
{
  dropped=grep("null",bank_name$Adj.Close)
  bank_name=bank_name[-dropped,]
  returns = rep(0,nrow(bank_name))
  for ( i in 1:nrow(bank_name)){
    returns[i] <- ((as.numeric(paste(bank_name$Adj.Close[i+1]))/(as.numeric(paste(bank_name$Adj.Close[i]))))-1)
    }
  average= mean(as.double(returns),na.rm=TRUE)*250*100
  volu=var(returns[1:nrow(bank_name)],na.rm=TRUE)*250*100
  risk= sqrt(volu) 
  vect<-c(average,volu,risk)
  return(vect)
}

RETURNS<-function(bank_name)
{
  returns<-rep(0,nrow(bank_name))
  for(i in 1:nrow(bank_name)){
    returns[i] <- ((as.numeric(paste(bank_name$Adj.Close[i+1]))/(as.numeric(paste(bank_name$Adj.Close[i]))))-1)
  }
  complete<-complete.cases(returns)
  return(returns[complete])
}

SHARPE <- function(ret,rfr,sd)
{
  sharpe_ratio = rep(0,13)
  sharpe_ratio = ((ret - rfr)/sd)
  return(c(sharpe_ratio))
}

bank_names = list(can , BB, axis , hdfc, boi , indus , pnb , idbi, cen, yes, icici, kotak,sbi)

store = rep(0,length(bank_names))
data=data.frame()
for (j in bank_names)
  {
  store<-bank(j)
  data<-rbind(data,store)
}

colnames(data)= c('Expected_Return', 'Volatility','Risk','Sharpe','Ratio_With_Nifty','BETA')
rownames(data)=c('Canara Bank','Bank Of Baroda','Axis','HDFC','Bank Of India','Indus','Punjab National Bank','IDBI','Central Bank OF India','Yes Bank','ICICI','Kotak Bank','SBI')

sharpe=SHARPE(data$Expected_Return,rfr,data$Risk)
data <- cbind(data,sharpe)
ratio = data$Expected_Return/bank(nifty)[1]
data <- cbind(data,ratio)

cov_matrix<-matrix(nrow=13,ncol=13)
for (i in seq_len(nrow(cov_matrix))){
  for (j in seq_len(ncol(cov_matrix))){
    cov_matrix[i,j]<- cov(RETURNS(bank_names[[i]]),RETURNS(bank_names[[j]]))*250
  }
}
cor<-cov2cor(cov_matrix)

weights<-rep(1/13,13)
weights_mat <- matrix(weights,nrow=1,ncol=13)
return_port <- weights_mat %*% as.matrix(data$Expected_Return)
sd_port<- sqrt((weights_mat %*% matrix)%*%t(weights_mat))*100
sharpe_ratio<- (return_port- rfr)/(sd_port)


calc_beta <- function(bank_name)
{
  beta=rep(0,13)
  print(length(RETURNS(bank_name)))
  beta = cov(RETURNS(bank_name),RETURNS(nifty))/ var(RETURNS(nifty))
  return(c(beta))
}

#bank_names = list(can , BB, axis , hdfc, boi , indus , pnb , idbi, cen, yes, icici, kotak,sbi)

can<-clean_null(can,dropped_n)
BB<-clean_null(BB,dropped_n)
axis<-clean_null(axis,dropped_n)
hdfc<-clean_null(hdfc,dropped_n)
boi<-clean_null(boi,dropped_n)
indus<-clean_null(indus,dropped_n)
pnb<-clean_null(pnb,dropped_n)
idbi<-clean_null(idbi,dropped_n)
cen<-clean_null(cen,dropped_n)
yes<-clean_null(yes,dropped_n)
icici<-clean_null(icici,dropped_n)
kotak<-clean_null(kotak,dropped_n)
sbi<-clean_null(sbi,dropped_n)

for (k in 1:13)
{
  beta[k]=calc_beta(bank_names[k])
}
beta[1]=calc_beta(can)
beta[2]=calc_beta(BB)
beta[3]=calc_beta(axis)
beta[4]=calc_beta(hdfc)
beta[5]=calc_beta(boi)
beta[6]=calc_beta(indus)
beta[7]=calc_beta(pnb)
beta[8]=calc_beta(idbi)
beta[9]=calc_beta(cen)
beta[10]=calc_beta(yes)
beta[11]=calc_beta(icici)
beta[12]=calc_beta(kotak)
beta[13]=calc_beta(sbi)

data<- cbind(data,beta)


# Method 2 
# ---------------------
nifty= nifty[1:length(can$Date),]
can$Date <- ymd(can$Date)
nifty$Date <- as.Date(nifty$Date,"%d-%m-%Y")
rangehdfc <- hdfc$Date == nifty$Date
can$Adj.Close <-can$Adj.Close[range]
nifty$Adj.Close <- nifty$Adj.Close[range]

fit<-lm(RETURNS(can) ~ RETURNS(nifty))
result <- summary(fit)
betalm <- result$coefficients[2,1]

#--------------------------

store_ret <- rep(0,13) 
portfolioReturns <- data.frame()
for(k in bank_names){
  store_ret <- RETURNS(k)
  portfolioReturns <- rbind(portfolioReturns,store_ret)
  #portfolioReturns<-t(portfolioReturns)
}
portfolioReturns<- t(portfolioReturns)
portfolioReturns <- as.timeSeries(portfolioReturns)
ef <-  portfolioFrontier(portfolioReturns,constraints = "LongOnly")
plot(ef,1)