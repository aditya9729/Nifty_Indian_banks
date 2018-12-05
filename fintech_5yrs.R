allahabad<-read.csv('ALBK.NS.csv')
united<-read.csv('UNITEDBNK.NS.csv')
uco<-read.csv('UCOBANK.NS.csv')
syndi<-read.csv('SYNDIBANK.NS.csv')
south<-read.csv('SOUTHBANK.NS.csv')
psb<-read.csv('PSB.NS.csv')
laksh<-read.csv('LAKSHVILAS.NS (1).csv')
ktk<-read.csv('KTKBANK.NS (1).csv')
jk<-read.csv('J&KBANK.NS (1).csv')
dhan<-read.csv('DHANBANK.NS (1).csv')
dena<-read.csv('DENABANK.NS (1).csv')
andhra<-read.csv('ANDHRABANK.NS (2).csv')
dcb<-read.csv('DCBBANK.NS (1).csv')
maha<-read.csv('MAHABANK.NS.csv')
orient<-read.csv('ORIENTBANK.NS.csv')
corp<-read.csv('CORPBANK.NS (1).csv')


bank<-function(bank_name)
{
  dropped<-grep("null",bank_name$Adj.Close)
  bank<-bank_name[-dropped,]
  returns<-rep(0,nrow(bank))
  for(i in 1:nrow(bank)){
    returns[i]<-((as.numeric(paste(bank[i+1,6]))/as.numeric(paste(bank[i,6])))-1)
  }
  first_entry<-length(returns)
  expected_returns<-mean(returns[1:first_entry],na.rm=TRUE)
  variance<-var(returns[1:first_entry],na.rm = TRUE)
  er<-expected_returns*250*100
  volu<-variance*250*100
  risk<-sqrt(variance*250*100)
  vect<-c(er,volu,risk)
  return(vect)
}

bank_names<-list(allahabad,united,uco,syndi,south,psb,laksh,ktk,jk,dhan,dena,andhra,dcb,maha,orient,corp)
store1<-rep(0,length(bank_names))

data<-data.frame()
for(i in bank_names){
  store1<-bank(i)
  data<-rbind(data,store1)
}

colnames(data)<-c('Expected_returns','Volatility','Risk')

rownames(data)<-c('Allahabad','United','Uco','Syndi','South','Psb','Laksh','Ktk','Jk','Dhan','Dena','Andhra','Dcb','Maha','Orient','Corp')


RETURNS<-function(bank_name)
{
  returns<-rep(0,nrow(bank_name))
  for(i in 1:nrow(bank_name)){
    returns[i]<-((as.numeric(paste(bank_name[i+1,6]))/as.numeric(paste(bank_name[i,6])))-1)
  }
  complete<-complete.cases(returns)
  return(returns[complete])
}

calc_beta <- function(bank_name)
{
  beta=rep(0,16)
  beta = cov(RETURNS(bank_name),RETURNS(nifty))/ var(RETURNS(nifty))
  return(c(beta))
}
covariance_matrix<-matrix(nrow=16,ncol=16)
for(i in seq_len(nrow(covariance_matrix))){
  for(j in seq_len(ncol(covariance_matrix))){
    covariance_matrix[i,j]<-cov(RETURNS(bank_names[[i]]),RETURNS(bank_names[[j]]))*250
  }
}
covariance_matrix<-cov2cor(covariance_matrix)
#initialize weights
in_weights<-rep(1/16,16)
weight<-matrix(in_weights,nrow=1,ncol=16)
expected_returns_ofport<-in_weights%*%as.matrix(data$Expected_returns)
port_ret<-expected_returns_ofport[1][1]
std_ofport<-sqrt((weight%*%covariance_matrix)%*%t(weight))
port_std<-std_ofport[1][1]*100
rfr<-3
sharpe_ratio<-((port_ret-rfr)/port_std)

#optimize
sum_of_weights<-1
objective.in<-as.vector(data$Expected_returns)
const.mat<-matrix(c())
lm(data$Expected_returns~data$Risk,data=data)

nifty<-read.csv('NSEI.csv')
nullvalues<-grep("null",nifty$Adj.Close)
nifty<-nifty[-nullvalues,]
removenull<-function(x,nullvalues)
{
  x<-x[-nullvalues,]
}
allahabad<-removenull(allahabad,nullvalues)
united<-removenull(united,nullvalues)
uco<-removenull(uco,nullvalues)
syndi<-removenull(syndi,nullvalues)
dcb<-removenull(dcb,nullvalues)
dena<-removenull(dena,nullvalues)
dhan<-removenull(dhan,nullvalues)
andhra<-removenull(andhra,nullvalues)
south<-removenull(south,nullvalues)
ktk<-removenull(ktk,nullvalues)
laksh<-removenull(laksh,nullvalues)
psb<-removenull(psb,nullvalues)
jk<-removenull(jk,nullvalues)
maha<-removenull(maha,nullvalues)
corp<-removenull(corp,nullvalues)
orient<-removenull(orient,nullvalues)


for (k in 1:16)
{
  beta[k]=calc_beta(bank_names[k])
}
beta[1]=calc_beta(allahabad)
beta[2]=calc_beta(united)
beta[3]=calc_beta(uco)
beta[4]=calc_beta(syndi)
beta[5]=calc_beta(south)
beta[6]=calc_beta(psb)
beta[7]=calc_beta(laksh)
beta[8]=calc_beta(ktk)
beta[9]=calc_beta(jk)
beta[10]=calc_beta(dhan)
beta[11]=calc_beta(dena)
beta[12]=calc_beta(andhra)
beta[13]=calc_beta(dcb)
beta[14]=calc_beta(maha)
beta[15]=calc_beta(orient)
beta[16]=calc_beta(corp)
data<-cbind(data,beta)



x_weights <- seq(from = 0, to = 1, length.out = 1000)

# create a data.table that contains the weights for the three assets
three_assets <- data.table(wx = rep(x_weights, each = length(x_weights)),
                           wy = rep(x_weights, length(x_weights)))

three_assets[, wz := 1 - wx - wy]


# calculate the expected returns and standard deviations for the 1000 possible portfolios
three_assets[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z,
                     sd_p = sqrt(wx^2 * sd_x^2 +
                                   wy^2 * sd_y^2 +
                                   wz^2 * sd_z^2 +
                                   2 * wx * wy * cov_xy +
                                   2 * wx * wz * cov_xz +
                                   2 * wy * wz * cov_yz))]