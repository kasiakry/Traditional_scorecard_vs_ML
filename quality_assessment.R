#Functions needed for quality assessment


hosmerlem = function(y, yhat, g=20) {
  cutyhat = cut(yhat,breaks = quantile(yhat, probs=seq(0,1, 1/g)), include.lowest=TRUE)  
  obs = xtabs(cbind(1 - y, y) ~ cutyhat)  
  expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat)  
  chisq = sum((obs - expect)^2/expect)  
  P = 1 - pchisq(chisq, g - 2)  
  return(list(chisq=chisq,p.value=P))
  hr=P
}

cal_psi <- function(data1,data2, bench, target, bin)
{
  ben<-sort(data1[,bench]);
  tar<-sort(data2[,target]);
  # get and sort benchmark and target variable
  ttl_bench<-length(ben);
  ttl_target<-length(tar);
  # get total num obs for benchmark and target
  n<-ttl_bench%/%bin; #Num of obs per bin
  psi_bin<-rep(0,times=bin) #initialize PSI=0 for each bin
  
  for (i in 1:bin) # calculate PSI for ith bin
  {
    
    lower_cut<-ben[(i-1)*n+1];
    if(i!=bin){upper_cut<-ben[(i-1)*n+n]; pct_ben<-n/ttl_bench;} else
    {upper_cut<-ben[ttl_bench];
    pct_ben<(ttl_bench-n*(bin-1))/ttl_bench;}
    #last bin should have all remaining obs
    
    pct_tar<-length(tar[tar>lower_cut&tar<=upper_cut])/ttl_target;
    psi_bin[i]<-(pct_tar-pct_ben)*log(pct_tar/pct_ben);
  }
  psi<-sum(psi_bin);
  return(psi);
}


cal_psi_zm <- function(data1,data2, bench, target)
{
  ben<-sort(data1[,bench]);
  tar<-sort(data2[,target]);
  bin<-length(unique(ben))
  bin_tar<-length(unique(tar))
  # get and sort benchmark and target variable
  ttl_bench<-length(ben);
  ttl_target<-length(tar);
  # get total num obs for benchmark and target
  tab_ben<-table(ben)
  pct_ben<-tab_ben/ttl_bench
  names<-names(tab_ben)
  tab_tar<- as.data.frame(table(tar))
  
  
  tab_tar<-merge(as.data.frame(tab_ben),tab_tar,by.x="ben",by.y="tar")
  tab_tar[,is.na(tab_tar)]<-0
  
  pct_tar<-tab_tar[,3]/ttl_target
  pct_ben<-tab_tar[,2]/ttl_bench
  psi_bin<-rep(0,times=bin) #initialize PSI=0 for each bin
  
  psi_bin<-(pct_tar-pct_ben)*log(pct_tar/pct_ben);
  psi<-sum(psi_bin);
  return(psi);
}