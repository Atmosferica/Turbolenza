kwm<-function(x,y,estr1,estr2){
  minimi<-NULL
  differenze<-NULL
  x<-log10(x)
  y<-log10(y)

  xn<-x[which(x>=estr1&x<=estr2)]
  yn<-y[which(x>=estr1&x<=estr2)]

  min<-(max(xn)+min(xn))*0.5
  for(i in 1:5000){
    fail <-try(function(x){
    ya<-y[which(x>=estr1&x<=min)]
    ydiff<-ya-y[which(x>=min&x<=estr2)]
    if(mean(ydiff)<0){
      min<-min-0.01
      estr1<-estr1-0.01
      estr2<-estr2-0.01
    }else
    {
      min<-min+0.01
      estr1<-estr1+0.01
      estr2<-estr2+0.01
    }
    minimi<-c(minimi,min)
    differenze<-c(differenze,mean(ydiff))
    })
    if(class(fail)=='try-error') min<-NA
  }
  
  min
}