
library(geepack)
library(survival)
library(date)
library(splines)
setwd("C:/Users/CH145296/Desktop/R data/")

mklist <- function(n) vector("list",n)

drawnorm <- function(mu,sig,t1=NA,t2=NA,left=FALSE,mid=FALSE,right=FALSE,vble="",lwd=1,col="black")
{
    x <- seq(mu-4*sig,mu+4*sig,length.out=1000)
    y <- (1/(sqrt(2*pi)*sig))*exp((-1/(2*sig^2))*(x-mu)^2)
    plot(x,y,type="n",main="",ylab="Density",xlab=vble)
    t1new <- which.min(abs(x-t1))
    t2new <- which.min(abs(x-t2))
    if (left) for (i in 1:t1new) lines(c(x[i],x[i]),c(0,y[i]),lwd=2,col="red")
    if (mid) for (i in t1new:t2new) lines(c(x[i],x[i]),c(0,y[i]),lwd=2,col="red")
    if (right) for (i in t2new:1000) lines(c(x[i],x[i]),c(0,y[i]),lwd=2,col="red")
    if (!is.na(t1)) lines(c(x[t1new],x[t1new]),c(0,y[t1new]),lwd=3)
    if (!is.na(t2)) lines(c(x[t2new],x[t2new]),c(0,y[t2new]),lwd=3)
    lines(x,y,lwd=lwd,col=col)
    abline(h=0)
}

logreg <- function(formula,save.table=FALSE,save.or=FALSE,unitchange=1)
{
    fit <- glm(formula,family="binomial")

    res <- summary(fit)
    if (save.table)
    {
        fname <- as.character(formula)
        write.table(res,paste(fname,".csv",sep=","))
    }

    num <- length(coef(fit))
    se <- 1.96*(sqrt(diag(vcov(fit))))
    ortab <- matrix(0,num,3)
    for (i in 1:num) ortab[i,] <- round(exp(as.numeric(coef(fit)[i])+c(-1,0,1)*se[i])^unitchange,4)
    if (save.or)
    {
        fname <- as.character(formula)
        write.table(ortab,paste(fname,".csv",sep=","))
    }
    
    return(list(fit=fit,table=res,estimates=ortab))
}

split.date <- function(x,char="/",ymd=FALSE,return.year=FALSE,return.month=FALSE,return.day=FALSE,year2d=FALSE)
{
    library(survival)
    splitone <- function(x,ymd=ymd,return.year=return.year,return.month=return.month,return.day=return.day)
    {
        temp <- strsplit(as.character(x),char)
        month <- rep(0,length(x))
        day <- rep(0,length(x))
        year <- rep(0,length(x))
        res <- rep(NA,length(x))
        if (ymd) {
        if (year2d) year <- 2000 + as.numeric(temp[[1]][1]) else year <- as.numeric(temp[[1]][1])
        month <- as.numeric(temp[[1]][2])
        day <- as.numeric(temp[[1]][3])
        }
        else {
        month <- as.numeric(temp[[1]][1])
        day <- as.numeric(temp[[1]][2])
        if (year2d) year <- 2000 + as.numeric(temp[[1]][3]) else year <- as.numeric(temp[[1]][3])
        }
        if (is.na(day) | is.na(month) | is.na(year)) res <- NA
        else res <- as.numeric(mdy.date(month,day,year))
        if (return.year) return(year) else if (return.month) return (month) else if (return.day) return(day) else return(res)
    }
    return(sapply(x,splitone,ymd=ymd,return.year=return.year,return.month=return.month,return.day=return.day))
}

join.date <- function(x,ymd=FALSE,return.year=FALSE,return.month=FALSE,return.day=FALSE,year2d=FALSE)
{
    library(survival)
    joinone <- function(x,ymd=ymd,year2d=year2d)
    {
        temp <- date.mdy(x)
        month <- as.numeric(temp[[1]])
        day <- as.numeric(temp[[2]])
        year <- as.numeric(temp[[3]])

        if (ymd) res <- paste(year-2000*as.numeric(year2d),month,day,sep="/") else res <- paste(month,day,year,sep="/")
        return(res)
    }
    return(sapply(x,joinone,ymd=ymd,year2d=year2d))
}

univar <- function(x,q=c(0,0.01,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.99,1))
{
    x <- as.numeric(x)
    n <- sum(!is.na(x))
    miss <- sum(is.na(x))
    mu <- mean(x,na.rm=T)
    sig <- sd(x,na.rm=T)
    md <- median(x,na.rm=T)
    mx <- max(x,na.rm=T)
    mn <- min(x,na.rm=T)
    rng <- mx-mn
    iqr <- as.numeric(quantile(x,0.75,na.rm=T) - quantile(x,0.25,na.rm=T))
    quants <- quantile(x,q,na.rm=T)
    res <- list(n=n,missing=miss,mean=mu,median=md,sd=sig,iqr=iqr,range=rng,max=mx,min=mn,quantiles=quants)
    return(res)
}

senspec <- function(dx,gs,sub=rep(T,length(dx)),printab=F)
{
    r <- dx[sub]
    g <- gs[sub]
    
    a <- sum(r==0 & g==0,na.rm=T)
    b <- sum(r==0 & g==1,na.rm=T)
    c <- sum(r==1 & g==0,na.rm=T)
    d <- sum(r==1 & g==1,na.rm=T)
    
    sens <- round(d/(b+d),3)
    spec <- round(a/(a+c),3)
    ppv <- round(d/(c+d),3)
    npv <- round(a/(a+b),3)
    
    if (printab) print(matrix(c(a,b,c,d),nrow=2,byrow=T))
    
    res <- matrix(0,4,5)
    res[1,] <- c(sens,round(prop.test(d,b+d)$conf.int,3),d,b+d)
    res[2,] <- c(spec,round(prop.test(a,a+c)$conf.int,3),a,a+c)
    res[3,] <- c(ppv,round(prop.test(d,c+d)$conf.int,3),d,c+d)
    res[4,] <- c(npv,round(prop.test(a,a+b)$conf.int,3),a,a+b)
    res <- data.frame(res,row.names=c("Sens","Spec","PPV","NPV"))
    names(res) <- c("Est","95 low","95 up","Num","Denom")
    return(res)
}

hosmerlem <- function (y, yhat, g = 10) 
{
    cutyhat <- cut(yhat, breaks = quantile(yhat, probs = seq(0, 
        1, 1/g)), include.lowest = T)
    obs <- xtabs(cbind(1 - y, y) ~ cutyhat)
    expect <- xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
    chisq <- sum((obs - expect)^2/expect)
    P <- 1 - pchisq(chisq, g - 2)
    c("X^2" = chisq, Df = g - 2, "P(>Chi)" = P)
}

groups <- function(dat,num=10)
{
    xbrk <- quantile(dat,seq(0,1,by=(1/num)),na.rm=T)[2:num]
    xcat <- rep(0,length(dat))
    for (i in 1:(num-1)) xcat <- xcat + as.numeric(dat < xbrk[i])
    return(num - xcat)
}
