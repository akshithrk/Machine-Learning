
setwd("C:/Users/CH145296/Desktop/R data/")
source("C:/Documents and Settings/CH145296/My Documents/R Files/helperfunctions.r")
source("C:/Documents and Settings/CH145296/My Documents/R Files/ppsq_homepn_functions.r")
source("C:/Documents and Settings/CH145296/My Documents/R Files/bmi_z.r")

###

today <- as.integer(mdy.date(12,31,2015))
readdata()
mrnlist <- unique(all.dat$mrn)

###
# MRN lists for various cohorts
###

alclocklist <- c(4341595,4267723,4532864,4648347,4582803,4626209,4645701,4425867,4433272,4455943,4275455,4233505,4294977,4495009,4689298) # 15 patient cohort with alcohol locks
telemedlist <- c(1232971,2154352,4666503,4736629,4742482,4780152,4796882,4848671) # 8 patient cohort of telemedicine cases
telemedmatchlist <- c(4666107, 4626209, 4582803, 4658607, 4648347, 4573588, 4768114, 4772922, 4653035,4751843, 4486535, 4015089, 4084730, 4055829, 4481929, 4550494) # 8 matched controls
telemedcontrollist <- c(1250557,2152162,2283140,4015089,4031955,4055829,4063662,4084730,4140680,4164134,4274540,4299158,4356052,4434946,4481929,4484673,4486535,
                4507139,4510144,4516588,4521115,4532864,4535219,4539126,4550494,4552605,4567096,4573588,4582803,4590009,4598279,4601432,4612687,4614614,
                4626209,4637187,4640286,4643660,4645701,4648347,4653035,4658607,4666107,4682205,4689298,4692021,4708026,4719616,4724680,4741710,4751843,
                4764295,4767469,4768114,4772922,4826998) # 56 patient cohort of telemedicine controls
validationlist <- c(4877831,4862326,4794680,4774766,4741079)
highrisk1 <- c(4233505,4267723,4275455,4294977,4341595,4425867,4433272,4455943,4495009,4532864,4582803,4626209,4645701,4648347,689298)
highrisk2 <- c(4233505,4267723,4275455,4294977,4341595,4425867,4433272,4455943,4495009,4532864,4582803,4626209,4645701,4648347,689298)
lowrisk1 <- c(352225,353847,1017021,1027207,1098632,2077612,2152049,2154352,2248746,2273380,2312146,2321215,4005971,4014109,
                4027482,4054296,4055829,4095149,4105397,4115133,4123887,4126432,4140680,4140749,4150209,4174433,4177080,4183553,
                4186995,4194427,4247613,4252811,4260593,4277218,4294431,4299946,4317463,4320391,4323727,4331497,4343492,4353872,
                4355754,4356052,4388567,4397815,4401398,4409083,4410975,4434946,4438273,4440634,4458379,4484673,4516588,4520698,
                4526356,4527640,4535219,4539804,4540560,4559200,4565407,4568270,4573588,4590009,4612687,4637187,4640286,4643660,
                4651824,4653035,4658607,4661676,4664431,4666107,4666503,4669346,4682205,4683080,4692021,4708026,4715874,4719616,
                4724680,4733238,4736629,4738115,4741710,4742482,4751613,4751843,4764295,4767469,4768114,4772379,4772922,4780152,
                4790205,4791818,4793692,4794680)
lowrisk2 <- c(352225,353847,1098632,1232971,2077612,2152049,2154352,2248746,2273380,2312146,2321215,4005971,4014109,4027482,4054296,
                4055829,4095149,4115133,4123887,4126432,4140680,4150209,4177080,4181585,4186995,4194427,4247613,4252811,4260593,4277218,
                4299946,4317463,4320391,4331497,4355754,4356052,4388567,4389337,4397815,4401398,4403186,4409083,4410975,4434946,4438273,
                4440634,4449346,4484673,4526356,4527640,4539804,4540560,4559200,4573588,4585065,4612687,4637187,4640286,4643660,4651824,
                4653035,4661676,4664431,4666107,4666503,4669346,4683080,4700798,4715874,4719616,4733238,4736629,4738115,4741079,4741710,
                4751613,4764295,4767469,4768114,4772379,4772922,4774766,4780152,4790205,4791818,4793692,4794680,4796882,4816351,4816547,
                4826998,4834902,4838949,4841176,4848671,4858091,4862326,4864238,4868969,4871820,4873598,4873723,4873769,4877831,4889465,
                4898219,4912540,4914556)

#####
# Main report
#####

prepdata(mrnlist)
#prepdata(highrisk1,mdy.date(5,1,2014),mdy.date(2,28,2015))
#prepdata(highrisk2,mdy.date(3,1,2015),mdy.date(12,31,2015))
#prepdata(lowrisk1,mdy.date(5,1,2014),mdy.date(2,28,2015))
#prepdata(lowrisk2,mdy.date(3,1,2015),mdy.date(12,31,2015))

nyears <- 5
clreport <- matrix(NA,12*nyears,18)
for (i in 1:nyears) for (j in 1:12)
{
    index <- (12*(i-1)+j)
    clyr <- as.integer(format(Sys.Date(),"%Y")) - i + 1
    clmo <- j
    m1 <- mdy.date(j,1,clyr)
    if (j==12) m2 <- mdy.date(12,31,clyr) else m2 <- mdy.date(j+1,1,clyr)-1
    clreport[index,] <- c(clmo,clyr,calcdash(m1,m2))
}

clreport <- data.frame(clreport)
names(clreport) <- c("month","year","cldays","cldaysnew","clabsi","clabsinew","clabsirate","npats","unplanhosp","los.median","percout","newhpn","death","transfer","weanoff","outptenc","remclabsi","medbmi")
clreport <- clreport[order(clreport$year,clreport$month),]
clreport <- clreport[-which(clreport$year>2015),]
clreport <- data.frame(obs=(1:length(clreport$month)),clreport)
write.table(clreport,"ppsq-homepn-clreport-monthly.csv",col.names=T,row.names=F,sep=",")

#####
# Validation report
#####

restab <- matrix(NA,5,17)
for (i in 1:5)
{
    readdata()
    prepdata(validationlist[i])
    restab[i,] <- c(validationlist[i],calcdash())
}
restab <- data.frame(restab)
names(restab) <- c("mrn","cldays","cldaysnew","clabsi","clabsinew","clabsirate","npats","unplanhosp","los.median","percout","newhpn","death","transfer","weanoff","outptenc","remclabsi","medbmi")
write.table(restab,"ppsq-homepn-clreport-validation.csv",col.names=T,row.names=F,sep=",")

#####
# Demographic table - alcohol locks
#####

prepdata(alclocklist,mdy.date(10,1,2014),mdy.date(9,30,2015))
restab <- calcdemog(mdy.date(3,1,2015),tabprint=T)

#####
# Control charts
#####

pdf("ppsq-homepn-controlcharttest-nearfinal.pdf",width=10.5,height=8)

# PAGE 1 -- CLABSI rate

x <- clreport$clabsirate
x1 <- clreport$clabsirate[1:25]
x2 <- clreport$clabsirate[26:length(x)]
x3 <- clreport$clabsinew/clreport$cldaysnew
x3 <- replace(x3,is.na(x3),NA)

bl1 <- mean(log(replace(x1,x1==0,NA)),na.rm=T)
blsd1 <- sd(log(replace(x1,x1==0,NA)),na.rm=T)
uplim1 <- exp(bl1+1.645*blsd1)
outlier1 <- (x1 > uplim1)

bl2 <- mean(log(replace(x2,x2==0,NA)),na.rm=T)
blsd2 <- sd(log(replace(x2,x2==0,NA)),na.rm=T)
uplim2 <- exp(bl2+1.645*blsd2)
outlier2 <- (x2 > uplim2)

par(xaxt="n")b
plot(x,type="n",lwd=3,main="Community acquired CLABSI rate per 1000 line days ",xlab="Calendar year",ylab="Rate",ylim=c(0,1.15*max(uplim1,uplim2,na.rm=T)),frame=T)
for (i in 1:10) lines(c(24.5 + i/10,24.5 + i/10),c(0,1.15*max(uplim1,uplim2)),lwd=3,col="grey")
lines(x,lwd=3)
#lines(x3,lwd=2,lty=4)
lines(c(0,25),c(exp(bl1),exp(bl1)),lty=1)
lines(c(25,length(x)),c(exp(bl2),exp(bl2)),lty=1)
par(xaxt="s")
axis(1,at=(0:3)*12 + 4,labels=c("Jan 2012","Jan 2013","Jan 2014","Jan 2015"))
axis(1,at=(0:12)*4 + 4,labels=NA)
lines(c(0,25),c(uplim1,uplim1),lty=2)
lines(c(25,length(x)),c(uplim2,uplim2),lty=2)
points(seq(1,length(x1))[outlier1],x1[outlier1],pch=19,cex=1.2,col="red")
points(seq(length(x1)+1,length(x1)+length(x2))[outlier2],x2[outlier2],pch=19,cex=1.2,col="red")
#legend("topright",legend=c("All patients","New HPN"),lty=c(1,4),lwd=c(3,2))


# PAGE 2 -- Unplanned hospitalization rate

x <- 1000*clreport$unplanhosp/clreport$cldays
bl <- mean(log(replace(x,x==0 | is.na(x),NA)),na.rm=T)
blsd <- sd(log(replace(x,x==0,NA)),na.rm=T)
uplim <- exp(bl+1.645*blsd)
outlier <- (x > uplim)

par(xaxt="n")
plot(x,type="l",lwd=3,main="Unplanned hospitalization rate per 1000 line days",xlab="Calendar year",ylab="Rate",ylim=c(0,1.05*uplim),frame=T)
abline(h=exp(bl),lty=1)
par(xaxt="s")
axis(1,at=(0:4)*12,labels=(2011:2015))
abline(h=uplim,lty=2)
points(seq(1,length(x))[outlier],x[outlier],pch=19,cex=1.2,col="red")

# PAGE 3 -- Median BMI

x <- clreport$medbmi
bl <- mean(x,na.rm=T)
blsd <- sd(x,na.rm=T)
uplim <- (bl+1.645*blsd)
outlier <- (x > uplim)

par(xaxt="n")
plot(x,type="l",lwd=3,main="Median BMI percentile",xlab="Calendar year",ylab="BMI percentile",ylim=c(40,90),frame=T)
abline(h=bl,lty=1)
par(xaxt="s")
axis(1,at=(0:4)*12,labels=(2011:2015))
#abline(h=uplim,lty=2)
#points(seq(1,length(x))[outlier],x[outlier],pch=19,cex=1.2,col="red")

dev.off()


###
# Figures for annual QMP
###

outpt.figs <- matrix(NA,4,3)
nsub.figs <- rep(NA,4)
newhpn.figs <- rep(NA,4)
hosp.figs <- rep(NA,4)

for (i in 1:4)
{
    targetyear <- 2011 + i
    m1 <- as.integer(mdy.date(1,1,targetyear))
    m2 <- as.integer(mdy.date(12,31,targetyear))
    thisoutpt <- outpt.dat[!(outpt.dat$datein > m2 | outpt.dat$datein < m1),]
    thisactive <- active.dat[!(active.dat$datein > m2 | active.dat$dateout < m1),]
    thisactive1 <- active.dat[!(active.dat$datein > m2 | active.dat$datein < m1),]
    thishosp <- hosp.dat[!(hosp.dat$dateout > m2 | hosp.dat$dateout < m1),]

    ### Clinic visits by year

    if (length(thisoutpt$datein)>0)
    {
    outpt.figs[i,1] <- sum(thisoutpt$clinic_id==1,na.rm=T)
    outpt.figs[i,2] <- sum(thisoutpt$clinic_id==2,na.rm=T)
    outpt.figs[i,3] <- sum(thisoutpt$clinic_id %in% c(1,2),na.rm=T)
    if (i==4) outpt.figs[4,] <- (12/7)*outpt.figs[4,]
    }

    ### Unique patients by year

    if (length(thisactive$datein)>0) nsub.figs[i] <- length(unique(thisactive$active_mrn))
    #if (i==5) nsub.figs[5] <- (12/7)*nsub.figs[5]

    ### New starts by year

    if (length(thisactive1$datein)>0) newhpn.figs[i] <- length(unique(thisactive1$active_mrn[thisactive1$st_start==1]))
    if (i==4) newhpn.figs[4] <- (12/7)*newhpn.figs[4]

    ### Hospital discharges at BCH by year

    if (length(thishosp$dateout)>0) hosp.figs[i] <- sum(thishosp$hosp_name==1,na.rm=T)
    if (i==4) hosp.figs[4] <- (12/7)*hosp.figs[4]
}

restab <- data.frame(2012:2015,outpt.figs,nsub.figs,newhpn.figs,hosp.figs)
names(restab) <- c("year","outpt.hpn","outpt.sbs","outpt.all","active.service","new.starts","discharges")
write.csv(restab,"ppsq-homepn-annualfigs.csv")

pdf("ppsq-homepn-annualfigs.pdf",width=10.5,height=8)

plot(2012:2015,outpt.figs[,3],type="l",main="Home PN outpatient visits by location",ylab="Total patient visits",xlab="Year",ylim=c(0,1300),lwd=2)
lines(2012:2015,outpt.figs[,1],lwd=2,lty=2)
lines(2012:2015,outpt.figs[,2],lwd=2,lty=3)
legend("bottomleft",legend=c("All","HPN","SBS"),lty=c(1,2,3),lwd=c(2,2,2))

plot(2012:2015,nsub.figs,type="l",main="Home PN unique patients on active service",ylab="Total patients",xlab="Year",lwd=2)
plot(2012:2015,newhpn.figs,type="l",main="New start Home PN unique patients enrolled",ylab="Total patients",xlab="Year",lwd=2)
plot(2012:2015,hosp.figs,type="l",main="Home PN hospital discharges at BCH",ylab="Total patients",xlab="Year",lwd=2)

dev.off()

###
# Same day central line report
###

prepdata(mrnlist)
nsub <- length(demog.dat$mrn)
clflag <- rep(NA,nsub)
for (i in 1:nsub)
{
    this.dat <- cl.dat[cl.dat$cvc_mrn==demog.dat$mrn[i],]
    clflag[i] <- as.numeric(sum(this.dat$remove_date=="",na.rm=T)-sum(this.dat$insert_date=="",na.rm=T) > 1)
}
cldouble <- cl.dat[cl.dat$cvc_mrn %in% demog.dat$mrn[clflag==1],]
cldouble <- cldouble[order(cldouble$cvc_mrn,cldouble$datein),c(4,5,12)]
write.csv(cldouble,"ppsq-homepn-multclreport.csv",row.names=F)

####################


cldup <- cl.dat[NULL,]
for (i in 1:length(demog.dat$mrn))
{
    this.dat <- cl.dat[cl.dat$cvc_mrn==demog.dat$mrn[i],]
    if (length(this.dat$insert_date) > length(unique(this.dat$insert_date))) cldup <- rbind(cldup,this.dat[duplicated(this.dat$insert_date) | duplicated(this.dat$insert_date,fromLast=T),])
}
print(cldup)

#

clflag <- rep(NA,length(demog.dat$mrn))
templist <- cl.dat[NULL,]

for (i in 1:length(demog.dat$mrn))
{
    thismrn <- demog.dat$mrn[i]
    this.dat <- cl.dat[cl.dat$cvc_mrn==thismrn,]
    numcl <- length(this.dat$cvc_mrn)
    if (numcl==0) next
    firstdate <- min(this.dat$datein)
    ndays <- today - firstdate + 1
    tempres <- matrix(0,numcl,ndays)

    for (j in 1:numcl) tempres[j,(today-this.dat$dateout[j]+1):(today-this.dat$datein[j]+1)] <- rep(1,this.dat$dateout[j]-this.dat$datein[j]+1)
    clflag[i] <- as.numeric(max(apply(tempres,2,sum,na.rm=T),na.rm=T)>1)
    if (clflag[i]==0) next
    whichcol <- which(apply(as.matrix(tempres),2,sum,na.rm=T) > 1)
    whichrow <- which(apply(as.matrix(tempres[,whichcol]),1,sum,na.rm=T) > 0)
    templist2 <- this.dat[whichrow,]
#    templist2 <- this.dat[!(duplicated(templist2$insert_date) | duplicated(templist2$insert_date,fromLast=T)),]
    templist2 <- templist2[templist2$dateout==today,]
    templist <- rbind(templist,templist2)
}
templist <- templist[,c(4:5,9,11:14,29)]
write.csv(templist[order(templist$cvc_mrn,templist$insert_date),],"ppsq-homepn-cloverlap.csv",row.names=F)

###########





#
