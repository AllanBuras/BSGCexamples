#######code used for the reply to Jetschke et al., 2023:

###load dendRolAB

library(dendRolAB)

###prepare Fig. 1:

###generate a tree-ring curve that features realistic growth patterns

set.seed(1)###R version 4.2.2 --> other versions might produce different data while the general results will be similar

PseudoSignal<-rnorm(100,1.5,0.3)###100 years with mean RW of 1.5 mm and sd of 0.3

PseudoSignal[2:100]<-PseudoSignal[2:100]+PseudoSignal[1:99]-mean(PseudoSignal[1:99])###add some autocorrelation

acf(PseudoSignal)###check autocorrelation

###now add 10 random, white-noises that are not correlated with the Signal

PseudoPopulation<-as.data.frame(matrix(nrow=100,ncol=10))

rownames(PseudoPopulation)<-1901:2000

for(i in 1:10)
{
  NOISE<-rnorm(100,0,0.3)
  MOD.NOIS.RES<-lm(NOISE~PseudoSignal)$resid###use model residuals since they are not correlated
  PseudoPopulation[,i]<-PseudoSignal+NOISE
}

###assess rbar

rwi.stats(PseudoPopulation)###reasonable rbar of 0.59

###now detrend since this is mandatory for bsgc

D.PseudoPopulation<-detrend(PseudoPopulation,method="Spline",nyrs = 30)

tiff(height=2000,width=3000,res=300,compression="lzw",file="D:\\WORK\\Projects\\PointerYears\\Manuscript4\\Figs\\Fig.1a.tif")

BSGC.Pseudo<-bsgc(D.PseudoPopulation,ylab="RWI",xlab="year",cex.lab=1.5,cex.axis=1.5,rm.succ=F)###apply bsgc
mtext(side=3,at=1900,text="(a)",cex=1.5)
points(x=c(1915,1946,1956,1995),y=apply(D.PseudoPopulation[c("1915","1946","1956","1995"),],1,mean),
       lwd=2,cex=3)


dev.off()

###and now look at some years which would be SGC pointer-years but not BSGC to exemplify GSGC

which(BSGC.Pseudo$pvals.sgc[,1]<0.025)###1914, 1924, 1958, and 1972

which(BSGC.Pseudo$pvals.gsgc[,1]<0.025)###1914

which(BSGC.Pseudo$pvals.sgc[,1]>0.975)###1915, however not declaviolet a pointer year

which(BSGC.Pseudo$pvals.gsgc[,1]>0.975)###1962 and 1971

BSGC.Pseudo$neg
BSGC.Pseudo$pos

###1946, 1956, and 1995 were only defined as pointer years due to conflation
###1915 not declared a pointer year due to conflation

###let's assess all of those one after another

###1915:

BSGC.Pseudo$pvals.sgc[c("1915"),1]###significant growth change
BSGC.Pseudo$pvals.gsgc[c("1915"),1]###non-significant global growth change

Zscores1915<-qnorm(c(BSGC.Pseudo$pvals.sgc[c("1915"),1],
                     BSGC.Pseudo$pvals.gsgc[c("1915"),1],
                     BSGC.Pseudo$pvals.conf[c("1915"),1]))


###1946:

BSGC.Pseudo$pvals.sgc[c("1946"),1]###significant growth change
BSGC.Pseudo$pvals.gsgc[c("1946"),1]###non-significant global growth change

Zscores1946<-qnorm(c(BSGC.Pseudo$pvals.sgc[c("1946"),1],
                     BSGC.Pseudo$pvals.gsgc[c("1946"),1],
                     BSGC.Pseudo$pvals.conf[c("1946"),1]))



###1956:

BSGC.Pseudo$pvals.sgc[c("1956"),1]###significant growth change
BSGC.Pseudo$pvals.gsgc[c("1956"),1]###non-significant global growth change

Zscores1956<-qnorm(c(BSGC.Pseudo$pvals.sgc[c("1956"),1],
                     BSGC.Pseudo$pvals.gsgc[c("1956"),1],
                     BSGC.Pseudo$pvals.conf[c("1956"),1]))



###1995:

BSGC.Pseudo$pvals.sgc[c("1995"),1]###significant growth change
BSGC.Pseudo$pvals.gsgc[c("1995"),1]###non-significant global growth change

Zscores1995<-qnorm(c(BSGC.Pseudo$pvals.sgc[c("1995"),1],
                     BSGC.Pseudo$pvals.gsgc[c("1995"),1],
                     BSGC.Pseudo$pvals.conf[c("1995"),1]))



tiff(height=2000,width=3000,res=300,compression="lzw",file="D:\\WORK\\Projects\\PointerYears\\Manuscript4\\Figs\\Fig.1b_e.tif")


par(mfrow=c(2,2))

plot(dnorm(seq(-3,3,0.01))~seq(-3,3,0.01),type="l",
     xlab="z-score",ylab="density",cex.lab=1.5,cex.axis=1.5,lwd=2,main="1915: no PY",cex.main=1.5)
abline(h=0,lwd=2)
lines(x=rep(-1.96,2),y=c(0,dnorm(-1.96)),lwd=2)
lines(x=rep(1.96,2),y=c(0,dnorm(1.96)),lwd=2)
points(Zscores1915,dnorm(Zscores1915),pch=16,cex=c(2,2,3),col=c("orange","dodgerblue","violet"))
legend(x=-3,y=0.4,legend=c("SGC","GSGC","BSGC"),fill=c("orange","dodgerblue","violet"),cex=1.25)
mtext(side=3,at=-3,text="(b)",cex=1.5)

plot(dnorm(seq(-3,3,0.01))~seq(-3,3,0.01),type="l",
     xlab="z-score",ylab="density",cex.lab=1.5,cex.axis=1.5,lwd=2,main="1946: neg PY",cex.main=1.5)
abline(h=0,lwd=2)
lines(x=rep(-1.96,2),y=c(0,dnorm(-1.96)),lwd=2)
lines(x=rep(1.96,2),y=c(0,dnorm(1.96)),lwd=2)
points(Zscores1946,dnorm(Zscores1946),pch=16,cex=c(2,2,3),col=c("orange","dodgerblue","violet"))
legend(x=-3,y=0.4,legend=c("SGC","GSGC","BSGC"),fill=c("orange","dodgerblue","violet"),cex=1.25)
mtext(side=3,at=-3,text="(c)",cex=1.5)

plot(dnorm(seq(-3,3,0.01))~seq(-3,3,0.01),type="l",
     xlab="z-score",ylab="density",cex.lab=1.5,cex.axis=1.5,lwd=2,main="1956: pos PY",cex.main=1.5)
abline(h=0,lwd=2)
lines(x=rep(-1.96,2),y=c(0,dnorm(-1.96)),lwd=2)
lines(x=rep(1.96,2),y=c(0,dnorm(1.96)),lwd=2)
points(Zscores1956,dnorm(Zscores1956),pch=16,cex=c(2,2,3),col=c("orange","dodgerblue","violet"))
legend(x=-3,y=0.4,legend=c("SGC","GSGC","BSGC"),fill=c("orange","dodgerblue","violet"),cex=1.25)
mtext(side=3,at=-3,text="(d)",cex=1.5)

plot(dnorm(seq(-3,3,0.01))~seq(-3,3,0.01),type="l",
     xlab="z-score",ylab="density",cex.lab=1.5,cex.axis=1.5,lwd=2,main="1995: pos PY",cex.main=1.5)
abline(h=0,lwd=2)
lines(x=rep(-1.96,2),y=c(0,dnorm(-1.96)),lwd=2)
lines(x=rep(1.96,2),y=c(0,dnorm(1.96)),lwd=2)
points(Zscores1995,dnorm(Zscores1995),pch=16,cex=c(2,2,3),col=c("orange","dodgerblue","violet"))
legend(x=-3,y=0.4,legend=c("SGC","GSGC","BSGC"),fill=c("orange","dodgerblue","violet"),cex=1.25)
mtext(side=3,at=-3,text="(e)",cex=1.5)

dev.off()




###reproduce pseudo-population as per Jetschke et al. (2023)

###step 1: generate pseudo-population

set.seed(123)###R version 4.2.2 --> other versions might produce different data while the general results will be similar

SIGNAL<-rep(1,125)

SIGNAL[20:39]<-0.75
SIGNAL[60:65]<-seq(0.75,1,0.05)
SIGNAL[81]<-0.75###in Jetschke et al. 2023 they stated this to be at 80 but Figs. 2 and 3 in their paper rather suggests it's 81
SIGNAL[c(101,103)]<-0.75

plot(SIGNAL,type="l")

RW.SIGNAL<-matrix(nrow=125,ncol=20)

RW.SIGNAL[,1:20]<-SIGNAL

for(i in 1:125)
{
  RW.SIGNAL[i,]<-RW.SIGNAL[i,]+rnorm(20,0,0.05)
}


plot(RW.SIGNAL[,1],type="l",col="grey",ylim=c(0.5,1.5))
for(i in 2:20)
{
  lines(RW.SIGNAL[,i],col="grey")
}

rownames(RW.SIGNAL)<-1:125

tiff(height=4000,width=4000,res=300,compression="lzw",file="D:\\WORK\\Projects\\PointerYears\\Manuscript4\\Figs\\Fig.2.tif")

par(mfrow=c(2,1))

TEST.SGC<-bsgc(RW.SIGNAL,w1=0,w2=1,rm.succ=F,maxlag=0,xlab="",ylab="RWI",
               cex.lab=1.5,cex.axis=1.5,main="SGC")
mtext(side=3,at=0,text="(a)",cex=1.5)
abline(v=seq(0,120,10),lty=3)

TEST.BSGC<-bsgc(RW.SIGNAL,rm.succ = T,xlab="",ylab="RWI",
                cex.lab=1.5,cex.axis=1.5,main="BSGC")
mtext(side=3,at=0,text="(b)",cex=1.5)
abline(v=seq(0,120,10),lty=3)

dev.off()


###visualize the bimodal distribution of GC and RW in Fig. 3


MASTER<-chron(RW.SIGNAL)[,1]

pnorm(scale(MASTER))

H.ABS<-hist(MASTER,breaks=50)
H.DIFF<-hist(diff(MASTER),breaks=50)


tiff(height=2000,width=4000,res=300,compression="lzw",file="D:\\WORK\\Projects\\PointerYears\\Manuscript4\\Figs\\Fig.3.tif")


par(mfrow=c(1,2))

barplot(H.ABS$counts/sum(H.ABS$count)*100,space=0,xlab="",ylab="percentage [%]",
        cex.lab=1.5,cex.axis=1.5)
axis(side=1,at=seq(0.5,55.5,5),labels=round(H.ABS$mids[seq(1,56,5)],digits=2),las=2,cex.axis=1.5)
mtext(side=1,line=4,text="RW [mm]",cex=1.5)
mtext(side=3,at=0,text="(a)",cex=1.5)


barplot(H.DIFF$counts/sum(H.DIFF$count)*100,space=0,xlab="",ylab="percentage [%]",
        cex.lab=1.5,cex.axis=1.5)
axis(side=1,at=seq(0.5,53.5,5),labels=round(H.DIFF$mids[seq(1,53,5)],digits=2),las=2,cex.axis=1.5)
mtext(side=1,line=4,text="GC [mm]",cex=1.5)
mtext(side=3,at=0,text="(b)",cex=1.5)

dev.off()

###compute the multiple of the growth change at time-step 19 in comparison to all normal growth changes

GC<-diff(MASTER)

(GC[19]/mean(GC[-c(19,39,59,80,100,102)]))###factor 30.2

###or the deviance in units of standard deviations

(GC[19]-mean(GC[-c(19,39,59,80,100,102)]))/sd(GC[-c(19,39,59,80,100,102)])###-6.29, thus extreme outlier








