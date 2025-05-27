library(ggplot2)
library(ggpubr)

gen.dat<-function(filename,n,path){
setwd('/home/student/Documents/aall-articles/comparison-study-pca-spca-efa/Lp_rot1129-main/Elizabeth-Simulation/Rdata')
load(filename)
out=out[which(sapply(1:length(out),function(i,out){1-is.null(out[[i]]$vari)},out=out)==1)]
sim=length(out)

L_class=list()
L_class$irls1= rowMeans(sapply(1:sim,function(i,out){out[[i]]$irls1$L_class},out=out))
L_class$irls0.5= rowMeans(sapply(1:sim,function(i,out){out[[i]]$irls0.5$L_class},out=out))
L_old=L!=0
#p<-par(mfrow=c(1,2),oma=c(0,0,3,0))
names= c("oblimin","irls1","irls0.5", "lasso0.01", "lasso0.05","lasso0.1","lasso0.2","lasso0.5")
dat1=data.frame(class1=as.numeric(L_old),
              ECIC= L_class[[1]],p=1,n=n)
dat1$m=with(dat1,n+10*p+class1)
dat2=data.frame(class1=as.numeric(L_old),p=0.5,
              ECIC= L_class[[2]],n=n)
dat2$m=with(dat2,n+10*p+class1)

dat=rbind(dat1,dat2)
return(dat)}
# dat <- na.omit(dat)

# print(dim(dat))
# print(head(dat))

dat <- dat[!is.na(dat$ECIC), ]

pdf(file='15_3.pdf', width=15, height=6.5)
pdf(file='15_3.pdf',width=15, height = 6.5)
dat100=gen.dat("Lp.1121.V2.15N100.RData",100,path)
dat200=gen.dat("Lp.1121.V2.15N200.RData",200,path)
dat300=gen.dat("Lp.1121.V2.15N300.RData",300,path)
dat=rbind(dat100,dat200,dat300)
ggplot(dat, aes(x=as.factor(m), y=ECIC, color=as.factor(p))) +
  geom_boxplot() +
  labs(colour='p', y='ECIC', x='') +
  scale_x_discrete(labels=c('0 \n n=400','1 \n n=400','0 \n n=400','1 \n n=400',
                            '0 \n n=800','1 \n n=800','0 \n n=800','1 \n n=800',
                            '0 \n n=1600','1 \n n=1600','0 \n n=1600','1 \n n=1600')) +
  theme_minimal(base_size = 23) +
  theme(panel.grid.major = element_blank())

dev.off()

# pdf(file='15_3.pdf',width=15, height = 6.5)
# dat100=gen.dat("Lp.1121.V2.15N100.RData",100,path)
# dat200=gen.dat("Lp.1121.V2.15N200.RData",200,path)
# dat300=gen.dat("Lp.1121.V2.15N300.RData",300,path)
# dat=rbind(dat100,dat200,dat300)
# ggplot(dat, aes(x=as.factor(m),y=ECIC,color=as.factor(p))) + geom_boxplot()+labs(colour= 'p',
#            y='ECIC',x='')+ylim(0.925,0.975)+
# scale_x_discrete(labels=c('0 \n n=400','1 \n n=400','0 \n n=400','1 \n n=400','0 \n n=800','1 \n n=800','0 \n n=800','1 \n n=800','0 \n n=1600','1 \n n=1600','0 \n n=1600','1 \n n=1600')) +theme_set(theme_minimal())+theme(panel.grid.major=element_line(colour=NA))+theme(text = element_text(size = 23))

# dev.off()

# setwd(paste0(path,'/Rdata'))
# pdf(file='30_5.pdf',width=15, height = 6.5)
# dat400=gen.dat("Lp.1121.30N400.RData",400,path)
# dat800=gen.dat("Lp.1121.30N800.RData",800,path)
# dat1600=gen.dat("Lp.1121.30N1600.RData",1600,path)
# dat=rbind(dat400,dat800,dat1600)
# ggplot(dat, aes(x=as.factor(m),y=ECIC,color=as.factor(p))) + geom_boxplot()+labs(colour= 'p',
#            y='ECIC',x='')+ylim(0.925,0.975)+
# scale_x_discrete(labels=c('0 \n n=400','1 \n n=400','0 \n n=400','1 \n n=400','0 \n n=800','1 \n n=800','0 \n n=800','1 \n n=800','0 \n n=1600','1 \n n=1600','0 \n n=1600','1 \n n=1600')) +theme_set(theme_bw())+theme(panel.grid.major=element_line(colour=NA))+theme(text = element_text(size = 23))

# dev.off()