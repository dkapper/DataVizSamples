setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library("ggplot2")
library("readxl")
library("reshape2")
library("ggpubr")
library("dplyr")

rv <- read_excel("Exp2.xlsx", sheet = "raw")
rv$rk2 <- as.factor(rv$rk2)
#rv$rk22 <- 3-rv$rk22
rv$rk22 <- as.factor(rv$rk22)
lrv <- melt(rv,id.vars=c("cell","baseline","sdbaseline","rk2","rk22"),value.name="mph",variable.name="t")
lrv$t<-as.character(substr(lrv$t,start=4,stop=6))
lrv$t<-as.numeric(lrv$t)
lrv$t<-(lrv$t-3)/12
lrv$timeblock <- ifelse(lrv$t <= 2, "1", ifelse((lrv$t > 2) & (lrv$t <= 4), "2", ifelse((lrv$t > 4) & (lrv$t <= 6-1/12), "3","4")))
lrv$timeblock <- as.factor(lrv$timeblock)

nv1 <- read_excel("Exp2.xlsx", sheet = "normalized1")
nv1$nk2 <- as.factor(nv1$nk2)
lnv1 <- melt(nv1,id.vars=c("cell","baseline","sdbaseline","nk2"),value.name="mph",variable.name="t")
lnv1$t<-as.character(substr(lnv1$t,start=4,stop=6))
lnv1$t<-as.numeric(lnv1$t)
lnv1$t<-(lnv1$t-3)/12
lnv1$timeblock <- ifelse(lnv1$t <= 2, "1", ifelse((lnv1$t > 2) & (lnv1$t <= 4), "2", ifelse((lnv1$t > 4) & (lnv1$t <= 6-1/12), "3","4")))
lnv1$timeblock <- as.factor(lnv1$timeblock)

nv2 <- read_excel("Exp2.xlsx", sheet = "normalized2")
nv2$nk2 <- as.factor(nv2$nk2)
lnv2 <- melt(nv2,id.vars=c("cell","baseline","sdbaseline","nk2"),value.name="mph",variable.name="t")
lnv2$t<-as.character(substr(lnv2$t,start=4,stop=6))
lnv2$t<-as.numeric(lnv2$t)
lnv2$t<-(lnv2$t-3)/12
lnv2$timeblock <- ifelse(lnv2$t <= 2, "1", ifelse((lnv2$t > 2) & (lnv2$t <= 4), "2", ifelse((lnv2$t > 4) & (lnv2$t <= 6-1/12), "3","4")))
lnv2$timeblock <- as.factor(lnv2$timeblock)

llabs = rv %>%
  group_by (rk22) %>%
  summarise (n=n()) %>%
  mutate(pct = paste0("n = ",n," (",round(100 * n/sum(n), 1), "%)"))

fig8A <- ggplot()+
  stat_summary(data=lrv,mapping=aes(x=t,y=mph,color=rk22,group=rk22),fun.data=mean_cl_boot,geom="smooth",fun.args=list(B=10000,conf.int=0.95))+
  geom_vline(xintercept=2,colour="orange",linetype="dashed")+geom_vline(xintercept=4,colour="orange",linetype="dashed")+
  geom_vline(xintercept=5.9166666667,colour="orange",linetype="dashed")+
  geom_text(mapping=aes(x=2, y=55, label=as.character(expression(paste("50 ",mu,"M NaN")))),parse=T, angle=90, vjust=-0.4, hjust=0.5)+
  geom_text(mapping=aes(x=4, y=55, label=as.character(expression(paste("100 ",mu,"M NaN")))),parse=T, angle=90, vjust=-0.4, hjust=0.5)+
  geom_text(mapping=aes(x=5.916666666667, y=55, label="12.5 mM NaN"), angle=90, vjust=-0.4,hjust=0.5)+
  scale_color_manual(labels = llabs$pct, values=c("cornflowerblue","darkred")) +
  ggtitle("")+xlab(label="Time (hours)")+ylab( expression(paste("Absolute Velocity (", mu,"m/hr)")))+theme_bw()+ theme(legend.position="none")+
  theme(axis.text.x = element_text(size=14),axis.text.y = element_text(size=14),legend.text=element_text(size=14),
        axis.text=element_text(size=14),legend.position=c(0.5,0),legend.justification=c(0.5,0),legend.title = element_blank(),
        legend.background = element_rect(color = "black",fill = "grey95", linetype = "solid"),legend.direction = "vertical")+
  coord_cartesian(ylim=c(0,60))

llabs = nv2 %>%
  group_by (nk2) %>%
  summarise (n=n()) %>%
  mutate(pct = paste0("n = ",n," (",round(100 * n/sum(n), 1), "%)"))

fig8B <- ggplot()+geom_rect(data=data.frame(xmin=1,xmax=2,ymin=-Inf,ymax=Inf),
                            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="darkgreen",alpha=0.3)+
  stat_summary(data=lnv2,mapping=aes(x=t,y=mph*sdbaseline+baseline,color=nk2,group=nk2),
               fun.data=mean_cl_boot,geom="smooth",fun.args=list(B=10000,conf.int=0.95))+
  geom_vline(xintercept=2,colour="orange",linetype="dashed")+geom_vline(xintercept=4,colour="orange",linetype="dashed")+
  geom_vline(xintercept=5.9166666667,colour="orange",linetype="dashed")+
  geom_text(mapping=aes(x=2, y=55, label=as.character(expression(paste("50 ",mu,"M NaN")))),parse=T, angle=90, vjust=-0.4, hjust=0.5)+
  geom_text(mapping=aes(x=4, y=55, label=as.character(expression(paste("100 ",mu,"M NaN")))),parse=T, angle=90, vjust=-0.4, hjust=0.5)+
  geom_text(mapping=aes(x=5.916666666667, y=55, label="12.5 mM NaN"), angle=90, vjust=-0.4,hjust=0.5)+
  scale_color_manual(labels = llabs$pct, values=c("cornflowerblue","darkred")) +
  ggtitle("")+xlab(label="Time (hours)")+ylab( expression(paste("Absolute Velocity (", mu,"m/hr)")))+theme_bw()+ theme(legend.position="none")+
  theme(axis.text.x = element_text(size=14),axis.text.y = element_text(size=14),legend.text=element_text(size=14),
        axis.text=element_text(size=14),legend.position=c(0.5,0),legend.justification=c(0.5,0),legend.title = element_blank(),
        legend.background = element_rect(color = "black",fill = "grey95", linetype = "solid"),legend.direction = "vertical")+
  coord_cartesian(ylim=c(0,60))

llabs = nv1 %>%
  group_by (nk2) %>%
  summarise (n=n()) %>%
  mutate(pct = paste0("n = ",n," (",round(100 * n/sum(n), 1), "%)"))

fig8F <- ggplot()+geom_rect(data=data.frame(xmin=0,xmax=1,ymin=-Inf,ymax=Inf),
                            aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),fill="darkgreen",alpha=0.3)+
  stat_summary(data=lnv1,mapping=aes(x=t,y=mph*sdbaseline+baseline,color=nk2,group=nk2),
               fun.data=mean_cl_boot,geom="smooth",fun.args=list(B=10000,conf.int=0.95))+
  geom_vline(xintercept=2,colour="orange",linetype="dashed")+geom_vline(xintercept=4,colour="orange",linetype="dashed")+
  geom_vline(xintercept=5.9166666667,colour="orange",linetype="dashed")+
  geom_text(mapping=aes(x=2, y=55, label=as.character(expression(paste("50 ",mu,"M NaN")))),parse=T, angle=90, vjust=-0.4, hjust=0.5)+
  geom_text(mapping=aes(x=4, y=55, label=as.character(expression(paste("100 ",mu,"M NaN")))),parse=T, angle=90, vjust=-0.4, hjust=0.5)+
  geom_text(mapping=aes(x=5.916666666667, y=55, label="12.5 mM NaN"), angle=90, vjust=-0.4,hjust=0.5)+
  scale_color_manual(labels = llabs$pct, values=c("cornflowerblue","darkred")) +
  ggtitle("")+xlab(label="Time (hours)")+ylab( expression(paste("Absolute Velocity (", mu,"m/hr)")))+theme_bw()+ theme(legend.position="none")+
  theme(axis.text.x = element_text(size=14),axis.text.y = element_text(size=14),legend.text=element_text(size=14),
        axis.text=element_text(size=14),legend.position=c(0.5,0),legend.justification=c(0.5,0),legend.title = element_blank(),
        legend.background = element_rect(color = "black",fill = "grey95", linetype = "solid"),legend.direction = "vertical")+
  coord_cartesian(ylim=c(0,60))

figure8 <- ggarrange(fig8B,fig8F,fig8A,nrow=1,ncol=3,labels=c("A","B","C"))
ggsave("figure8.pdf",figure8,width=11,height=5,units="in")
