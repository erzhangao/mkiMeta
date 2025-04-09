library(ggplot2)
library(dplyr)
library(reshape2)
input.otu="otu.tab.txt"
input.group="group.txt"
top=10


data=read.table(input.otu,header = T,row.names = 1)
group=read.table(input.group,header = T)
colnames(group)=c("SampleID","Group")

data.sum=apply(data, 1, sum)
index=rownames(as.data.frame(data.sum[order(data.sum,decreasing = T)][1:top]))
data.top=data[index,]

groupname=unique(group$Group)
data.mean=matrix(NA,ncol = 4,nrow = length(rownames(data.top)),
                 dimnames = list(rownames(data.top),groupname))

for (g in groupname){
  a=data.top[,group %>% filter(Group==g) %>% pull(SampleID)]
  data.mean[,g]=apply(a, 1, mean)
}

data.mean=as.data.frame(data.mean)
data.mean$Type1=data.mean$Ga/data.mean$Ha
data.mean$logType1=log(data.mean$Type1)
data.mean$Type2=data.mean$Gb/data.mean$Hb
data.mean$logType2=log(data.mean$Type2)
data.mean$Sample=rownames(data.mean)
data.mean$trend=ifelse((data.mean$logType1)*(data.mean$logType2)<=0,"opposite trend","same trend")
data.mean$trend2=ifelse((data.mean$logType1)*(data.mean$logType2)<=0,"1","0")

data.plot=data.frame(Type1=data.mean$logType1,Type2=data.mean$logType2,
                     trend=data.mean$trend,feature=data.mean$Sample)

data.plot=data.plot[order(data.plot$Type1),]

datam=melt(data.plot)
datam$feature=factor(datam$feature,levels = data.plot$feature)
datam$trend=factor(datam$trend,levels = c("same trend" ,"opposite trend"))
ggplot(datam, aes(x = value, y = feature)) + 
  geom_point(stat = "identity",aes(colour=trend,shape=variable),size = 3) +
  scale_shape_manual(values =c(16,1))+
  geom_vline(xintercept=0)+
  labs(y = "", x = "Log(E/S)", title = "")+
  theme_bw()+
  theme(plot.margin = margin(1,0.5,0.5,0.5,'cm'),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        text = element_text(size = 16),
        axis.text.y=element_blank(),
        axis.ticks.y =element_blank(),
        axis.text.x=element_text(face="bold", size=12),
        legend.direction = "vertical")+
  scale_x_continuous(limits = c(-2.5,2.5))
ggsave("dot.plot.pdf")

###累计丰度

data=read.table(input.otu,header = T,row.names = 1)
group=read.table(input.group,header = T)
colnames(group)=c("SampleID","Group")

data.sum=apply(data, 1, sum)
index=rownames(as.data.frame(data.sum[order(data.sum,decreasing = T)][1:top]))
data.top=data[index,]

groupname=unique(group$Group)
data.mean=matrix(NA,ncol = 4,nrow = length(rownames(data.top)),
                 dimnames = list(rownames(data.top),groupname))

for (g in groupname){
  a=data.top[,group %>% filter(Group==g) %>% pull(SampleID)]
  data.mean[,g]=apply(a, 1, mean)
}

data.mean=as.data.frame(data.mean)
data.mean$Type1=data.mean$Ga/data.mean$Ha
data.mean$logType1=log(data.mean$Type1)
data.mean$Type2=data.mean$Gb/data.mean$Hb
data.mean$logType2=log(data.mean$Type2)
data.mean$Sample=rownames(data.mean)
data.mean$trend=ifelse((data.mean$logType1)*(data.mean$logType2)<=0,"opposite trend","same trend")
data.mean$trend2=ifelse((data.mean$logType1)*(data.mean$logType2)<=0,"1","0")

data.same=data[data.mean %>% filter(trend2==0) %>% pull(Sample),]
a=apply(data.same, 1, sum)
a=a[order(a)]
data.opposite=data[data.mean %>% filter(trend2==1) %>% pull(Sample),]
b=apply(data.opposite, 1, sum)
b=b[order(b)]
cul.tab=data.frame(OTU.order=c(1:5),Same=a,Opposite=b)
cul.tab=cul.tab %>% mutate(cumsumS=cumsum(a),cumsumO=cumsum(b))
cul.tab.tp1=cul.tab[,-(2:3)]
rownames(cul.tab.tp1)=cul.tab.tp1[,1]
cul.tab.plot.m=melt(cul.tab.tp1,measure=c("cumsumS","cumsumO"),id=1)

cul.tab.plot.m$variable<-factor(cul.tab.plot.m$variable,
                                levels = c('cumsumS','cumsumO'),
                                labels = c("A","B"))
ggplot(cul.tab.plot.m, aes(x = OTU.order, y = value)) + 
  geom_point(stat = "identity",aes(colour=variable,shape=variable),size = 3) +
  scale_shape_manual(values =c(1,1))+
  labs(y = "", x = "", title = "")+
  #theme_bw()+
  theme(plot.margin = margin(1,0.5,0.5,0.5,'cm'),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        text = element_text(size = 16),
        axis.text.y=element_blank(),
        axis.ticks.y =element_blank(),
        axis.text.x=element_text(face="bold", size=12),
        legend.direction = "vertical",
        legend.title = element_blank())