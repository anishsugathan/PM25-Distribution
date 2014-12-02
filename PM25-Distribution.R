# Histogram from city-level WHO PM2.5 pollution 
# From project : Spring/2014/EPW Paper
# Author: Anish Sugathan | Email: anish.iimb@gmail.com
#

require("ggplot2")
require("gridExtra")
library('reshape')

CountryCity<-read.csv('WHO_PM25_Data.csv')
CountryCity$Country<-factor(CountryCity$Country)
#plot country distributions
fill_colours<-c("#00E400","#FFFF00","#FF7E00","#FF0000","#99004C","#8F1134")
hist_border_col<-"#252525"
hist_fill_col<-"#969696"
p1<-ggplot(subset(CountryCity,Country=='India'),aes(x=PM25))
p1<-p1+geom_histogram(aes(fill="f1",colour="c1",alpha=1),binwidth=5)+scale_color_manual(values=c("c1"=hist_border_col))+scale_fill_manual(values=c("f1"=hist_fill_col))#+coord_flip()
p1<-p1+xlim(c(0,175))
p1<-p1+theme_classic()+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.x=element_blank(),axis.line.x=element_blank()
                             ,axis.title.y=element_blank() ,legend.position="none",plot.margin=unit(c(0,0,0,1.35),"lines")) 
p1<-p1+annotate("text",label=c("India\n(124 Cities)"),x=c(150),y=c(15),angle=0,vjust=1,hjust=0.5,size=5,face="bold")
p1<-p1+geom_vline(xintercept=c(10,40),linetype=c("dashed","solid"),size=1) 
p1<-p1+annotate("text",label=c("WHO"),x=c(8),y=c(16),angle=0,vjust=0,hjust=1,size=5,face="bold")
p1<-p1+annotate("text",label=c("NAAQS"),x=c(42),y=c(16),angle=0,vjust=0,hjust=0,size=5,face="bold")

p2<-ggplot(subset(CountryCity,Country=='China' ),aes(x=PM25))
p2<-p2+geom_histogram(aes(fill="f1",colour="c1",alpha=1),binwidth=5)+scale_color_manual(values=c("c1"=hist_border_col))+scale_fill_manual(values=c("f1"=hist_fill_col))#+coord_flip()
p2<-p2+xlim(c(0,175))
p2<-p2+theme_classic()+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.x=element_blank(),axis.line.x=element_blank()
                             ,axis.title.y=element_blank() ,legend.position="none",plot.margin=unit(c(0,0,0,1.35),"lines")) 
p2<-p2+annotate("text",label=c("China\n(112 Cities)"),x=c(150),y=c(30),angle=0,vjust=1,hjust=0.5,size=5,face="bold")
p2<-p2+geom_vline(xintercept=c(10,35),linetype=c("dashed","solid"),size=1) 

p3<-ggplot(subset(CountryCity,Country=='USA'),aes(x=PM25))
p3<-p3+geom_histogram(aes(fill="f1",colour="c1",alpha=1),binwidth=5)+scale_color_manual(values=c("c1"=hist_border_col))+scale_fill_manual(values=c("f1"=hist_fill_col))#+coord_flip()
p3<-p3+xlim(c(0,175))+xlab(label=expression("Annual Average PM2.5 ("*mu~"g/m3)"))
p3<-p3+theme_classic()+theme(axis.title.y=element_blank(),axis.title.x=element_text(size=14),axis.text.x=element_text(size=14),legend.position="none",plot.margin=unit(c(0,0,0,1),"lines"))
p3<-p3+annotate("text",label=c("USA\n(379 Cities)"),x=c(150),y=c(200),angle=0,vjust=1,hjust=0.5,size=5,face="bold")
p3<-p3+geom_vline(xintercept=c(10,12),linetype=c("dashed","solid"),size=1) 

p4<-ggplot(subset(CountryCity,Country=='Europe'),aes(x=PM25))
p4<-p4+geom_histogram(aes(fill="f1",colour="c1",alpha=1),binwidth=5)+scale_color_manual(values=c("c1"=hist_border_col))+scale_fill_manual(values=c("f1"=hist_fill_col))#+coord_flip()
p4<-p4+xlim(c(0,175))
p4<-p4+theme_classic()+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.x=element_blank(),axis.line.x=element_blank()
                             ,axis.title.y=element_blank() ,legend.position="none",plot.margin=unit(c(0,0,0,1),"lines")) 
p4<-p4+annotate("text",label=c("Europe\n(565 Cities)"),x=c(150),y=c(150),angle=0,vjust=1,hjust=0.5,size=5,face="bold")
p4<-p4+geom_vline(xintercept=c(10,25),linetype=c("dashed","solid"),size=1) 

#save graphic
png("CountryDistr.png",height=10,width=10,units="in",res=300)
grid.arrange(p1,p2,p4,p3,ncol=1,nrow=4,heights=c(6,6,6,6),widths=c(6,6,6,6))
dev.off()
