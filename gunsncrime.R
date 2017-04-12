#clear 
cat("\014")  

x=gun_crime_data
attach(x)
x[is.na(x)]<-0
View(x)
xx<-cbind(pop,ampct,metpct,unrate,prison,totemployed,employ,military,rpci,domestic_gun_production,gallup_owngun,pgs,totmajor,murder,rape,robbery,assault,burg_test,burglary,hg,gun,majorcrime,mc_noburg,gunsamm,amrmms,amhms,phg,pgun,mc_noassault,r1524,r2544,r4564)
y<-prcomp(x,scale=TRUE)
y
summary(y)
#str(y)
#head(y$scores)
yy<-princomp(x,scores = TRUE,cor = TRUE)
summary(yy)

#plots

#pie

slices <- c(10, 12,5, 16, 8)
lbls <- c( "gun", "unrate", "phg", "military", "hg")
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/pie.jpeg",width=500,height=500)
pie(slices, labels = lbls, main="Pie Chart of Guns and Crime")
dev.off()

#dot

jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/dot.jpeg",width=500,height=500)
dotchart(xx,labels=row.names(xx2),cex=.7,main="Guns")
dev.off()

#scree
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/scree_prcomp.jpeg",width=500,height=500)
plot(y,type="lines")
dev.off()
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/scree_princomp.jpeg",width=500,height=500)
plot(yy,type="lines")
dev.off()

#bi 

jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/bi_prcomp.jpeg",width=500,height=500)
biplot(y)
dev.off() 

jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/bi_princomp.jpeg",width=500,height=500)
biplot(yy)
dev.off() 

r=(r1524+r2544+r4564)/3


#Decision Trees

d<-ctree(majorcrime~military,data=x)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/gdt1.jpeg",width=500,height=500)
plot(d)
dev.off()

d1<-ctree(majorcrime~military+unrate,data=x)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/gdt3.jpeg",width=500,height=500)
plot(d1)
dev.off()

d4<-ctree(majorcrime~military+unrate+gun,data=x)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/gdt5.jpeg",width=500,height=500)
plot(d4)
dev.off()

d3<-ctree(majorcrime~military+unrate+gun+hg,data=x)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/gdt4.jpeg",width=500,height=500)
plot(d3)
dev.off()

d5<-ctree(majorcrime~military+unrate+gun+hg+phg,data=x)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/gdt6.jpeg",width=500,height=500)
plot(d5)
dev.off()

