x3=Fatality
View(x3)
attach(x3)
xx3<-cbind(X,state,year,mrall,beertax,mlda,vmiles,unrate,perinc)
y2<-prcomp(xx3,scale=TRUE)
summary(y2)
yy2<-princomp(xx3,scores = TRUE,cor = TRUE)
summary(yy2)
#unrate,beertax,perinc,vmiles,mrall

#plots

#density
xxx3<-density(xx3)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/kernel_density_plot.jpeg",width=500,height=500)
plot(xxx3)
dev.off()

#histogram
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/histogram.jpeg",width=500,height=500)
hist(xx3)
dev.off()

#pie
slices <- c(5,16,12, 9, 7)
lbls <- c( "mrall", "unrate", "beertax", "perinc", "vmiles")
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/pie.jpeg",width=500,height=500)
pie(slices, labels = lbls, main="Pie Chart of Fatality")
dev.off()

#dot

jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/dot.jpeg",width=500,height=500)
dotchart(xx3,labels=row.names(xx3),cex=.9,main="Fatality")
dev.off()

#scree
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/scree_prcomp.jpeg",width=500,height=500)
plot(y2,type="lines")
dev.off()
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/scree_princomp.jpeg",width=500,height=500)
plot(yy2,type="lines")
dev.off()

#bi 

jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/bi_prcomp.jpeg",width=500,height=500)
biplot(y2)
dev.off()

#decision trees

fd1 <-ctree(year ~ unrate + beertax + perinc + vmiles + mrall, data = x3)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/fdt1.jpeg",width=500,height=500)
plot(fd1)
dev.off()

fd2 <-ctree(year ~ unrate + beertax + perinc + vmiles , data = x3)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/fdt2.jpeg",width=500,height=500)
plot(fd2)
dev.off()

fd3 <-ctree(year ~ unrate + beertax + perinc, data = x3)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/fdt3.jpeg",width=500,height=500)
plot(fd3)
dev.off()

fd4 <-ctree(year ~ unrate + beertax, data = x3)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/fdt4.jpeg",width=500,height=500)
plot(fd4)
dev.off()

fd5 <-ctree(year ~ unrate, data = x3)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/fdt5.jpeg",width=500,height=500)
plot(fd5)
dev.off()