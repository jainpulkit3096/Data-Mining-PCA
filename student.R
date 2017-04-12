#clear 
cat("\014")

x2=student.mat
Grades<-G1+G2+G3/3
xx2<-cbind(Medu,Fedu,traveltime,studytime,failures,famrel,freetime,goout,Dalc,Walc,health,absences,Grades)
attach(x2)
y3<-prcomp(xx2,scale=TRUE)
summary(y3)
yy3<-princomp(xx2,scores = TRUE,cor = TRUE)
summary(yy3)
#plots
#pie

slices <- c(10, 8, 15, 11, 5)
lbls <- c( "reprate", "censor4", "tenure", "censor1", "disrate")
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/pie.jpeg",width=500,height=500)
pie(slices, labels = lbls, main="Pie Chart of Guns and Crime")
dev.off()

#dot

jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/dot.jpeg",width=500,height=500)
dotchart(xx2,labels=row.names(xx2),cex=.7,main="Student")
dev.off()

#histogram

jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/histogram.jpeg",width=500,height=500)
hist(xx2)
dev.off()

#scree

jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/scree_prcomp.jpeg",width=500,height=500)
plot(y3,type="lines")
dev.off()

jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/scree_princomp.jpeg",width=500,height=500)
plot(yy3,type="lines")
dev.off()

#bi

jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/bi_prcomp.jpeg",width=500,height=500)
biplot(y3)
dev.off()

jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/bi_princomp.jpeg",width=500,height=500)
biplot(yy3)
dev.off()

#density
xxx2<-density(xx2)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/kernel_density.jpeg",width=500,height=500)
plot(xxx2)
dev.off()

#decision trees

sd1 <-ctree(Grades ~ Walc + goout + famrel + health + failures, data = x2)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/sdt1.jpeg",width=500,height=500)
plot(sd1)
dev.off()

sd2 <-ctree(Grades ~ Walc + goout + famrel + health, data = x2)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/sdt2.jpeg",width=500,height=500)
plot(sd2)
dev.off()

sd3 <-ctree(Grades ~ Walc + goout + famrel, data = x2)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/sdt3.jpeg",width=500,height=500)
plot(sd3)
dev.off()

sd4 <-ctree(Grades ~ Walc + goout, data = x2)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/sdt4.jpeg",width=500,height=500)
plot(sd4)
dev.off()

sd5 <-ctree(Grades ~ Walc , data = x2)
jpeg(filename = "C:/Users/VINOD JAIN/Documents/final/sdt5.jpeg",width=500,height=500)
plot(sd5)
dev.off()