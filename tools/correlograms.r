library("pgirmess")
#RESIDUAL Y
data=read.csv('../srcdata/distortion_measures/bati_vs_grid.csv')
datam=data$YRESIDUAL
correlog=correlog(cbind(data$X,data$Y), datam)
max(dist(cbind(data$X,data$Y)))

data2=read.csv('../srcdata/distortion_measures/rues_vs_grid.csv')
datam2=data2$YRESIDUAL
correlog2=correlog(cbind(data2$X,data2$Y), datam2)

max(dist(cbind(data2$X,data2$Y)))

mat1=as.data.frame(correlog)
mat2=as.data.frame(correlog2)

#Résidus Y
plot(mat1$dist.class,mat1$coef, type="n", xlab="Distance (meters)" ,ylab="Morans'I value", xlim=c(0,8000), ylim=c(-0.7,0.7))
abline(h = -1:5, col = "black", lty = 3)
abline(v = 1000, col = rgb(1,0.2,0.2,1), lty = 1, lwd=4)
abline(v = 2000, col = rgb(1,0.5,0.5,1), lty = 1, lwd=4)
abline(v = 4000, col = rgb(1,0.8,0.8,1), lty = 1, lwd=4)
lines(mat1$dist.class,mat1$coef, col="chartreuse4") #Points du bati
points(mat1, col = ifelse(mat1$p.value<0.05,'red','chartreuse'), pch = 19 )
lines(mat2$dist.class,mat2$coef, col="darkgoldenrod4") # Points des rues
points(mat2, col = ifelse(mat2$p.value<0.05,'red','darkgoldenrod'), pch = 19 )


#RESIDUAL X
data=read.csv('../srcdata/distortion_measures/bati_vs_grid.csv')
datam=data$XRESIDUAL
correlog=correlog(cbind(data$X,data$Y), datam)


data2=read.csv('../srcdata/distortion_measures/rues_vs_grid.csv')
datam2=data2$XRESIDUAL
correlog2=correlog(cbind(data2$X,data2$Y), datam2)

mat1=as.data.frame(correlog)
mat2=as.data.frame(correlog2)

plot(mat1$dist.class,mat1$coef, type="n", xlab="Distance (meters)" ,ylab="Morans'I value", xlim=c(0,8000), ylim=c(-0.7,0.7))
abline(h = -1:5, col = "black", lty = 3)
abline(v = 1000, col = rgb(1,0.2,0.2,1), lty = 1, lwd=4)
abline(v = 2000, col = rgb(1,0.5,0.5,1), lty = 1, lwd=4)
abline(v = 4000, col = rgb(1,0.8,0.8,1), lty = 1, lwd=4)
lines(mat1$dist.class,mat1$coef, col="chartreuse4")
points(mat1, col = ifelse(mat$p.value<0.05,'red','chartreuse'), pch = 19 )

lines(mat2$dist.class,mat2$coef, col="darkgoldenrod4")
points(mat2, col = ifelse(mat2$p.value<0.05,'red','darkgoldenrod'), pch = 19 )


#ORIENTATION 
data=read.csv('../srcdata/distortion_measures/bati_vs_grid.csv')
datam=data$ORI
correlog=correlog(cbind(data$X,data$Y), datam)


data2=read.csv('../srcdata/distortion_measures/rues_vs_grid.csv')
datam2=data2$ORI
correlog2=correlog(cbind(data2$X,data2$Y), datam2)

mat1=as.data.frame(correlog)
mat2=as.data.frame(correlog2)

plot(mat1$dist.class,mat1$coef, type="n", xlab="Distance (meters)" ,ylab="Morans'I value", xlim=range(1:8000),ylim=range(-1:1))
abline(h = -1:5, col = "black", lty = 3)
abline(v = 1000, col = rgb(1,0.2,0.2,1), lty = 1, lwd=4)
abline(v = 2000, col = rgb(1,0.5,0.5,1), lty = 1, lwd=4)
abline(v = 4000, col = rgb(1,0.8,0.8,1), lty = 1, lwd=4)
lines(mat1$dist.class,mat1$coef, col="chartreuse4")
points(mat1, col = ifelse(mat$p.value<0.05,'red','chartreuse'), pch = 19 )
lines(mat2$dist.class,mat2$coef, col="darkgoldenrod4")
points(mat2, col = ifelse(mat2$p.value<0.05,'red','darkgoldenrod'), pch = 19 )
