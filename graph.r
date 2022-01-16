library(TSSS)
data <- read.csv("nikkei_recent.csv",header=T)

init_value <- data$終値
init_value <- init_value[!is.na(init_value)]
value <- diff(log(init_value))

par(mfrow=c(4,1)) 
par(mgp=c(1.3, 0.5, 0))
par(mar = c(2.5, 3, 1, 1)) # グラフの周囲の余白の広さを行数で指定．下，左，上，右の順．
# 時変分散の推定
z <- tvvar( value, trend.order = 2, tau2.ini = NULL )
# plot( exp( z$trend[,2] ),type="l" )
# ???ガウス型平滑化
s3 <- ngsmth( z$sm, noisev = 1, tau2 = z$tau2, noisew = 4, sigma2 = pi*pi/2, k = 190)
tt <- s3$trend

plot(init_value,type="l")
# 結果表示
plot( z$sm,type="l" )
plot( tt[,4],type="l",lwd=2 )
for (j in 1:7){
  lines( tt[,j],type="l" )
}
plot( exp(tt[,4]/2),type="l" )


