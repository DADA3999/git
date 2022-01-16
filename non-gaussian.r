library(TSSS)
data <- read.csv("nikkei_recent.csv",header=T)

init_value <- data$�I�l
init_value <- init_value[!is.na(init_value)]
value <- diff(log(init_value))

par(mfrow=c(1,1)) 
par(mgp=c(1.3, 0.5, 0))
par(mar = c(2.5, 3, 1, 1)) # �O���t�̎��̗͂]���̍L�����s���Ŏw��D���C���C��C�E�̏��D

z <- tvvar( value, trend.order = 2, tau2.ini = NULL )
# �V�X�e���m�C�Y: ���K���z, �ϑ��m�C�Y�F2�d�w�����z
# s1 <- ngsmth( z$sm, noisev = 1, tau2 = z$tau2, noisew = 4, sigma2 = pi*pi/2, k = 190)
# �V�X�e���m�C�Y: ���K���z�C�ϑ��m�C�Y�F���K���z(�J���}���t�B���^�Ɠ���)
# s2 <- ngsmth( z$sm, noisev = 1, tau2 = z$tau2, noisew = 2, sigma2 = pi*pi/6, k = 190 )
# �V�X�e���m�C�Y�F�R�[�V�[���z�C�ϑ��m�C�Y�F2�d�w�����z
# ���ꂪAIC�ŏ�
s3 <- ngsmth( z$sm, noisev = 2, tau2 = z$tau2, bv = 1.0,noisew = 1,sigma2 = z$sigma2 )
# ���㕪�z��3�����v���b�g
plot(s3, "smt", theta = 25, phi = 30, expand = 0.25, col = "white")

# tt <- s3$trend
# 
# plot(init_value,type="l")
# # ���ʕ\��
# plot( z$sm,type="l" )
# plot( tt[,4],type="l",lwd=2 )
# for (j in 1:7){
#   lines( tt[,j],type="l" )
# }
# plot( exp(tt[,4]/2),type="l" )