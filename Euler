options(width=10000)
options(max.print=9999999)


a=matrix(scan("coordprocom.xvg"),ncol=4,byrow=T)
b=a[,-1]
c=matrix(scan("coordASP33CAY.xvg"),ncol=4,byrow=T)
d=c[,-1]
e=matrix(scan("coordGLY53CAZ.xvg"),ncol=4,byrow=T)
f=e[,-1]

x12=cbind(b[,1],d[,1])
dx12=x12[,2]-x12[,1]
y12=cbind(b[,2],d[,2])
dy12=y12[,2]-y12[,1]
z12=cbind(b[,3],d[,3])
dz12=z12[,2]-z12[,1]

x13=cbind(b[,1],f[,1])
dx13=x13[,2]-x13[,1]
y13=cbind(b[,2],f[,2])
dy13=y13[,2]-y13[,1]
z13=cbind(b[,3],f[,3])
dz13=z13[,2]-z13[,1]

cosalpha=dy12/sqrt(dx12^2 + dy12^2)
alpha=acos(cosalpha)
alphadeg=alpha*180/pi

cosbeta=dz12/sqrt(dx12^2 + dy12^2 + dz12^2)
beta=acos(cosbeta)
betadeg=beta*180/pi

cosgama= (-1*dy13*sin(alpha) + dx13*cosalpha)/sqrt(dx13^2 + dy13^2 + dz13^2)
gama=acos(cosgama)
gamadeg=gama*180/pi
