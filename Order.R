options(width=10000)
options(max.print=9999999)

#library(kza)

a=matrix(scan("order.dat"), ncol=1)

b=kz(a,50)
#b=kza(a, 50, kz=NULL, k = 3, m = 0, tol = 1.0e-5)

c=matrix(b, ncol=1)

d=matrix(seq(0,40000,by=2), ncol=1)

e=cbind(d,c)

g=seq(a[1,], a[2001,], by= 2)


