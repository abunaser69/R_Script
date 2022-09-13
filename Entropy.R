options(width=10000)
options(max.print=9999999)


a=matrix(scan("eigen5ps-40ns.xvg"), ncol=2547, byrow=T)
b=log(a*4569.75 +1)
c=apply(b,1,sum)
e=(0.00831451/2)*c
f=matrix(e, ncol=1)
g=seq(0,40000,by=500)
i=matrix(g, ncol=1)
j=i[-1,]
k=matrix(j,ncol=1)
l=cbind(k,f)
