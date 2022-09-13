options(width=10000)
options(max.print=9999999)


a=matrix(scan("coordprocom20ns.xvg"),ncol=4,byrow=T)
b=a[,-c(1,2,3)]
c=matrix(b, ncol=1)
d=(c-1.5)
e=matrix(scan("coordref.xvg"),ncol=4,byrow=T)
f=e[,-c(1,2,3)]
g=matrix(f, ncol=1)
j=g[(1:400),]
k=matrix(j, ncol=1)
i=hist(d,breaks=seq(0.2,1.03,by=0.01))
#l=hist(k,breaks=seq(2.73,3.46,by=0.01))
