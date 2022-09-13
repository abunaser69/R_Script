options(width=10000)
options(max.print=9999999)


a=matrix(scan("count_0.01.dat"), ncol=1)

b=a/sum(a)

sum(b)

c=b/0.01

sum(c*0.01)

dg=-1*300*0.00831451*log(c/1.47)

dgsmax=dg-max(dg)
