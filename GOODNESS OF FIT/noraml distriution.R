lower<-c(1,3,5,7,9)
upper<-c(3,5,7,9,11)
x<-(lower+upper)/2
f<-c(1,4,6,4,1)
fx<-x*f
fxx<-f*x*x
fsum<-sum(f)
fxsum<-sum(fx)
fxxsum<-sum(fxx)

mean=fxsum/fsum
sd=(sum(f*(x-mean)^2)/fsum)^0.5

z1=((lower-mean)/sd)
print(z1)

z2=((upper-mean)/sd)
print(z2)

px = pnorm(z2) - pnorm(z1)
print(px)

npx = fsum*px
print(npx)
result=round(npx)
print(result)


mydata=data.frame(lower,upper,x,f,fx,fxx,z1,z2,px,npx,result)
sums=list("","","",fsum,fxsum,fxxsum,"","","","","")
mydata=rbind(mydata,sums)
print(mydata,row.names=FALSE)

res=((f-npx)^2)/npx

chisq=sum(res)

chitable<-qchisq(.95,df=length(x)-1)

print(paste("the chi value is:",chisq))

print(paste("the chi table value is:",chitable))

if(chisq>chitable){
  print("h0 is rejected")
  print("h1 is accepeted")
}else{
  print("h0 is accepeted")
  print("h1 is rejected")
}
print("noraml distribution is completed!!!")