x<-c(0,1,2,3,4,5,6,7,8)

f<-c(103,143,98,42,8,4,2,0,0)

fx<-x*f

sumfx<-sum(fx)

fsum=sum(f)

n=max(x)
lambda=sumfx/fsum

px=c()

for(i in x){
  t=(exp(-lambda)*(lambda^i))/(factorial(i))
  px=c(px,t)
}

print(px)

npx=c(fsum*px)

print(npx)

result=round(npx)

print(result)

mydata<-data.frame(x,f,fx,px,npx,result)

sums = list("",fsum,sumfx,"","",sum(result))

mydata<-rbind(mydata,sums)

print(mydata,row.names=FALSE)

res=((f-npx)^2)/npx

chisq=sum(res)

chitable<-qchisq(.95,df=n)

print(paste("the chi value is:",chisq))

print(paste("the chi table value is:",chitable))

if(chisq>chitable){
  print("h0 is rejected")
  print("h1 is accepeted")
}else{
  print("h0 is accepeted")
  print("h1 is rejected")
}
print("pission distribution is completed!!!")