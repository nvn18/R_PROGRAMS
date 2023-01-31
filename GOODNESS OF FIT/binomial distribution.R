x<-c(0,1,2,3,4,5,6)

f<-c(13,25,52,58,32,16,4)

fx<-x*f

sumfx<-sum(fx)

fsum=sum(f)

mean=sumfx/fsum

n=length(x)-1

p=mean/n

print(p)

q=1-p

r=function(n,r)
{
  return(factorial(n)/(factorial(n-r)*factorial(r)))
}
px=c()

for(i in x){
t=r(n,i)*(p^i)*(q^(n-i))
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
print("binomal distribution is completed!!!")