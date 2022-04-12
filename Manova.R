#data
data=read.csv(file.choose(),sep=";")
data
data1=data[1:8,-3]
data2=data[9:16,-3]
data3=data[17:24,-3]
data4=data[,-3]
as.factor(data$Biru)
p=2 #banyak variabel y (gabah dan jerami)
n=8 #banyak data yg diambil
k=3 #banyaknya varietas (biru1,biru2,biru3)
xbar1=colMeans(data1)
xbar2=colMeans(data2)
xbar3=colMeans(data3)
covdata1=cov(data1)
covdata2=cov(data2)
covdata3=cov(data3)

##Test Multivariat Normal
library(MVN)
mvn(data=data4,mvnTest="mardia")

##Homogenitas Varians
install.packages("biotools")
library(biotools)
#Ho ditolak jika p value < alpha. jika Ho diterima, data tsb homogen
BM=boxM(data=data4,group=data[,3]);BM 

#Persiapan data untuk uji Manova
N1=nrow(data1)
N2=nrow(data2)
N3=nrow(data3)

x1=as.matrix(data1)
x2=as.matrix(data2)
x3=as.matrix(data3)
x=x1+x2+x3

xb1=colMeans(data1)
xb2=colMeans(data2)
xb3=colMeans(data3)
xb=cbind(xb1,xb2,xb3);xb

S1=cov(data1)
S2=cov(data2)
S3=cov(data3)

#H0 = rataan >=2 grup sampel diambil dari sampel distribusi yang sama
#H1 = paling sedikit terdapat 2 rataan yang tidak sama

#Manova one way (wilks lambda)
H=N1*(xb1-xb)%*%t(xb1-xb)+N2*(xb2-xb)%*%t(xb2-xb)+N3*(xb3-xb)%*%t(xb3-xb)
E=(N1-1)*S1+(N2-1)*S2+(N3-1)*S3
lambda=det(E)/det(H+E);lambda
VH=k-1
VE=k*(n-1)
W=VE+VH-0.5*(p+VH+1)
t=sqrt(((p^2)*(VH^2)-4)/((p^2)+(VH^2)-5))
df1=p*VH
df2=W*t-0.5*(p*VH-2)
Fhitung=((1-lambda^(1/t))/lambda^(1/t))*(df2/df1);Fhitung
Ftabel=qf(1-0.05,df1,df2);Ftabel
#H0 ditolak karena Fhitung > Ftabel

#Manova one way (pillai)
res.man=manova(cbind(Gabah,Jerami)~Biru,data=data)
summary(res.man)
#kesimpulan: karena p value < alpha maka H0 ditolak

#Uji Lanjut jika Ho ditolak (untuk mengetahui penyebab penolakan)
res.man=manova(cbind(Gabah,Jerami)~Biru,data=data)
summary.aov(res.man)
#kesimpulan: untuk gabah, pvalue < alpha maka Ho ditolak artinya terdapat perbedaan rata2 dari hasil gabah varietas 1 berbeda dengan varietas 2 dan 3
#kesimpulan: untuk jerami, pvalue > alpha maka Ho diterima artinya tidak terdapat perbedaan antara varietas 1 2 3
