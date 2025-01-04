#results=matrix(NA,nrow=30,ncol=36)

#Read in length and age files
year=87
len.raw=read.table(paste("len",year,".dat",sep=""), sep=",",header=FALSE)
names(len.raw)=c("cruise","vessel","haul_join","area","bottom_depth","fishing_depth","month","day","year","latdd","londd","species","sex","size_group","frequency")
age.raw=read.table(paste("age",year,".dat",sep=""),sep=",",header=FALSE)
names(age.raw)=c("cruise","vessel","haul_join","area","bottom_depth","fishing_depth","month","day","year","latdd","londd","age","sex","species","weight","length","specimen_number")
len.haul=length(unique(len.raw$haul_join))
age.haul=length(unique(age.raw$haul_join))
#Set up matrices
lenq=array(0,80)
alk=matrix(0,nrow=80,ncol=15)
#fill in the matrices
for (i in 1:nrow(len.raw)){
	len.temp=len.raw[i,]
	if(len.temp$size_group>80) len.temp$size_group=80
	lenq[len.temp$size_group]=lenq[len.temp$size_group]+len.temp$frequency
}
len.samp=sum(lenq)

lenq=lenq/sum(lenq)
for (i in 1:nrow(age.raw)){
	age.temp=age.raw[i,]
	if(age.temp$age>0&age.temp$age<99){
		age.temp$age[age.temp$age>15]=15
		alk[age.temp$length,age.temp$age]=alk[age.temp$length,age.temp$age]+1
	}
}
#Puts 0 in all rows with lengths without ages)
age.samp=sum(alk)
alk=alk/(apply(alk,MARGIN=1,FUN=sum))
alk[is.nan(alk)]=0
agec=lenq %*% alk
agec=agec/sum(agec)
barplot(agec,names.arg=1:15)
#Length weight regressions
x=lm(log(age.raw$weight)~log(age.raw$length))
plot(log(age.raw$weight),log(age.raw$length))
a=exp(coef(x)[1]+(vcov(x)[1,1]/2))
b=coef(x)[2]
wt=a*(seq(1:80))^b
mean.wt=sum(wt*lenq)
#Mean length
lenq_mat=matrix(lenq,ncol=15,nrow=80)
qk=lenq_mat*alk
agec_mat=matrix(agec,nrow=80,ncol=15,byrow=TRUE)
qk=qk/agec_mat
qk=qk/matrix(colSums(qk),nrow=80,ncol=15,byrow=TRUE)
mean.length=colSums(1:80*qk)
mean.weight=a*mean.length^b
results[year-74,1]=year
results[year-74,2]=len.samp
results[year-74,3]=age.samp
results[year-74,4]=len.haul
results[year-74,5]=age.haul
results[year-74,6]=mean.wt
results[year-74,7:21]=agec
results[year-74,22:36]=mean.weight
