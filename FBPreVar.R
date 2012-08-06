require(plyr)
require(stringr)
require(tm)
require(RecordLinkage)
require(R.oo)
require(psych)
library("lubridate")
require(penalized)
setwd("C:/My Projects/Zheng - FB Data Predict Vars/")

#collect the family size information from fb per applicationid
fbfamily<-read.csv("fbfamily.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
summary(fbfamily$ApplicationId)
head(fbfamily)
colnames(fbfamily)[1]<-"family_name"
familynumber<-ddply(fbfamily,"ApplicationId",function(df) dim(df)[1])
colnames(familynumber)[2]<-"size"
head(familynumber)


fbinterests<-read.csv("fbinterests.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
head(fbinterests)
colnames(fbinterests)[1]<-"interestname"
fbinterests$interestcategory<-tolower(fbinterests$interestcategory)
str_match(fbinterests$interestcategory,"business")
interestsnumber<-ddply(fbinterests,"ApplicationId", function(df) length(unique(df$interestcategory)))
colnames(interestsnumber)[2]<-"number"

interestspublic<-ddply(fbinterests,"ApplicationId", function(df) 'public figure' %in% df$interestcategory)
colnames(interestspublic)[2]<-"interestpublic"
interestsbusiness<-ddply(fbinterests,"ApplicationId", function(df) ('business services' %in% df$interestcategory)|('local business' %in% df$interestcategory))
colnames(interestsbusiness)[2]<-"interestbusiness"

fblikes<-read.csv("fblikes.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(fblikes)[1]<-"likename"
likecategory<-ddply(fblikes,"ApplicationId", function(df) length(unique(df$likecategory)))
colnames(likecategory)[2]<-"likesize"
head(likecategory)

fbstatusdate<-read.csv("FBstatusdate.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(fbstatusdate)[1]<-"postname"
fbstatusdate$update_time<-as.Date(fbstatusdate$update_time,"%Y-%m-%d")
fbstatusdate$create_time<-as.Date(fbstatusdate$create_time,"%Y-%m-%d")
createwithmonth<-ddply(fbstatusdate,"ApplicationId", function(df) c(sum(df$create_time>(Sys.Date()-months(1))), sum(df$create_time>(Sys.Date()-months(3))),sum(df$create_time>(Sys.Date()-months(6)))))
colnames(createwithmonth)[2]<-"createin1m"
colnames(createwithmonth)[3]<-"createin3m"
colnames(createwithmonth)[4]<-"createin6m"
colnames(createwithmonth)
fbdatawithperf<-merge(fbdatawithperf,createwithmonth,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
head(fbdatawithperf)
fbdatawithperf$createin1m<-ifelse(is.na(fbdatawithperf$createin1m),0,fbdatawithperf$createin1m)
fbdatawithperf$createin3m<-ifelse(is.na(fbdatawithperf$createin3m),0,fbdatawithperf$createin3m)
fbdatawithperf$createin6m<-ifelse(is.na(fbdatawithperf$createin6m),0,fbdatawithperf$createin6m)


fbevent<-read.csv("fbevent.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
fbevent$start_time<-as.Date(fbevent$start_time,"%Y-%m-%d")
Sys.Date()
sum(fbevent$start_time>Sys.Date())
dim(fbevent)
eventnumber<-ddply(fbevent,"ApplicationId", function(df) dim(df)[1])
lastevent<-ddply(fbevent,"ApplicationId", function(df) as.numeric(Sys.Date()-max(df$start_time)))
eventwith1m<-ddply(fbevent,"ApplicationId", function(df) c(sum(df$start_time>(Sys.Date()-months(1))), sum(df$start_time>(Sys.Date()-months(3))),sum(df$start_time>(Sys.Date()-months(6)))))
fbdatawithperf<-merge(fbdatawithperf,eventwith1m,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
fbdatawithperf$event6m<-ifelse(is.na(fbdatawithperf$event6m),0,fbdatawithperf$event6m)
fbdatawithperf$event3m<-ifelse(is.na(fbdatawithperf$event3m),0,fbdatawithperf$event3m)
fbdatawithperf$event1m<-ifelse(is.na(fbdatawithperf$event1m),0,fbdatawithperf$event1m)

colnames(eventnumber)[2]<-"eventsize"

##the below bit of code is recently added need to be done before convert fbdata to fbdatawithperf
fbstatus<-read.csv("fbstatus.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);

commentsall<-ddply(fbstatus,"ApplicationId", function(df) dim(df)[1])
uniqueusers<-ddply(fbstatus,"ApplicationId", function(df) length(unique(df$replyname)))
colnames(commentsall)[2]<-"totalcomments"
colnames(uniqueusers)[2]<-"commentusersize"
fbdata1<-merge(fbdata,commentsall,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
head(fbdata1)
fbdata1<-merge(fbdata1,uniqueusers,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
fbdata<-fbdata1
head(fbdata)
fbdata1<-merge(fbdatawithperf,commentsall,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
fbdata1<-merge(fbdata1,uniqueusers,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
fbdatawithperf<-fbdata1
######################################################

eventnumber<-ddply(fbevent,"ApplicationId", function(df) dim(df)[1])
lastevent<-ddply(fbevent,"ApplicationId", function(df) as.numeric(Sys.Date()-max(df$start_time)))
colnames(eventnumber)[2]<-"eventsize"



fbgroup<-read.csv("fbgroups.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(fbgroup)
groupsize<-ddply(fbgroup,"ApplicationId", function(df) length(unique(df$group)))

fbactivity<-read.csv("fbactivity.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
summary(fbactivity)
head(fbactivity)
fbactivity$created_time<-as.Date(fbactivity$created_time,"%Y-%m-%d")
activitysize<-ddply(fbactivity,"ApplicationId", function(df) length(df$actname))
colnames(activitysize)[2]<-"activitysize"
activitycatsize<-ddply(fbactivity,"ApplicationId", function(df) length(unique(df$actcatogory)))
activitywithmonth<-ddply(fbactivity,"ApplicationId",function(df) c(sum(df$created_time>(Sys.Date()-months(1))), sum(df$created_time>(Sys.Date()-months(3))),sum(df$created_time>(Sys.Date()-months(6)))))
colnames(activitywithmonth)[2]<-"activityin1m"
colnames(activitywithmonth)[3]<-"activityin3m"
colnames(activitywithmonth)[4]<-"activityin6m"

fbdatawithperf<-merge(fbdatawithperf,activitywithmonth,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
fbdatawithperf$activityin1m[is.na(fbdatawithperf$activityin1m)]<-0
fbdatawithperf$activityin3m[is.na(fbdatawithperf$activityin3m)]<-0
fbdatawithperf$activityin6m[is.na(fbdatawithperf$activityin6m)]<-0
range(fbdatawithperf$activityin6m,na.rm=TRUE)

linksnumber<-read.csv("linknumofappid.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(linksnumber)[1]<-"linknumber"


fbdata<-read.csv("FBdatame.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
fbdata$id<-format(as.numeric(fbdata$id),scientific=FALSE)
head(fbdata)
fbdata$birthday<-as.Date(fbdata$birthday,"%d/%m/%Y")
fbdata$appDate<-as.Date(fbdata$appDate,"%d/%m/%Y")
fbdata$lastupdateday<-as.Date(fbdata$lastupdateday,"%d/%m/%Y")
fbdata$appDOB<-as.Date(fbdata$appDOB,"%d/%m/%Y")
fbdata$PayBackDate<-as.Date(fbdata$PayBackDate,"%d/%m/%Y")
fbdata$NextDueDate<-as.Date(fbdata$NextDueDate,"%d/%m/%Y")
colnames(fbdata)[7]<-"gender"

fbdata1<-merge(fbdata,familynumber,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
colnames(fbdata1)[17]<-"familysize"
fbdata1<-merge(fbdata1,interestsnumber,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
colnames(fbdata1)[18]<-"interestnumber"

fbdata1<-merge(fbdata1,interestspublic,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
fbdata1<-merge(fbdata1,interestsbusiness,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
fbdata1<-merge(fbdata1,likecategory,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
colnames(fbdata1)[21]<-"likecatsize"
fbdata1<-merge(fbdata1,eventnumber,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
fbdata1<-merge(fbdata1,lastevent,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
fbdata1<-merge(fbdata1,groupsize,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
fbdata1<-merge(fbdata1,activitysize,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
fbdata1<-merge(fbdata1,activitycatsize,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
fbdata1<-merge(fbdata1,activitycatsize,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
fbdata1<-merge(fbdata1,linksnumber,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationID")
head(fbdata1)

fbdata<-read.csv("fbdata1.csv",header=TRUE,sep=",",na.strings="NA",stringsAsFactors=FALSE);
fbdata$firstname<-trim(tolower(fbdata$firstname))
fbdata$surname<-trim(tolower(fbdata$surname))
fbdata$appfirstname<-trim(tolower(fbdata$appfirstname))
fbdata$appsurname<-trim(tolower(fbdata$appsurname))
fbdata$jarofirstname<-jarowinkler(fbdata$firstname,fbdata$appfirstname)
fbdata$jarosurname<-jarowinkler(fbdata$surname,fbdata$appsurname)
fbdata<-transform(fbdata,jarosim=(jarofirstname+jarosurname)/2)
fbdata<-transform(fbdata,DOBMatch=ifelse(birthday==appDOB,1,0))
fbdata<-(transform(fbdata,age=floor(as.numeric(Sys.Date()-fbdata$birthday)/365)))
fbdatawithperf<-subset(fbdata,!is.na(Bad45))


ggplot(fbdatawithperf,aes(x=jarosim,y=age,color=as.factor(Bad45)))+geom_point()
ggplot(fbdatawithperf,aes(x=jarosim,y=age,color=as.factor(Bad45)))+geom_point()+geom_jitter()
length(fbdatawithperf$Bad45[fbdatawithperf$jarosim<1.0])
length(fbdatawithperf$Bad45[fbdatawithperf$Bad45==1&fbdatawithperf$jarosim<1.0])
length(fbdatawithperf$Bad45[fbdatawithperf$jarosim==1.0])
length(fbdatawithperf$Bad45[fbdatawithperf$Bad45==1&fbdatawithperf$jarosim==1.0])


pred<-prediction(fbdatawithperf$jarosim,fbdatawithperf$Bad45)
perf<-performance(pred,"tpr","fpr")
plot(perf,main="ROC curve for mixed score and Arrears45",colorize=T)
auc<-performance(pred,"auc")
auc
jarolevel<-seq(1,0,-0.01)
arrears45<-rep(0,101)
index <- 1
for(i in seq(1,0,-0.01))
{
  arrears45[index]<-length(fbdatawithperf$Bad45[fbdatawithperf$Bad45==1&fbdatawithperf$jarosim>=i])/length(fbdatawithperf$Bad45[fbdatawithperf$jarosim>=i])
  index<- index + 1
}
jaroarrears<-data.frame(jarolevel,arrears45)
jaroarrears<-jaroarrears[with(jaroarrears,order(jarolevel)),]
ggplot(jaroarrears,aes(x=jarolevel,y=arrears45))+geom_line()
ggplot(jaroarrears,aes(x=jarolevel,y=arrears45))+geom_point()+geom_smooth()

summary(fbdatawithperf$linknumber)
fbdatawithperf$linknumber<-ifelse(is.na(fbdatawithperf$linknumber),0,fbdatawithperf$linknumber)
ggplot(fbdatawithperf,aes(x=jarosim,y=linknumber,color=as.factor(Bad45)))+geom_point()+facet_grid(DOBMatch~.)
ggplot(fbdatawithperf,aes(x=linknumber))+geom_histogram(binwidth=20)+facet_grid(gender~.)
mean(fbdatawithperf$linknumber[which(fbdatawithperf$gender=='femal')])
mean(fbdatawithperf$linknumber[which(fbdatawithperf$gender=='male')])
head(fbdatawithperf)
fbdatawithperf<-transform(fbdatawithperf,linknumberratio=linknumber/2000)
fbdatawithperf$linknumberratio<-ifelse(fbdatawithperf$linknumberratio>1,1,fbdatawithperf$linknumberratio)


linklevel<-seq(0,1,0.001)
length(linklevel)
arrears45<-rep(0,1001)
proportion<-rep(0,1001)
index <- 1
for(i in seq(0,1,0.001))
{
  arrears45[index]<-length(fbdatawithperf$Bad45[fbdatawithperf$Bad45==1&fbdatawithperf$linknumberratio>=i])/length(fbdatawithperf$Bad45[fbdatawithperf$linknumberratio>=i])
  proportion[index]<-length(fbdatawithperf$Bad45[fbdatawithperf$linknumberratio>=i])/2440
  index<- index + 1
}
linkarrears<-data.frame(linklevel,arrears45,proportion)
tail(linkarrears)

p<-ggplot(linkarrears,aes(x=linklevel,y=arrears45))+geom_line()
p1<-ggplot(linkarrears,aes(x=linklevel,y=proportion))+geom_line())


length(which(fbdatawithperf$DOBMatch==1&fbdatawithperf$Bad45==1&fbdatawithperf$jarosim==1))/length(which(fbdatawithperf$DOBMatch==1&fbdatawithperf$jarosim==1))

length(which(fbdatawithperf$interestpublic==1&fbdatawithperf$Bad45==1))/length(which(fbdatawithperf$interestpublic==1))

length(which(fbdatawithperf$interestbusiness==1&fbdatawithperf$Bad45==1))/length(which(fbdatawithperf$interestbusiness==1))

fbdatawithperf$interestnumber<-ifelse(is.na(fbdatawithperf$interestnumber),0,fbdatawithperf$interestnumber)
ggplot(fbdatawithperf,aes(x=interestnumber))+geom_histogram(binwidth=1)+facet_grid(Bad45~.)

summary(fbdatawithperf)

fbdatawithperf$familysize<-ifelse(is.na(fbdatawithperf$familysize),0,fbdatawithperf$familysize)
ggplot(fbdatawithperf,aes(x=familysize))+geom_histogram(binwidth=1)+facet_grid(Bad45~.)

ggplot(fbdatawithperf,aes(x=interestnumber,y=familysize,color=as.factor(Bad45)))+geom_point()+geom_jitter()

fbdatawithperf$likecatsize<-ifelse(is.na(fbdatawithperf$likecatsize),0,fbdatawithperf$likecatsize)
ggplot(fbdatawithperf,aes(x=likecatsize))+geom_histogram(binwidth=5)+facet_grid(Bad45~.)

fbdatawithperf$eventsize<-ifelse(is.na(fbdatawithperf$eventsize),0,fbdatawithperf$eventsize)
length(which(fbdatawithperf$eventsize>10&fbdatawithperf$Bad45==1))/length(which(fbdatawithperf$eventsize>10))

familysize<-seq(0,67,1)
arrears45<-rep(0,68)
size<-rep(0,68)
index <- 1
for(i in seq(0,67,1))
{
  arrears45[index]<-length(fbdatawithperf$Bad45[fbdatawithperf$Bad45==1&fbdatawithperf$familysize>=i])
  size[index]<-length(fbdatawithperf$Bad45[fbdatawithperf$familysize>=i])
  index<- index + 1
}
familysizearrears<-data.frame(familysize,arrears45,size)
familysizearrears<-transform(familysizearrears,arrearsratio=arrears45/size)
familysizearrears

eventsize<-seq(0,37,1)
arrears45<-rep(0,38)
size<-rep(0,37)
index <- 1
for(i in seq(0,37,1))
{
  arrears45[index]<-length(fbdatawithperf$Bad45[fbdatawithperf$Bad45==1&fbdatawithperf$eventsize>=i])
  size[index]<-length(fbdatawithperf$Bad45[fbdatawithperf$eventsize>=i])
  index<- index + 1
}
eventarrears<-data.frame(eventsize,arrears45,size)
eventarrears<-transform(eventarrears,arrearsratio=arrears45/size)

fbdatawithperf$activitysize<-ifelse(is.na(fbdatawithperf$activitysize),0,fbdatawithperf$activitysize)

activitysize<-seq(0,424,1)
arrears45<-rep(0,425)
size<-rep(0,425)
index <- 1
for(i in seq(0,424,1))
{
  arrears45[index]<-length(fbdatawithperf$Bad45[fbdatawithperf$Bad45==1&fbdatawithperf$activitysize>=i])
  size[index]<-length(fbdatawithperf$Bad45[fbdatawithperf$activitysize>=i])
  index<- index + 1
}
activitysizearrears<-data.frame(activitysize,arrears45,size)
activitysizearrears<-transform(activitysizearrears,arrearsratio=arrears45/size)
ggplot(activitysizearrears,aes(x=activitysize,y=arrearsratio))+geom_line()
ggplot(activitysizearrears,aes(x=activitysize,y=arrearsratio))+geom_line()+geom_smooth()

fbdatawithperf$totalcomments<-ifelse(is.na(fbdatawithperf$totalcomments),0,fbdatawithperf$totalcomments)
fbdatawithperf$commentusersize<-ifelse(is.na(fbdatawithperf$commentusersize),0,fbdatawithperf$commentusersize)
head(fbdatawithperf)
ggplot(fbdatawithperf,aes(x=totalcomments))+geom_histogram(binwidth=5)+facet_grid(Bad45~.)
ggplot(fbdatawithperf,aes(x=commentusersize))+geom_histogram(binwidth=5)+facet_grid(Bad45~.)
ggplot(fbdatawithperf,aes(x=createin3m))+geom_histogram(binwidth=10)+facet_grid(Bad45~.)

uniqueusersize<-seq(0,273,1)
arrears45<-rep(0,274)
size<-rep(0,274)
index <- 1
for(i in seq(0,273,1))
{
  arrears45[index]<-length(fbdatawithperf$Bad45[fbdatawithperf$Bad45==1&fbdatawithperf$commentusersize>=i])
  size[index]<-length(fbdatawithperf$Bad45[fbdatawithperf$commentusersize>=i])
  index<- index + 1
}
uniqueusersizearrears<-data.frame(uniqueusersize,arrears45,size)
uniqueusersizearrears<-transform(uniqueusersizearrears,arrearsratio=arrears45/size)
ggplot(uniqueusersizearrears,aes(x=uniqueusersize,y=arrearsratio))+geom_line()+geom_smooth()
write.csv(uniqueusersizearrears,file="uniqueusersizearrears.csv")

commentsize<-seq(0,1184,1)
arrears45<-rep(0,1185)
size<-rep(0,1185)
index <- 1
for(i in seq(0,1184,1))
{
  arrears45[index]<-length(fbdatawithperf$Bad45[fbdatawithperf$Bad45==1&fbdatawithperf$totalcomments>=i])
  size[index]<-length(fbdatawithperf$Bad45[fbdatawithperf$totalcomments>=i])
  index<- index + 1
}
commentsizearrears<-data.frame(commentsize,arrears45,size)
commentsizearrears<-transform(commentsizearrears,arrearsratio=arrears45/size)
ggplot(commentsizearrears,aes(x=commentsize,y=arrearsratio))+geom_line()+geom_smooth()
write.csv(commentsizearrears,file="commentsizearrears.csv")


pred<-prediction(fbdatawithperf$MixedScore,1-fbdatawithperf$Bad45)
perf<-performance(pred,"tpr","fpr")
plot(perf,main="ROC curve for mixed score and Arrears45",colorize=T)
auc<-performance(pred,"auc")
auc

fbdatanew<-fbdatawithperf[,c("Bad45","jarosim","MixedScore","familysize","interestnumber","interestpublic","interestbusiness","likecatsize","eventsize","activitysize","linknumber","age","linknumberratio","totalcomments","commentusersize","createin1m","createin3m","createin6m","activityin3m")]

fbdatanew$interestpublic<-ifelse(is.na(fbdatanew$interestpublic),0,ifelse(fbdatanew$interestpublic==TRUE,1,0))

fbdatanew$interestbusiness<-ifelse(is.na(fbdatanew$interestbusiness),0,ifelse(fbdatanew$interestbusiness==TRUE,1,0))

fbdatanewneg<-fbdatanew[which(fbdatanew$Bad45==1),]
head(fbdatanewneg)
fbdatanewpos<-fbdatanew[which(fbdatanew$Bad45==0),]
negtrainind<-sample(seq(1:157),100)
negtrain<-fbdatanewneg[negtrainind,]
summary(negtrain)
negtest<-fbdatanewneg[-negtrainind,]
dim(negtest)
postrainind<-sample(seq(1:2283),100)
postrain<-fbdatanewpos[postrainind,]
summary(postrain)
postest<-fbdatanewpos[-postrainind,]
dim(postest)

train<-rbind(negtrain,postrain)
summary(train)

cor.test(train[,"Bad45"],train[,"commentusersize"])
cor.test(train[,"MixedScore"],train[,"commentusersize"])
cor.test(train[,"MixedScore"],train[,"Bad45"])
t.test(commentusersize~as.factor(Bad45),data=train)

##feature selection using caret##############################################

train$Bad45<-as.factor(train$Bad45)
control <- rfeControl(functions = rfFuncs, method = "boot", verbose = FALSE,returnResamp = "final", number = 50)
profile<-rfe(train[,3:14],train$Bad45,sizes=sizes,rfeControl=control)

if(require("multicore",quietly=TRUE,warn.conflicts=FALSE))
  {
    control$workers<-multicore:::detectCores()
    control$computeFunction<-mclapply
    control$computeArgs<-list(mc.preschedule=FALSE,mc.set.seed=FALSE)
   }
gbmFuncs$fit<-function(x,y,first,last,...){
  library("gbm")
  n.levels<-length(unique(y))
  if(n.levels==2){
    distribution = "bernoulli"
  }else{
    distribution = "gaussian"
  }
  gbm.fit(x,y,distribution=distribution,...)
}
gbmFuncs$pred<-function(object, x){
  n.trees<-suppressWarnings(gbm.perf(object,plot.it=FALSE,method="OOB"))
  if(n.trees<=0) n.trees<-object$n.trees
  predict(object,x,n.trees=n.trees,type="link")
}
control$functions<-gbmFuncs

train$Bad45<-as.factor(train$Bad45)
control <- rfeControl(functions = rfFuncs, method = "boot", verbose = FALSE,returnResamp = "final", number = 50)
profile<-rfe(train$Bad45,train[,3:14],sizes=sizes,rfeControl=control)

fit<-randomForest(Bad45~.,data=train)
im
importance(fit)
pred<-predict(fit,negtest)
pred2<-predict(fit,postest)

test<-rbind(negtest,postest)

pred<-prediction(test$MixedScore,1-test$Bad45)
perf<-performance(pred,"tpr","fpr")
plot(perf,main="ROC curve for mixed score and Arrears45",colorize=T)
auc<-performance(pred,"auc")
auc$y.values

glmmodel<-glmnet(as.matrix(train[,seq(2,14,by=1)]),train$Bad45,family="binomial",alpha=0.5)

predglm<-predict(glmmodel,as.matrix(test[,seq(2,14,by=1)]))
pred<-prediction(1-predglm[,"s1"],1-test$Bad45)
perf<-performance(pred,"tpr","fpr")
plot(perf,main="ROC curve for mixed score and Arrears45",colorize=T)
auc<-performance(pred,"auc")
auc





glmmodel<-glm(Bad45~MixedScore+familysize+commentusersize,data=train,family=binomial("logit"))
test$prob<-predict(glmmodel,newdata=test,type="response")
pred<-prediction(1-test$prob,1-test$Bad45)
perf<-performance(pred,"tpr","fpr")
plot(perf,main="ROC curve for mixed score and Arrears45",colorize=T)
auc<-performance(pred,"auc")
auc

colnames(negtest)


n.trees<-1e2
profile.1 <- rfe(x, y.1, sizes = sizes, rfeControl = control, verbose = FALSE,n.trees = n.trees)
profile.2 <- rfe(x, y.2, sizes = sizes, rfeControl = control, verbose = FALSE,
profile.3 <- rfe(x, y.3, sizes = sizes, rfeControl = control, verbose = FALSE,n.trees = n.trees)
profile.4 <- rfe(x, y.4, sizes = sizes, rfeControl = control, verbose = FALSE,n.trees = n.trees)

fbdatanew<-fbdatawithperf[,c("Bad45","jarosim","MixedScore","familysize","interestnumber","interestpublic","interestbusiness","likecatsize","eventsize","activitysize","linknumber","age","linknumberratio","totalcomments","commentusersize","createin1m","createin3m","createin6m","activityin3m")]
fbdatanewneg<-fbdatanew[which(fbdatanew$Bad45==1),]
fbdatanewpos<-fbdatanew[which(fbdatanew$Bad45==0),]                 
                 
############################################################################                 
                 
##test significant of added commentusersize#################################
                 aucaddusersize<-c()
                 aucmixscore<-c()
                 for(i in seq(1,1000, by=1))
                 {
                   negtrainind<-sample(seq(1:157),100)
                   negtrain<-fbdatanewneg[negtrainind,]
                   negtest<-fbdatanewneg[-negtrainind,]
                   
                   postrainind<-sample(seq(1:2283),100)
                   postrain<-fbdatanewpos[postrainind,]
                   postest<-fbdatanewpos[-postrainind,]
                   
                   train<-rbind(negtrain,postrain)
                   test<-rbind(negtest,postest)
                   
                  # glmmodel<-glm(Bad45~MixedScore+commentusersize#createin3m,data=train,family=binomial("logit"))
                   glmmodel<-glm(Bad45~MixedScore+activityin3m,data=train,family=binomial("logit"))
                   test$prob<-predict(glmmodel,newdata=test,type="response")
                   pred<-prediction(1-test$prob,1-test$Bad45)
                   auc1<-performance(pred,"auc")
                   aucaddusersize<-c(auc1@y.values[[1]][1],aucaddusersize)
                   
                   pred<-prediction(test$MixedScore,1-test$Bad45)
                   auc2<-performance(pred,"auc")
                   aucmixscore<-c(auc2@y.values[[1]][1],aucmixscore)
                 }
                 re<-data.frame(aucaddusersize,aucmixscore)
                 
                 with(re,wilcox.test(aucaddusersize,aucmixscore,paired=TRUE))                 
                 with(re,t.test(aucaddusersize,aucmixscore,paired=TRUE))
                 ggplot(data=re,aes(x=aucaddusersize,y=aucmixscore))+geom_point()
                 with(re,sum(aucaddusersize>aucmixscore))
                 
                 wilcox.test(event3m~Bad45,data=fbdatawithperf) 
                 t.test(event1m~Bad45,data=fbdatawithperf)
                 with(fbdatawithperf,cor.test(event6m,Bad45))
                 
######################################################################################
                 
eventwith1m<-ddply(fbevent,"ApplicationId", function(df) c(sum(df$start_time>(Sys.Date()-months(1))), sum(df$start_time>(Sys.Date()-months(3))),sum(df$start_time>(Sys.Date()-months(6)))))
fbdatawithperf<-merge(fbdatawithperf,eventwith1m,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationId")
fbdatawithperf$event1m<-ifelse(is.na(fbdatawithperf$event1m),0,fbdatawithperf$event1m)
fbdatawithperf$event3m<-ifelse(is.na(fbdatawithperf$event3m),0,fbdatawithperf$event3m)
fbdatawithperf$event6m<-ifelse(is.na(fbdatawithperf$event6m),0,fbdatawithperf$event6m)
head(fbdatawithperf)

attach(fbdatawithperf)
summary(fbdatawithperf)
fit<-aov(Bad45~(createin3m+commentusersize+activityin3m+jarosim)^2)
summary(fit)                 
                 
######add the model to the whole population rather than FB connection population                 
fbdatawithproid<-read.csv("FBdatamewithproposalid.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);               
fbdatawithproid<-data.frame(fbdatawithproid$ApplicationID,fbdatawithproid$ProposalID,fbdatawithproid$MixedScore)
colnames(fbdatawithproid)<-c("ApplicationID","ProposalID","MixedScore") 
summary(fbdatawithperf)
fbdata1<-merge(fbdatawithperf,fbdatawithproid,all.x=TRUE,by.x="ApplicationID",by.y="ApplicationID")
fbdatawithperf<-fbdata1
proids<-fbdata1$ProposalID
                 
performapr<-read.csv("performapr.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(performapr)[1]<-"PayBackDate"
dim(performapr)
sum(performapr$ProposalID %in% proids)
performapr<-performapr[complete.cases(performapr),]
performapr<-performapr[(!(performapr$ProposalID %in% proids)),]
dim(performapr)
#trainnofb<-performapr[index2,]                   
index2<-sample(1:dim(performapr)[1],5000)
fbdatanofb<-performapr[index2,]
fbdatanofb$createin3m<--1
fbdatanewneg2<-fbdatanewneg[,c("Bad45","MixedScore","createin3m","activitysize","jarosim","totalcomments","commentusersize","eventsize","linknumberratio","linknumber","familysize","interestnumber","activityin3m")]
fbdatanewpos2<-fbdatanewpos[,c("Bad45","MixedScore","createin3m","activitysize","jarosim","totalcomments","commentusersize","eventsize","linknumberratio","linknumber","familysize","interestnumber","activityin3m")]

fbdatanofb$createin3m<--1
fbdatanofb$activitysize<--1
fbdatanofb$jarosim<--1
fbdatanofb$totalcomments<--1
fbdatanofb$commentusersize<--1
fbdatanofb$eventsize<--1
fbdatanofb$linknumberratio<--1
fbdatanofb$linknumber<--1
fbdatanofb$familysize<--1
fbdatanofb$interestnumber<--1
fbdatanofb$activityin3m<--1
fbdatanofb2<-  fbdatanofb[,c("Bad45","MixedScore","createin3m","activitysize","jarosim","totalcomments","commentusersize","eventsize","linknumberratio","linknumber","familysize","interestnumber","activityin3m")]
dim(fbdatanofb2)
                 
#if we use all L0 application, then as follows##
fbdatanofb<-performapr                 
fbdatanofb$createin3m<--1
fbdatanofb2<-  fbdatanofb[,c("Bad45","MixedScore","createin3m")]                 
    
colnames(fbdatanofb2)[3]<-"eventsize"

###########################################################################                 
                 
##test significant on the whole population#################################
beats<-c()
ttest<-c()                 
for(s in seq(100,1000,by=100))
{
                 aucaddusersize<-c()
                 aucmixscore<-c()
                 for(i in seq(1,1000, by=1))#1000 times sample to see the performance
                 {
                   negtrainind<-sample(seq(1:157),100)
                   negtrain<-fbdatanewneg2[negtrainind,c("Bad45","MixedScore","eventsize")]
                   negtest<-fbdatanewneg2[-negtrainind,c("Bad45","MixedScore","eventsize")]
                   
                   postrainind<-sample(seq(1:2283),2000)
                   postrain<-fbdatanewpos2[postrainind,c("Bad45","MixedScore","eventsize")]
                   postest<-fbdatanewpos2[-postrainind,c("Bad45","MixedScore","eventsize")]
                   
                   nofbtrainind<-sample(seq(1:dim(fbdatanofb2)[1]),4000)
                   nofbtrain<-fbdatanofb2[nofbtrainind,]
                   nofbtest<-fbdatanofb2[-nofbtrainind,]
                  
                   train<-rbind(negtrain,postrain,nofbtrain)
                   test<-rbind(negtest,postest,nofbtest)
                   
             #      train$fbflag<-ifelse(train$createin3m>=0,1,0)
             #      test$fbflag<-ifelse(test$createin3m>=0,1,0)
                   train$fbflag<-ifelse(train$eventsize>=0,1,0)
                   test$fbflag<-ifelse(test$eventsize>=0,1,0)
                   
                   
                   # glmmodel<-glm(Bad45~MixedScore+commentusersize#createin3m,data=train,family=binomial("logit"))
                   glmmodel<-glm(Bad45~MixedScore+fbflag+eventsize,data=train,family=binomial("logit"))
                   test$prob<-predict(glmmodel,newdata=test,type="response")
                   pred<-prediction(1-test$prob,1-test$Bad45)
                   auc1<-performance(pred,"auc")
                   aucaddusersize<-c(auc1@y.values[[1]][1],aucaddusersize)
                   
                   pred<-prediction(test$MixedScore,1-test$Bad45)
                   auc2<-performance(pred,"auc")
                   aucmixscore<-c(auc2@y.values[[1]][1],aucmixscore)
                 }
                 re<-data.frame(aucaddusersize,aucmixscore)
                 
                 with(re,sum(aucaddusersize>aucmixscore))
                 
                 with(re,wilcox.test(aucaddusersize,aucmixscore,paired=TRUE))                 
                 with(re,t.test(aucaddusersize,aucmixscore,paired=TRUE))
                 ggplot(data=re,aes(x=aucaddusersize,y=aucmixscore))+geom_point()
                 beats<-c(beats,with(re,sum(aucaddusersize>aucmixscore)))
                 
                 wilcox.test(event3m~Bad45,data=fbdatawithperf) 
                 t.test(event1m~Bad45,data=fbdatawithperf)
                 with(fbdatawithperf,cor.test(event6m,Bad45))
}
                 
                 re30000<-re
                 
                 
######################################################################################
refull_L0<-re
colnames(refull_L0)[1]<-"auc3features"                 
x<-seq(0.71,0.74,by=0.001)
y<-seq(0.71,0.74,by=0.001)                 
diag<-data.frame(x,y)
p1<-ggplot(refull_L0,aes(x=aucmixscore,y=auc3features))+geom_point()+geom_smooth()                 
p1+geom_line(aes(x=x,y=y,color="red"),data=diag)
                 
size<-seq(100,1000,by=100)
res<-data.frame(size,beats)
                 
penfit<-penalized(Bad45,penalized=~MixedScore+familysize+eventsize+jarosim+linknumberratio+totalcomments+commentusersize+event3m+createin3m+activityin3m,data=fbdatawithperf,lambda1=10,model="logistic")
summary(penfit)   
coefficients(penfit)

####Using Caret package to find good features############
                 fbdatawithperf$Bad45<-as.factor(fbdatawithperf$Bad45)
                 control <- rfeControl(functions = rfFuncs, method = "boot", verbose = FALSE,returnResamp = "final", number = 200)
                 profile<-rfe(fbdatawithperf[,17:45],fbdatawithperf$Bad45,sizes=sizes,rfeControl=control)
                 profile
#########################################################

############
                 aucaddusersize<-c()
                 aucmixscore<-c()
                 aucaddfe<-c()
                 aucaddfc<-c()  
                 aucaddfa<-c()
                 aucaddfj<-c()	
                 aucaddft<-c()				      
                 aucaddfcu<-c()
                 aucaddflr<-c()
                 aucaddfl<-c()
                 aucaddff<-c()
                 aucaddfi<-c()			   
                 aucaddfa3m<-c()
                 
                 aucadde<-c()
                 aucaddc<-c()	
                 aucadda<-c()
                 aucaddj<-c()	
                 aucaddt<-c()				      
                 aucaddcu<-c()
                 aucaddlr<-c()
                 aucaddl<-c()
                 aucaddf<-c()
                 aucaddi<-c()			   
                 aucadda3m<-c()
                 
                 for(i in seq(1,1000, by=1))#1000 times sample to see the performance
                 {
                   negtrainind<-sample(seq(1:157),100)
                   negtrain<-fbdatanewneg2[negtrainind,]
                   negtest<-fbdatanewneg2[-negtrainind,]
                   
                   postrainind<-sample(seq(1:2283),2000)
                   postrain<-fbdatanewpos2[postrainind,]
                   postest<-fbdatanewpos2[-postrainind,]
                   
                   nofbtrainind<-sample(seq(1:dim(fbdatanofb2)[1]),4000)
                   nofbtrain<-fbdatanofb2[nofbtrainind,]
                   nofbtest<-fbdatanofb2[-nofbtrainind,]
                   
                   train<-rbind(negtrain,postrain,nofbtrain)
                   test<-rbind(negtest,postest,nofbtest)
                   
                   #      train$fbflag<-ifelse(train$createin3m>=0,1,0)
                   #      test$fbflag<-ifelse(test$createin3m>=0,1,0)
                   train$fbflag<-ifelse(train$eventsize>=0,1,0)
                   test$fbflag<-ifelse(test$eventsize>=0,1,0)
                   
                   
                   # glmmodel<-glm(Bad45~MixedScore+commentusersize#createin3m,data=train,family=binomial("logit"))
                   glmmodelfe<-glm(Bad45~MixedScore+fbflag+eventsize,data=train,family=binomial("logit"))
                   test$probfe<-predict(glmmodelfe,newdata=test,type="response")
                   pred<-prediction(1-test$probfe,1-test$Bad45)
                   aucfe<-performance(pred,"auc")
                   aucaddfe<-c(aucfe@y.values[[1]][1],aucaddfe)
                   
                   glmmodelfc<-glm(Bad45~MixedScore+fbflag+createin3m,data=train,family=binomial("logit"))
                   test$probfc<-predict(glmmodelfc,newdata=test,type="response")
                   pred<-prediction(1-test$probfc,1-test$Bad45)
                   aucfc<-performance(pred,"auc")
                   aucaddfc<-c(aucfc@y.values[[1]][1],aucaddfc)
                   
                   
                   glmmodelfa<-glm(Bad45~MixedScore+fbflag+activitysize,data=train,family=binomial("logit"))
                   test$probfa<-predict(glmmodelfa,newdata=test,type="response")
                   pred<-prediction(1-test$probfa,1-test$Bad45)
                   aucfa<-performance(pred,"auc")
                   aucaddfa<-c(aucfa@y.values[[1]][1],aucaddfa)
                   
                   glmmodelfj<-glm(Bad45~MixedScore+fbflag+jarosim,data=train,family=binomial("logit"))
                   test$probfj<-predict(glmmodelfj,newdata=test,type="response")
                   pred<-prediction(1-test$probfj,1-test$Bad45)
                   aucfj<-performance(pred,"auc")
                   aucaddfj<-c(aucfj@y.values[[1]][1],aucaddfj)
                   
                   glmmodelft<-glm(Bad45~MixedScore+fbflag+totalcomments,data=train,family=binomial("logit"))
                   test$probft<-predict(glmmodelft,newdata=test,type="response")
                   pred<-prediction(1-test$probft,1-test$Bad45)
                   aucft<-performance(pred,"auc")
                   aucaddft<-c(aucft@y.values[[1]][1],aucaddft)
                   
                   glmmodelfcu<-glm(Bad45~MixedScore+fbflag+commentusersize,data=train,family=binomial("logit"))
                   test$probfcu<-predict(glmmodelfcu,newdata=test,type="response")
                   pred<-prediction(1-test$probfcu,1-test$Bad45)
                   aucfcu<-performance(pred,"auc")
                   aucaddfcu<-c(aucfcu@y.values[[1]][1],aucaddfcu)
                   
                   glmmodelflr<-glm(Bad45~MixedScore+fbflag+linknumberratio,data=train,family=binomial("logit"))
                   test$probflr<-predict(glmmodelflr,newdata=test,type="response")
                   pred<-prediction(1-test$probflr,1-test$Bad45)
                   aucflr<-performance(pred,"auc")
                   aucaddflr<-c(aucflr@y.values[[1]][1],aucaddflr)
                   
                   glmmodelfl<-glm(Bad45~MixedScore+fbflag+linknumber,data=train,family=binomial("logit"))
                   test$probfl<-predict(glmmodelfl,newdata=test,type="response")
                   pred<-prediction(1-test$probfl,1-test$Bad45)
                   aucfl<-performance(pred,"auc")
                   aucaddfl<-c(aucfl@y.values[[1]][1],aucaddfl)
                   
                   
                   
                   glmmodelff<-glm(Bad45~MixedScore+fbflag+familysize,data=train,family=binomial("logit"))
                   test$probff<-predict(glmmodelff,newdata=test,type="response")
                   pred<-prediction(1-test$probff,1-test$Bad45)
                   aucff<-performance(pred,"auc")
                   aucaddff<-c(aucff@y.values[[1]][1],aucaddff)
                   
                   glmmodelfi<-glm(Bad45~MixedScore+fbflag+interestnumber,data=train,family=binomial("logit"))
                   test$probfi<-predict(glmmodelfi,newdata=test,type="response")
                   pred<-prediction(1-test$probfi,1-test$Bad45)
                   aucfi<-performance(pred,"auc")
                   aucaddfi<-c(aucfi@y.values[[1]][1],aucaddfi)
                   
                   glmmodelfa3m<-glm(Bad45~MixedScore+fbflag+activityin3m,data=train,family=binomial("logit"))
                   test$probfa3m<-predict(glmmodelfa3m,newdata=test,type="response")
                   pred<-prediction(1-test$probfa3m,1-test$Bad45)
                   aucfa3m<-performance(pred,"auc")
                   aucaddfa3m<-c(aucfa3m@y.values[[1]][1],aucaddfa3m)
                   
                   ####
                   glmmodele<-glm(Bad45~MixedScore+eventsize,data=train,family=binomial("logit"))
                   test$probe<-predict(glmmodele,newdata=test,type="response")
                   pred<-prediction(1-test$probe,1-test$Bad45)
                   auce<-performance(pred,"auc")
                   aucadde<-c(auce@y.values[[1]][1],aucadde)
                   
                   glmmodelc<-glm(Bad45~MixedScore+createin3m,data=train,family=binomial("logit"))
                   test$probc<-predict(glmmodelc,newdata=test,type="response")
                   pred<-prediction(1-test$probc,1-test$Bad45)
                   aucc<-performance(pred,"auc")
                   aucaddc<-c(aucc@y.values[[1]][1],aucaddc)
                   
                   
                   
                   glmmodela<-glm(Bad45~MixedScore+activitysize,data=train,family=binomial("logit"))
                   test$proba<-predict(glmmodela,newdata=test,type="response")
                   pred<-prediction(1-test$proba,1-test$Bad45)
                   auca<-performance(pred,"auc")
                   aucadda<-c(auca@y.values[[1]][1],aucadda)
                   
                   glmmodelj<-glm(Bad45~MixedScore+jarosim,data=train,family=binomial("logit"))
                   test$probj<-predict(glmmodelj,newdata=test,type="response")
                   pred<-prediction(1-test$probj,1-test$Bad45)
                   aucj<-performance(pred,"auc")
                   aucaddj<-c(aucj@y.values[[1]][1],aucaddj)
                   
                   glmmodelt<-glm(Bad45~MixedScore+totalcomments,data=train,family=binomial("logit"))
                   test$probt<-predict(glmmodelt,newdata=test,type="response")
                   pred<-prediction(1-test$probt,1-test$Bad45)
                   auct<-performance(pred,"auc")
                   aucaddt<-c(auct@y.values[[1]][1],aucaddt)
                   
                   
                   
                   
                   glmmodelcu<-glm(Bad45~MixedScore+commentusersize,data=train,family=binomial("logit"))
                   test$probcu<-predict(glmmodelcu,newdata=test,type="response")
                   pred<-prediction(1-test$probcu,1-test$Bad45)
                   auccu<-performance(pred,"auc")
                   aucaddcu<-c(auccu@y.values[[1]][1],aucaddcu)
                   
                   glmmodellr<-glm(Bad45~MixedScore+linknumberratio,data=train,family=binomial("logit"))
                   test$problr<-predict(glmmodellr,newdata=test,type="response")
                   pred<-prediction(1-test$problr,1-test$Bad45)
                   auclr<-performance(pred,"auc")
                   aucaddlr<-c(auclr@y.values[[1]][1],aucaddlr)
                   
                   glmmodell<-glm(Bad45~MixedScore+linknumber,data=train,family=binomial("logit"))
                   test$probl<-predict(glmmodell,newdata=test,type="response")
                   pred<-prediction(1-test$probl,1-test$Bad45)
                   aucl<-performance(pred,"auc")
                   aucaddl<-c(aucl@y.values[[1]][1],aucaddl)
                   
                   
                   
                   glmmodelf<-glm(Bad45~MixedScore+familysize,data=train,family=binomial("logit"))
                   test$probf<-predict(glmmodelf,newdata=test,type="response")
                   pred<-prediction(1-test$probf,1-test$Bad45)
                   aucf<-performance(pred,"auc")
                   aucaddf<-c(aucf@y.values[[1]][1],aucaddf)
                   
                   glmmodeli<-glm(Bad45~MixedScore+interestnumber,data=train,family=binomial("logit"))
                   test$probi<-predict(glmmodeli,newdata=test,type="response")
                   pred<-prediction(1-test$probi,1-test$Bad45)
                   auci<-performance(pred,"auc")
                   aucaddi<-c(auci@y.values[[1]][1],aucaddi)
                   
                   glmmodela3m<-glm(Bad45~MixedScore+activityin3m,data=train,family=binomial("logit"))
                   test$proba3m<-predict(glmmodela3m,newdata=test,type="response")
                   pred<-prediction(1-test$proba3m,1-test$Bad45)
                   auca3m<-performance(pred,"auc")
                   aucadda3m<-c(auca3m@y.values[[1]][1],aucadda3m)
                   
                   
                   ####
                   
                   
                   pred<-prediction(test$MixedScore/1000,1-test$Bad45)
                   auc2<-performance(pred,"auc")
                   aucmixscore<-c(auc2@y.values[[1]][1],aucmixscore)
                 }
                 remorefeautres<-data.frame(aucmixscore,aucaddfe,aucaddfc,aucaddfa,aucaddfj,aucaddft,aucaddfcu,aucaddflr,aucaddfl,aucaddff,aucaddfi,aucaddfa3m,aucadde,aucaddc,aucadda,aucaddj,aucaddt,aucaddcu,aucaddlr,aucaddl,aucaddf,aucaddi,aucadda3m)
                 
                
                 id<-1:1000
                 remorefeautres<-data.frame(id,remorefeautres)
                 
                 md<-melt(remorefeautres,id=c("id"))
                 par(cex.axis=0.8,col.axis="white")
                 boxplot(value~variable,data=md)
                 axis(1, at=1:23,labels=flabel, col.axis="red", las=2)  
                 
##############################################################
            control <- rfeControl(functions = rfFuncs, method = "boot", verbose = FALSE,returnResamp = "final", number = 50)
            profile<-rfe(train[,2:14],train$Bad45,sizes=sizes,rfeControl=control)
            profile
##############################################################
                 
                 
                 
                 for(s in seq(100,1000,by=100))
                 {
                   aucaddusersize<-c()
                   aucmixscore<-c()
                   aucaddflag<-c()
                   aucaddfe<-c()
                   aucaddfc<-c()	
                   aucaddfa<-c()
                   aucaddfj<-c()	
                   aucaddft<-c()				      
                   aucaddfcu<-c()
                   aucaddflr<-c()
                   aucaddfl<-c()
                   aucaddff<-c()
                   aucaddfi<-c()			   
                   aucaddfa3m<-c()
                   
                   aucadde<-c()
                   aucaddc<-c()	
                   aucadda<-c()
                   aucaddj<-c()	
                   aucaddt<-c()				      
                   aucaddcu<-c()
                   aucaddlr<-c()
                   aucaddl<-c()
                   aucaddf<-c()
                   aucaddi<-c()			   
                   aucadda3m<-c()
                   
                   for(i in seq(1,1000, by=1))#1000 times sample to see the performance
                   {
                     negtrainind<-sample(seq(1:157),100)
                     negtrain<-fbdatanewneg2[negtrainind,]
                     negtest<-fbdatanewneg2[-negtrainind,]
                     
                     postrainind<-sample(seq(1:2283),2000)
                     postrain<-fbdatanewpos2[postrainind,]
                     postest<-fbdatanewpos2[-postrainind,]
                     
                     nofbtrainind<-sample(seq(1:dim(fbdatanofb2)[1]),30000)
                     nofbtrain<-fbdatanofb2[nofbtrainind,]
                     nofbtest<-fbdatanofb2[-nofbtrainind,]
                     
                     train<-rbind(negtrain,postrain,nofbtrain)
                     test<-rbind(negtest,postest,nofbtest)
                     
                     #      train$fbflag<-ifelse(train$createin3m>=0,1,0)
                     #      test$fbflag<-ifelse(test$createin3m>=0,1,0)
                     train$fbflag<-ifelse(train$eventsize>=0,1,0)
                     test$fbflag<-ifelse(test$eventsize>=0,1,0)
                     
                     glmmodelflag<-glm(Bad45~MixedScore+fbflag,data=train,family=binomial("logit"))
                     test$probf<-predict(glmmodelflag,newdata=test,type="response")
                     pred<-prediction(1-test$probf,1-test$Bad45)
                     aucflag<-performance(pred,"auc")
                     aucaddflag<-c(aucflag@y.values[[1]][1],aucaddflag)
                     
                     # glmmodel<-glm(Bad45~MixedScore+commentusersize#createin3m,data=train,family=binomial("logit"))
                     glmmodelfe<-glm(Bad45~MixedScore+fbflag+eventsize,data=train,family=binomial("logit"))
                     test$probfe<-predict(glmmodelfe,newdata=test,type="response")
                     pred<-prediction(1-test$probfe,1-test$Bad45)
                     aucfe<-performance(pred,"auc")
                     aucaddfe<-c(aucfe@y.values[[1]][1],aucaddfe)
                     
                     glmmodelfc<-glm(Bad45~MixedScore+fbflag+createin3m,data=train,family=binomial("logit"))
                     test$probfc<-predict(glmmodelfc,newdata=test,type="response")
                     pred<-prediction(1-test$probfc,1-test$Bad45)
                     aucfc<-performance(pred,"auc")
                     aucaddfc<-c(aucfc@y.values[[1]][1],aucaddfc)
                     
                     
                     glmmodelfa<-glm(Bad45~MixedScore+fbflag+activitysize,data=train,family=binomial("logit"))
                     test$probfa<-predict(glmmodelfa,newdata=test,type="response")
                     pred<-prediction(1-test$probfa,1-test$Bad45)
                     aucfa<-performance(pred,"auc")
                     aucaddfa<-c(aucfa@y.values[[1]][1],aucaddfa)
                     
                     glmmodelfj<-glm(Bad45~MixedScore+fbflag+jarosim,data=train,family=binomial("logit"))
                     test$probfj<-predict(glmmodelfj,newdata=test,type="response")
                     pred<-prediction(1-test$probfj,1-test$Bad45)
                     aucfj<-performance(pred,"auc")
                     aucaddfj<-c(aucfj@y.values[[1]][1],aucaddfj)
                     
                     glmmodelft<-glm(Bad45~MixedScore+fbflag+totalcomments,data=train,family=binomial("logit"))
                     test$probft<-predict(glmmodelft,newdata=test,type="response")
                     pred<-prediction(1-test$probft,1-test$Bad45)
                     aucft<-performance(pred,"auc")
                     aucaddft<-c(aucft@y.values[[1]][1],aucaddft)
                     
                     glmmodelfcu<-glm(Bad45~MixedScore+fbflag+commentusersize,data=train,family=binomial("logit"))
                     test$probfcu<-predict(glmmodelfcu,newdata=test,type="response")
                     pred<-prediction(1-test$probfcu,1-test$Bad45)
                     aucfcu<-performance(pred,"auc")
                     aucaddfcu<-c(aucfcu@y.values[[1]][1],aucaddfcu)
                     
                     glmmodelflr<-glm(Bad45~MixedScore+fbflag+linknumberratio,data=train,family=binomial("logit"))
                     test$probflr<-predict(glmmodelflr,newdata=test,type="response")
                     pred<-prediction(1-test$probflr,1-test$Bad45)
                     aucflr<-performance(pred,"auc")
                     aucaddflr<-c(aucflr@y.values[[1]][1],aucaddflr)
                     
                     glmmodelfl<-glm(Bad45~MixedScore+fbflag+linknumber,data=train,family=binomial("logit"))
                     test$probfl<-predict(glmmodelfl,newdata=test,type="response")
                     pred<-prediction(1-test$probfl,1-test$Bad45)
                     aucfl<-performance(pred,"auc")
                     aucaddfl<-c(aucfl@y.values[[1]][1],aucaddfl)
                     
                     
                     
                     glmmodelff<-glm(Bad45~MixedScore+fbflag+familysize,data=train,family=binomial("logit"))
                     test$probff<-predict(glmmodelff,newdata=test,type="response")
                     pred<-prediction(1-test$probff,1-test$Bad45)
                     aucff<-performance(pred,"auc")
                     aucaddff<-c(aucff@y.values[[1]][1],aucaddff)
                     
                     glmmodelfi<-glm(Bad45~MixedScore+fbflag+interestnumber,data=train,family=binomial("logit"))
                     test$probfi<-predict(glmmodelfi,newdata=test,type="response")
                     pred<-prediction(1-test$probfi,1-test$Bad45)
                     aucfi<-performance(pred,"auc")
                     aucaddfi<-c(aucfi@y.values[[1]][1],aucaddfi)
                     
                     glmmodelfa3m<-glm(Bad45~MixedScore+fbflag+activityin3m,data=train,family=binomial("logit"))
                     test$probfa3m<-predict(glmmodelfa3m,newdata=test,type="response")
                     pred<-prediction(1-test$probfa3m,1-test$Bad45)
                     aucfa3m<-performance(pred,"auc")
                     aucaddfa3m<-c(aucfa3m@y.values[[1]][1],aucaddfa3m)
                     
                     ####
                     glmmodele<-glm(Bad45~MixedScore+eventsize,data=train,family=binomial("logit"))
                     test$probe<-predict(glmmodele,newdata=test,type="response")
                     pred<-prediction(1-test$probe,1-test$Bad45)
                     auce<-performance(pred,"auc")
                     aucadde<-c(auce@y.values[[1]][1],aucadde)
                     
                     glmmodelc<-glm(Bad45~MixedScore+createin3m,data=train,family=binomial("logit"))
                     test$probc<-predict(glmmodelc,newdata=test,type="response")
                     pred<-prediction(1-test$probc,1-test$Bad45)
                     aucc<-performance(pred,"auc")
                     aucaddc<-c(aucc@y.values[[1]][1],aucaddc)
                     
                     
                     
                     glmmodela<-glm(Bad45~MixedScore+activitysize,data=train,family=binomial("logit"))
                     test$proba<-predict(glmmodela,newdata=test,type="response")
                     pred<-prediction(1-test$proba,1-test$Bad45)
                     auca<-performance(pred,"auc")
                     aucadda<-c(auca@y.values[[1]][1],aucadda)
                     
                     glmmodelj<-glm(Bad45~MixedScore+jarosim,data=train,family=binomial("logit"))
                     test$probj<-predict(glmmodelj,newdata=test,type="response")
                     pred<-prediction(1-test$probj,1-test$Bad45)
                     aucj<-performance(pred,"auc")
                     aucaddj<-c(aucj@y.values[[1]][1],aucaddj)
                     
                     glmmodelt<-glm(Bad45~MixedScore+totalcomments,data=train,family=binomial("logit"))
                     test$probt<-predict(glmmodelt,newdata=test,type="response")
                     pred<-prediction(1-test$probt,1-test$Bad45)
                     auct<-performance(pred,"auc")
                     aucaddt<-c(auct@y.values[[1]][1],aucaddt)
                     
                     
                     
                     
                     glmmodelcu<-glm(Bad45~MixedScore+commentusersize,data=train,family=binomial("logit"))
                     test$probcu<-predict(glmmodelcu,newdata=test,type="response")
                     pred<-prediction(1-test$probcu,1-test$Bad45)
                     auccu<-performance(pred,"auc")
                     aucaddcu<-c(auccu@y.values[[1]][1],aucaddcu)
                     
                     glmmodellr<-glm(Bad45~MixedScore+linknumberratio,data=train,family=binomial("logit"))
                     test$problr<-predict(glmmodellr,newdata=test,type="response")
                     pred<-prediction(1-test$problr,1-test$Bad45)
                     auclr<-performance(pred,"auc")
                     aucaddlr<-c(auclr@y.values[[1]][1],aucaddlr)
                     
                     glmmodell<-glm(Bad45~MixedScore+linknumber,data=train,family=binomial("logit"))
                     test$probl<-predict(glmmodell,newdata=test,type="response")
                     pred<-prediction(1-test$probl,1-test$Bad45)
                     aucl<-performance(pred,"auc")
                     aucaddl<-c(aucl@y.values[[1]][1],aucaddl)
                     
                     
                     
                     glmmodelf<-glm(Bad45~MixedScore+familysize,data=train,family=binomial("logit"))
                     test$probf<-predict(glmmodelf,newdata=test,type="response")
                     pred<-prediction(1-test$probf,1-test$Bad45)
                     aucf<-performance(pred,"auc")
                     aucaddf<-c(aucf@y.values[[1]][1],aucaddf)
                     
                     glmmodeli<-glm(Bad45~MixedScore+interestnumber,data=train,family=binomial("logit"))
                     test$probi<-predict(glmmodeli,newdata=test,type="response")
                     pred<-prediction(1-test$probi,1-test$Bad45)
                     auci<-performance(pred,"auc")
                     aucaddi<-c(auci@y.values[[1]][1],aucaddi)
                     
                     glmmodela3m<-glm(Bad45~MixedScore+activityin3m,data=train,family=binomial("logit"))
                     test$proba3m<-predict(glmmodela3m,newdata=test,type="response")
                     pred<-prediction(1-test$proba3m,1-test$Bad45)
                     auca3m<-performance(pred,"auc")
                     aucadda3m<-c(auca3m@y.values[[1]][1],aucadda3m)
                     
                     
                     ####
                     
                     
                     pred<-prediction(test$MixedScore/1000,1-test$Bad45)
                     auc2<-performance(pred,"auc")
                     aucmixscore<-c(auc2@y.values[[1]][1],aucmixscore)
                   }
                   remorefeatures<-data.frame(aucmixscore,aucaddflag,aucaddfe,aucaddfc,aucaddfa,aucaddfj,aucaddft,aucaddfcu,aucaddflr,aucaddfl,aucaddff,aucaddfi,aucaddfa3m,aucadde,aucaddc,aucadda,aucaddj,aucaddt,aucaddcu,aucaddlr,aucaddl,aucaddf,aucaddi,aucadda3m)
                   
                   id<-1:1000
                   
                   remorefeatures<-data.frame(id,remorefeatures)
                   
                   colnames(remorefeatures)<-	c("id", "mixscore" ,"addflag","addfe"   , "addfc" ,   "addfa" ,   "addfj" ,  
                                                "addft" ,   "addfcu"  , "addflr"  , "addfl"  ,  "addff" ,   "addfi"  , 
                                                "addfa3m"  ,"adde"   ,  "addc" ,    "adda"  ,   "addj" ,    "addt" ,   
                                                "addcu"  ,  "addlr" ,   "addl"   ,  "addf"   ,  "addi"   ,  "adda3m" )
                   
                   flabel<-c("mixscore" ,"addflag","addfevent"   , "addfcomin3m" ,   "addfactivity" ,   "addfjaro" ,  
                             "addftotalcom" ,   "addfcomuser"  , "addfnormfriend"  , "addffriend"  ,  "addffamily" ,   "addfinterest"  , 
                             "addfactivityin3m"  ,"addevent"   ,  "addcomin3m" ,    "addactivity"  ,   "addjaro" ,    "addtotalcom" ,   
                             "addcomuser"  ,  "addnormfriend" ,   "addfriend"   ,  "addfamily"   ,  "addinterest"   ,  "addactivityin3m" )
                   
                   
                   md<-melt(remorefeatures,id=c("id"))
                   par(cex.axis=0.8,col.axis="black")
                   boxplot(value~variable,data=md,xaxt='n')
                   axis(1, at=1:24,labels=flabel, col.axis="red", las=2)
                   
                   
                   ####################################################################################
                   aucmixscore<-c()
                   aucadde<-c()
                   aucaddc<-c()	
                   aucadda<-c()
                   aucaddj<-c()	
                   aucaddt<-c()				      
                   aucaddcu<-c()
                   aucaddlr<-c()
                   aucaddl<-c()
                   aucaddf<-c()
                   aucaddi<-c()			   
                   aucadda3m<-c()
                   
                   for(i in seq(1,1000, by=1))#1000 times sample to see the performance
                   {
                     negtrainind<-sample(seq(1:157),100)
                     negtrain<-fbdatanewneg2[negtrainind,]
                     negtest<-fbdatanewneg2[-negtrainind,]
                     
                     postrainind<-sample(seq(1:2283),2000)
                     postrain<-fbdatanewpos2[postrainind,]
                     postest<-fbdatanewpos2[-postrainind,]
                     
                     #     nofbtrainind<-sample(seq(1:dim(fbdatanofb2)[1]),4000)
                     #     nofbtrain<-fbdatanofb2[nofbtrainind,]
                     #     nofbtest<-fbdatanofb2[-nofbtrainind,]
                     
                     train<-rbind(negtrain,postrain)
                     test<-rbind(negtest,postest)
                     
                     #      train$fbflag<-ifelse(train$createin3m>=0,1,0)
                     #      test$fbflag<-ifelse(test$createin3m>=0,1,0)
                     #     train$fbflag<-ifelse(train$eventsize>=0,1,0)
                     #     test$fbflag<-ifelse(test$eventsize>=0,1,0)
                     
                     #      glmmodelflag<-glm(Bad45~MixedScore+fbflag,data=train,family=binomial("logit"))
                     #      test$probf<-predict(glmmodelflag,newdata=test,type="response")
                     #      pred<-prediction(1-test$probf,1-test$Bad45)
                     #      aucflag<-performance(pred,"auc")
                     #      aucaddflag<-c(aucflag@y.values[[1]][1],aucaddflag)
                     
                     
                     
                     ####
                     glmmodele<-glm(Bad45~MixedScore+eventsize,data=train,family=binomial("logit"))
                     test$probe<-predict(glmmodele,newdata=test,type="response")
                     pred<-prediction(1-test$probe,1-test$Bad45)
                     auce<-performance(pred,"auc")
                     aucadde<-c(auce@y.values[[1]][1],aucadde)
                     
                     glmmodelc<-glm(Bad45~MixedScore+createin3m,data=train,family=binomial("logit"))
                     test$probc<-predict(glmmodelc,newdata=test,type="response")
                     pred<-prediction(1-test$probc,1-test$Bad45)
                     aucc<-performance(pred,"auc")
                     aucaddc<-c(aucc@y.values[[1]][1],aucaddc)
                     
                     
                     
                     glmmodela<-glm(Bad45~MixedScore+activitysize,data=train,family=binomial("logit"))
                     test$proba<-predict(glmmodela,newdata=test,type="response")
                     pred<-prediction(1-test$proba,1-test$Bad45)
                     auca<-performance(pred,"auc")
                     aucadda<-c(auca@y.values[[1]][1],aucadda)
                     
                     glmmodelj<-glm(Bad45~MixedScore+jarosim,data=train,family=binomial("logit"))
                     test$probj<-predict(glmmodelj,newdata=test,type="response")
                     pred<-prediction(1-test$probj,1-test$Bad45)
                     aucj<-performance(pred,"auc")
                     aucaddj<-c(aucj@y.values[[1]][1],aucaddj)
                     
                     glmmodelt<-glm(Bad45~MixedScore+totalcomments,data=train,family=binomial("logit"))
                     test$probt<-predict(glmmodelt,newdata=test,type="response")
                     pred<-prediction(1-test$probt,1-test$Bad45)
                     auct<-performance(pred,"auc")
                     aucaddt<-c(auct@y.values[[1]][1],aucaddt)
                     
                     
                     
                     
                     glmmodelcu<-glm(Bad45~MixedScore+commentusersize,data=train,family=binomial("logit"))
                     test$probcu<-predict(glmmodelcu,newdata=test,type="response")
                     pred<-prediction(1-test$probcu,1-test$Bad45)
                     auccu<-performance(pred,"auc")
                     aucaddcu<-c(auccu@y.values[[1]][1],aucaddcu)
                     
                     glmmodellr<-glm(Bad45~MixedScore+linknumberratio,data=train,family=binomial("logit"))
                     test$problr<-predict(glmmodellr,newdata=test,type="response")
                     pred<-prediction(1-test$problr,1-test$Bad45)
                     auclr<-performance(pred,"auc")
                     aucaddlr<-c(auclr@y.values[[1]][1],aucaddlr)
                     
                     glmmodell<-glm(Bad45~MixedScore+linknumber,data=train,family=binomial("logit"))
                     test$probl<-predict(glmmodell,newdata=test,type="response")
                     pred<-prediction(1-test$probl,1-test$Bad45)
                     aucl<-performance(pred,"auc")
                     aucaddl<-c(aucl@y.values[[1]][1],aucaddl)
                     
                     
                     
                     glmmodelf<-glm(Bad45~MixedScore+familysize,data=train,family=binomial("logit"))
                     test$probf<-predict(glmmodelf,newdata=test,type="response")
                     pred<-prediction(1-test$probf,1-test$Bad45)
                     aucf<-performance(pred,"auc")
                     aucaddf<-c(aucf@y.values[[1]][1],aucaddf)
                     
                     glmmodeli<-glm(Bad45~MixedScore+interestnumber,data=train,family=binomial("logit"))
                     test$probi<-predict(glmmodeli,newdata=test,type="response")
                     pred<-prediction(1-test$probi,1-test$Bad45)
                     auci<-performance(pred,"auc")
                     aucaddi<-c(auci@y.values[[1]][1],aucaddi)
                     
                     glmmodela3m<-glm(Bad45~MixedScore+activityin3m,data=train,family=binomial("logit"))
                     test$proba3m<-predict(glmmodela3m,newdata=test,type="response")
                     pred<-prediction(1-test$proba3m,1-test$Bad45)
                     auca3m<-performance(pred,"auc")
                     aucadda3m<-c(auca3m@y.values[[1]][1],aucadda3m)
                     
                     
                     ####
                     
                     
                     pred<-prediction(test$MixedScore/1000,1-test$Bad45)
                     auc2<-performance(pred,"auc")
                     aucmixscore<-c(auc2@y.values[[1]][1],aucmixscore)
                   }
                   remorefeatures<-data.frame(aucmixscore,aucadde,aucaddc,aucadda,aucaddj,aucaddt,aucaddcu,aucaddlr,aucaddl,aucaddf,aucaddi,aucadda3m)
                   
                   id<-1:1000
                   
                   remorefeatures<-data.frame(id,remorefeatures)
                   
                   colnames(remorefeatures)<-	c("id", "mixscore"  ,"adde"   ,  "addc" ,    "adda"  ,   "addj" ,    "addt" ,   
                                                "addcu"  ,  "addlr" ,   "addl"   ,  "addf"   ,  "addi"   ,  "adda3m" )
                   
                   flabel<-c("mixscore" ,"addevent"   ,  "addcomin3m" ,    "addactivity"  ,   "addjaro" ,    "addtotalcom" ,   
                             "addcomuser"  ,  "addnormfriend" ,   "addfriend"   ,  "addfamily"   ,  "addinterest"   ,  "addactivityin3m" )
                   
                   
                   md<-melt(remorefeatures,id=c("id")) 
                   par(cex.axis=0.8,col.axis="black")
                   boxplot(value~variable,data=md,xaxt='n')
                   axis(1, at=1:24,labels=flabel, col.axis="red", las=2)			 
                 
                 