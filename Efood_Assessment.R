##Part 1##

###Q1#

orders<-read.csv(file.choose(),fileEncoding = "UTF-8")
a<-merge(aggregate(amount~city,sum,data=orders),aggregate(order_id~city,length,data=orders),"city")
a<-merge(a,aggregate(user_id~city,FUN=function(x)k=length(unique(x)),data=orders),"city")
a$efood_basket<-a$amount/a$order_id
a$efood_freq<-a$order_id/a$user_id

b<-merge(aggregate(amount~city+cuisine,sum,data=orders),aggregate(order_id~city+cuisine,length,data=orders),c("city","cuisine"))
b<-merge(b,aggregate(user_id~city+cuisine,FUN=function(x)k=length(unique(x)),data=orders),c("city","cuisine"))
b<-b[b$cuisine=="Breakfast",]
b$breakfast_basket<-b$amount/b$order_id
b$breakfast_freq<-b$order_id/b$user_id

c<-aggregate(order_id~city+cuisine+user_id,length,data=orders)
c$users_greater_3<-ifelse(c$order_id>3,1,0)
d<-merge(aggregate(users_greater_3~city+cuisine,sum,data=c),aggregate(user_id~city+cuisine,FUN=function(x)k=length(unique(x)),data=orders),c("city","cuisine"))
d<-d[d$cuisine=="Breakfast",]
d$breakfast_user3freq_perc<-d$users_greater_3/d$user_id

e<-aggregate(order_id~city+user_id,length,data=orders)
e$users_greater_3<-ifelse(e$order_id>3,1,0)
f<-merge(aggregate(users_greater_3~city,sum,data=e),aggregate(user_id~city,FUN=function(x)k=length(unique(x)),data=orders),c("city"))
f$efood_user3freq_perc<-f$users_greater_3/f$user_id

Finala<-merge(a,b,"city")[,c("city","efood_basket","efood_freq","breakfast_basket","breakfast_freq")]
Finalb<-merge(f,d,"city")
Final<-merge(Finala,Finalb,"city")[,c("city","efood_basket","efood_freq","breakfast_basket","breakfast_freq","breakfast_user3freq_perc","efood_user3freq_perc")]

##Final is the final table as indicated in the pdf##

##Q2##

g<-aggregate(order_id~city+user_id,length,data=orders)
g<-g[order(g$city,-g$order_id),]
r<-Reduce(rbind,by(g,g["city"],head,n=10))
paste(round(100*sum(r$order_id)/sum(g$order_id),2),"%")

##Part 2##

orders$order_timestamp<-as.Date(orders$order_timestamp)
orders$Month<-as.numeric(format(orders$order_timestamp,'%m'))
modeldata<-merge(aggregate(order_id~user_id,length,data = orders),aggregate(amount~user_id,sum,data = orders),"user_id")
names(modeldata)[2]<-"orders_per_month"
typeorder<-aggregate(order_id~user_id+cuisine,length,data = orders)
users<-reshape(typeorder,idvar = "user_id",v.names = "order_id",timevar = "cuisine",direction = "wide")
users[is.na(users)]<-0
modeldata<-merge(modeldata,users,"user_id")
modeldata$user_id<-as.factor(modeldata$user_id)
kmdata<-scale(modeldata[,-1])
k<-2:10
clusters<-data.frame(matrix(nrow=dim(kmdata)[1],ncol=9))
names(clusters)<-paste("cl_",k)
for(i in k){
km <- kmeans(kmdata, centers = i, nstart=25)
clusters[,i-1]<-km$cluster}
modeldata<-cbind(modeldata,clusters)

segments<-list()
for (i in 1:9) {
                segments[[i]]<-merge(aggregate(modeldata[,2:7],list(modeldata[,7+i]),mean),aggregate(modeldata$user_id,list(modeldata[,7+i]),length),"Group.1")
                names(segments[[i]])[1]<-"clusters"
                names(segments[[i]])[8]<-"Users"
}
total<-apply(modeldata[,2:7],2,mean)

library(writexl)
write_xlsx(segments,"segments.xlsx")

##segments list has some statistics per segment for all different segments and have been saved to an excel##

