#segunda entrega del curso de visualizacion


library(ggplot2)
library(dplyr)

trans <- read_csv("~/Desktop/MBD.comillas/visualizacion/productos_boris/2da_entrega/madrid_transactions.csv")
str(trans)
summary(trans)


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++     Individual Variables' Analysis     ++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#Transactions vs country
trans200<-trans %>% group_by(customer_country) %>% mutate(num=n()) %>% filter(n()>200)  
trans200$customer_country <-factor(trans200$customer_country)
trans200 <- transform(trans200, customer_country=reorder(customer_country, num) ) 
bp<-ggplot(trans200,aes(x=customer_country))+geom_bar()+xlab("Countries")+ylab("Num. Transacciones")+
  ggtitle("Frec. Transacciones según País")+theme(plot.title = element_text(size = 14, face = "bold"))

bp

#Montos gastados
ha<-ggplot(trans, aes(x=log10(amount)))+geom_histogram(bins=20)+xlim(0,4)+xlab("log10(amount)")+ylab("Frecuencia")+
  ggtitle("Histograma del log10 de las montos")+theme(plot.title = element_text(size = 14, face = "bold"))
ha

#frecuencia según la hora
transh<-trans %>% group_by(hour) %>% mutate(num=n()) 
transh$hour <-factor(transh$hour)
th<-ggplot(transh,aes(x=hour))+geom_bar()+xlab("Hora")+ylab("Num. Transacciones")+
  ggtitle("Frec. Transacciones según hora")+theme(plot.title = element_text(size = 14, face = "bold"))
th

#frecuencia según la categoria
transc<-trans %>% group_by(category) %>% mutate(total=sum(amount)) %>% filter(total>5000)
transc$amount<-transc$amount/1000
transc$category <-factor(transc$category)
transc <- transform(transc, category=reorder(category, total) ) 
fc<-ggplot(transc,aes(category,amount))+geom_col()+xlab("Categoría")+ylab("Monto(miles)")+
  ggtitle("Monto total por Categoría")+
  theme(plot.title = element_text(size = 14, face = "bold"), axis.text.x  = element_text(size = 8,angle=90))
fc

grid.arrange(bp,ha,th,fc,ncol=2,nrow=2)

sum(trans$amount)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#++++++++++++++++++++++++++++     Crossed Variables Analysis     ++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#Try to figure out a relation between hour and category and amount spent

#total amount of money spent by hour and day
trans4<-trans %>% group_by(hour,weekday) %>% mutate(total=sum(amount)) 
dh<-ggplot(trans4,aes(hour,total,color=weekday))+geom_line()+xlab("Hora")+ylab("Monto")+
  ggtitle("Monto total por Día y hora")+
  theme(plot.title = element_text(size = 14, face = "bold"))
dh

#total amount of money spent by category, hour and day
trans5<-trans %>% group_by(hour,category,weekday) %>% mutate(total=sum(amount)) %>% filter(total>5001) 
cdh<-ggplot(trans5,aes(hour,total,color=category,linetype=weekday))+geom_line()+xlab("Hora")+ylab("Monto")+
  ggtitle("Monto total por Categoría, Día y hora")+
  theme(plot.title = element_text(size = 14, face = "bold"))
cdh

grid.arrange(dh,cdh,ncol=2,nrow=1)




#figuring out the relationship between category



#total amount of money spent by category and hour
trans6<-trans %>% group_by(category) %>% filter(sum(amount)>10000)
trans6<-trans6 %>% group_by(customer_country) %>% mutate(total=sum(amount)) %>% filter(total>20000) 
trans6$customer_country <-factor(trans6$customer_country)
trans6 <- transform(trans6, customer_country=reorder(customer_country, total) )
cc<-ggplot(trans6,aes(customer_country,amount,fill=category))+geom_col()+xlab("País")+ylab("Monto")+
  ggtitle("Monto total por Categoría y País")+
  theme(plot.title = element_text(size = 14, face = "bold"), axis.text.x  = element_text(size = 8,angle=90))
cc

trans7<-trans %>% group_by(customer_country) %>% mutate(total=sum(amount)) %>% filter(total>20000) 
trans7$customer_country <-factor(trans7$customer_country)
trans7 <- transform(trans7, customer_country=reorder(customer_country, total) )
cbp<-ggplot(trans7,aes(customer_country,amount))+geom_boxplot()+xlab("País")+ylab("Monto promedio")+
  ggtitle("Monto Promedio por País")+ylim(0,500)+
  theme(plot.title = element_text(size = 14, face = "bold"), axis.text.x  = element_text(size = 8,angle=90))
cbp

grid.arrange(cc,cbp,ncol=2,nrow=1)



#total amount of money spent by category and hour
trans3<-trans %>% group_by(hour,category) %>% mutate(total=sum(amount)) %>% filter(total>5001) 
ch<-ggplot(trans3,aes(hour,total,color=category))+geom_line()+xlab("Hora")+ylab("Monto")+
  ggtitle("Monto total por Categoría y hora")+
  theme(plot.title = element_text(size = 14, face = "bold"))
ch

#times transaction for category and hour are performed
trans2<-trans %>% group_by(hour,category) %>% mutate(num=n()) %>% filter(num>30) 
ggplot(trans2,aes(hour,num,color=category))+geom_line()



# Croos country against the others

ca<- ggplot(trans200,aes(customer_country,amount))+geom_boxplot()+ylim(0,200)

#category against amount
ggplot(transc,aes(category,amount))+geom_boxplot()+ylim(0,500)+xlab("Categoría")+ylab("Num. Transacciones")+
  ggtitle("Frec. Transacciones según Categoría")+
  theme(plot.title = element_text(size = 14, face = "bold"), axis.text.x  = element_text(size = 8,angle=90))

ggplot(transc,aes(category,amount,color=daytime))+geom_jitter(width=0.2)

ggplot(trans200,aes(customer_country,amount,color=daytime))+geom_jitter(width=0.2)+ylim(0,500)

trans1<-trans %>% filter(category=="Fashion & Shoes")
ggplot(trans1,aes(hour,amount))+geom_jitter(width=0.2)+ylim(0,1000)

ggplot(transc,aes(hour,amount))+geom_jitter(width=0.2)+facet_wrap(~category)+ylim(0,700)




ggplot(trans,aes(x=customer_country))+geom_bar()

ggplot(trans,aes(x=customer_country))+geom_bar()

trans200<-trans %>%
  group_by(customer_country) %>%
  mutate(num=n()) %>%
  filter(num>200) %>%
  arrange(num)

trans200<-trans200 %>%
  group_by(customer_country)

ggplot(trans200,aes(x=customer_country))+geom_bar()
