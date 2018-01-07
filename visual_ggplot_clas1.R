
##########################################################################
#-------------------------------------------------------------------------
#--------------       AESTHETIC MAPPING     ------------------------------
#-------------------------------------------------------------------------
##########################################################################


#Clase 1 de visualizacion, intensión manejar ggplot


library(ggplot2)
data(mpg)
str(mpg)
ggplot(data=mpg, aes(x=hwy))+geom_histogram()
library(gcookbook)

#cambio el numero de bins
ggplot(data=mpg, aes(x=hwy))+geom_histogram(bins=10)
ggplot(data=mpg, aes(x=hwy))+geom_histogram(bins=50)
ggplot(data=mpg, aes(x=hwy))+geom_histogram(binwidth=.5)
ggplot(data=mpg, aes(x=hwy))+geom_histogram(binwidth=.1)

#vamos a cambiar la marca
a<-ggplot(data=mpg, aes(x=hwy))
a+greom_area()
a+geom_freqpoly()
a+geom_area(stat='bin')

#si lo que queremos es analizar una variable categorica, usamos un barchar
b<-ggplot(mpg,aes(manufacturer))
b+geom_bar()

b<-ggplot(mpg,aes(class))
b+geom_bar()

#intentemoslo con dos varibales continuas
c<-ggplot(mpg,aes(displ,hwy))
c+geom_point()
c+geom_point()+geom_smooth()
c+geom_point()+geom_smooth(method='lm')
c+geom_point(aes(color=class))
c+geom_point(aes(color=as.factor(cyl)))
c+geom_point(aes(shape=as.factor(cyl)))
c+geom_point(aes(size=cyl))
c+geom_point(aes(color=displ<5))

#categorico contra numerico
ggplot(mpg,aes(drv,hwy))+geom_jitter(width=0.2)
ggplot(mpg,aes(drv,hwy))+geom_violin()
#boxplot
ggplot(mpg,aes(drv,hwy,fill=drv))+geom_boxplot()

ggplot(mpg,aes(class,hwy))+geom_jitter(width=0.2)
ggplot(mpg,aes(class,hwy,fill=class))+geom_violin()
ggplot(mpg,aes(class,hwy))+geom_boxplot()
#cambiar eje de coordenadas
ggplot(mpg,aes(class,hwy))+geom_boxplot()+coord_flip()

ggplot(mpg,aes(displ,hwy))+geom_point()+facet_wrap(~class)

#-------------------------------------------------------------------------
#--------------       AESTHETIC MAPPING     ------------------------------
#-------------------------------------------------------------------------




#-------------------------------------------------------------------------
#--------------       Geometric Objects     ------------------------------
#-------------------------------------------------------------------------

# points: geom_point
# lines: geom_line
# area: geom_area
# Boxplots: geom_boxplots


#-------------------------------------------------------------------------
#---------------------       Ejericio 1     ------------------------------
#-------------------------------------------------------------------------

library(babynames)
celeste<-dplyr::filter(babynames,name=="Celeste")
ggplot(celeste,aes(year,n))+geom_point()
ggplot(celeste,aes(year,n))+geom_line()
ggplot(celeste,aes(year,n,color=sex))+geom_point()



##########################################################################
#-------------------------------------------------------------------------
#--------------      OTher uses     ------------------------------
#-------------------------------------------------------------------------
##########################################################################


#Clase 1 de visualizacion, intensión manejar ggplot


library(ggplot2)
library(gcookbook)
library(ggthemes)
library(gridExtra)

mydata=uspopage
str(mydata)
summary(mydata)
e<-ggplot(mydata, aes(Year,Thousands,color=AgeGroup))+geom_line()

str(worldpop)
ggplot(worldpop, aes(Year,Population))+geom_line()

d<-ggplot(worldpop, aes(Year,log(Population)))+geom_line()

ggplot(worldpop, aes(Year,log(Population)))+geom_line()+xlim(1000,2500)+xlab("Año, cero...")+ylab("Log(poblacion")

ggplot(mpg,aes(drv,hwy))+geom_jitter(width=0.5)+xlim("f","r")+ylim(20,30)

ggplot(mpg,aes(displ,hwy))+geom_point(aes(color=class))+scale_x_continuous(breaks=c(2,6))+ggtitle("La hostia")+theme(plot.title = element_text(hjust=0.5))

p<-ggplot(mpg,aes(displ,hwy))+geom_point()
p+annotate("text",x=c(2,5),y=c(40,25),label=c("hostia1","Patxi, Maquina"),color="violet",size=16,angle=23,fontface="bold")

ggplot(economics,aes(date,unemploy))+geom_point()

ggplot(economics)+geom_point(aes(date,unemploy,color="red"))+geom_point(aes(date,pop,color="blue"))+geom_point(aes(date,unemploy/pop,color="grey"))

p <- ggplot(economics, aes(x = date))
p <- p + geom_line(aes(y = pop, colour = "pop"))
p <- p + geom_line(aes(y = unemploy, colour = "unemploy"))
p <- p + scale_y_continuous(sec.axis = sec_axis(~.*2, name = "unemploy/pop"))
p

#con esto salvo la última imagen como .png
ggsave("plot1.png",width=6,heigt=5)

#uso librería ggthemes
c<-ggplot(economics,aes(date,unemploy))+geom_point()+theme_fivethirtyeight()

#varios gráficos en un mismo gráfico
grid.arrange(c,d,e,p,ncol=2,nrow=2)
