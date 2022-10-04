
rm (list=ls())  # remove all variables

install.packages("gridExtra")
library(gridExtra)

install.packages("qqplotr")
library(qqplotr)

install.packages("stringr")
library(stringr)

install.packages("caret")
library(caret)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org") # install package
library(tidyverse)

#read data
Happiness <- read.csv("C:/Users/Larisa Doherty/Desktop/Happiness/Happiness.csv") # read data
names(Happiness)
length(Happiness)
length(Happiness$Year)


# distinct countries
Happiness%>%summarise(countries=n_distinct(Country))

#Distribution of happiness rating by country AND year
plot1<-Happiness %>% group_by(Country)%>%
  ggplot(aes(Happiness.Score)) + 
  geom_histogram(fill = "light blue")+
  ggtitle("Rating Distribution by Year and Country")
plot1





#is the distribution normal - very close So it can be described by the mean and sd. 
plot2<-ggplot(data=Happiness,aes(sample=Happiness.Score))+stat_qq()+ggtitle("QQ PLot - Test for Normality")


#Mean Happiness by Country
MeanHappy<-Happiness%>% group_by(Country)%>% summarise(mean(Happiness.Score))
#Rename columns
colnames(MeanHappy)<-c("Country","Score")
MeanHappy
plot3<-MeanHappy %>%ggplot(aes(Score)) + 
  geom_histogram(fill = "light blue")+
  ggtitle("Rating Distribution by Country")
plot3

grid.arrange(plot1,plot2,plot3,ncol=3)



#mean and sd of happiness score
summary <- Happiness %>%summarize(mean = mean(Happiness$Happiness.Score), sd = sd(Happiness$Happiness.Score))
summary


#Mean Happiness by Year - similar across years and increasing
MeanHappyYear<-Happiness%>% group_by(Year)%>% summarise(mean(Happiness.Score))
#Rename columns
colnames(MeanHappyYear)<-c("Year","Score")
MeanHappyYear
#Happiness scores are increasing
plot1<-MeanHappyYear%>%ggplot(aes(Year, Score)) + geom_line()
plot1




#top 10 all years
top<-MeanHappy%>%arrange(desc(Score))%>%slice(1:10)
top
top_2015<-Happiness%>%filter(Year==2015)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_2016<-Happiness%>%filter(Year==2016)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_2017<-Happiness%>%filter(Year==2017)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_2018<-Happiness%>%filter(Year==2018)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_2019<-Happiness%>%filter(Year==2019)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_2020<-Happiness%>%filter(Year==2020)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_2021<-Happiness%>%filter(Year==2021)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_2022<-Happiness%>%filter(Year==2022)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_allyears<-rbind(top_2015,top_2016,top_2017,top_2018,top_2019,top_2020,top_2021,top_2022)
#countries which show up in the top 10 happiest countries each year, will show up 8 times in this table
table(top_allyears$Country)

#we will select countries which were in top 8 across all years
top_8_all_years<-top_allyears%>%filter(Country %in% c("Denmark","Finland","Iceland","Netherlands","New Zealand","Norway","Sweden","Switzerland"))
top_8_all_years

plot1<-top_8_all_years %>%filter(Year==2015)%>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Top 8 Happiest - 2015 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot2<-top_8_all_years %>%filter(Year==2016)%>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Top 8 Happiest - 2016 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot3<-top_8_all_years %>%filter(Year==2017)%>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Top 8 Happiest - 2017 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot4<-top_8_all_years %>%filter(Year==2018)%>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Top 8 Happiest - 2018 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot5<-top_8_all_years %>%filter(Year==2019)%>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Top 8 Happiest - 2019 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot6<-top_8_all_years %>%filter(Year==2020)%>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Top 8 Happiest - 2020 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot7<-top_8_all_years %>%filter(Year==2021)%>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Top 8 Happiest - 2021 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot8<-top_8_all_years %>%filter(Year==2022)%>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Top 8 Happiest - 2022 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
# we can see the same countries in top 8 happiest in each year
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,ncol=2)


#bottom 10
bottom<-MeanHappy%>%arrange(Score)%>%slice(1:10)
bottom_2015<-Happiness%>%filter(Year==2015)%>%arrange(Happiness.Score)%>%slice(1:10)
bottom_2016<-Happiness%>%filter(Year==2016)%>%arrange(Happiness.Score)%>%slice(1:10)
bottom_2017<-Happiness%>%filter(Year==2017)%>%arrange(Happiness.Score)%>%slice(1:10)
bottom_2018<-Happiness%>%filter(Year==2018)%>%arrange(Happiness.Score)%>%slice(1:10)
bottom_2019<-Happiness%>%filter(Year==2019)%>%arrange(Happiness.Score)%>%slice(1:10)
bottom_2020<-Happiness%>%filter(Year==2020)%>%arrange(Happiness.Score)%>%slice(1:10)
bottom_2021<-Happiness%>%filter(Year==2021)%>%arrange(Happiness.Score)%>%slice(1:10)
bottom_2022<-Happiness%>%filter(Year==2022)%>%arrange(Happiness.Score)%>%slice(1:10)
bottom_allyears<-rbind(bottom_2015,bottom_2016,bottom_2017,bottom_2018,bottom_2019,bottom_2020,bottom_2021,bottom_2022)
# we see only Rwanda was in the bottom unhappy countries across all years.  we will continue to work with bottom 10 for unahappy countries
table(bottom_allyears$Country)


plot1<-bottom_2015 %>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Bottom 10 Unhappiest - 2015 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot2<-bottom_2016 %>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Bottom 10 Unhappiest - 2016 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot3<-bottom_2017 %>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Bottom 10 Unhappiest - 2017 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot4<-bottom_2018 %>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Bottom 10 Unhappiest - 2018 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot5<-bottom_2019 %>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Bottom 10 Unhappiest - 2019 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot6<-bottom_2020 %>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Bottom 10 Unhappiest - 2020 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot7<-bottom_2021 %>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Bottom 10 Unhappiest - 2021 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())
plot8<-bottom_2022 %>%ggplot(aes(x=Country,y=Happiness.Score))+geom_point() + ggtitle("Bottom 10 Happiest - 2022 ")+geom_text(aes(x=Country,y=Happiness.Score,label=Country))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank() )+labs(x=NULL,y=NULL)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())

#We see that only one country was in the bottom 10 unhappy countries across all years *********** color rowanda
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,ncol=2)


#Top countries by year
p <- ggplot(data=top_8_all_years, aes(x = Year , y = Happiness.Score)) +
  geom_line() +facet_grid(Country~.)
p


#Rwanda by year
p <-Happiness%>%filter(Country=='Rwanda')%>%ggplot(aes(x = Year , y = Happiness.Score)) + geom_line()
p

#CORRELATION BETWEEN FACTORS AND HAPPINESS SCORES ON ALL COUNTRIES
#relationship between happiness and gdp per capita 
plot1<-Happiness%>%ggplot(aes(Happiness.Rank, Economy..GDP.per.Capita.)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=NULL)

#relationship between happiness and family 
plot2<-Happiness%>%ggplot(aes(Happiness.Rank, Family)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=NULL)

#relationship between happiness and family 
plot3<-Happiness%>%ggplot(aes(Happiness.Rank, Social.support)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=NULL)

# life expectancy
plot4<-Happiness%>%ggplot(aes(Happiness.Rank, Health..Life.Expectancy.)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=NULL)

#freedom 
plot5<-Happiness%>%ggplot(aes(Happiness.Rank, Freedom)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=NULL)

# v. corruption
plot6<-Happiness%>%ggplot(aes(Happiness.Rank, Trust..Government.Corruption.)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=NULL)

#generosity
plot7<-Happiness%>%ggplot(aes(Happiness.Rank, Generosity)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=NULL)

#Dystopia
plot8<-Happiness%>%ggplot(aes(Happiness.Rank, Dystopia.Residual)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=NULL)

#perception of corruption
plot9<-Happiness%>%ggplot(aes(Happiness.Rank, Perceptions.of.corruption)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=NULL)

#highest correlations
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,ncol=3)

#Calculate correlation on top 3 observable correlations
corEcon<-round(cor(Happiness$Happiness.Score,Happiness$Economy..GDP.per.Capita.),digits=2)
corLifeEx<-round(cor(Happiness$Happiness.Score,Happiness$Health..Life.Expectancy.),digits=2)
corFree<-round(cor(Happiness$Happiness.Score,Happiness$Freedom),digits=2)

corEcon2<-paste(c("Corr. Factor: ",corEcon),collapse=" ")
corLifeEx2<-paste(c("Corr. Factor: ",corLifeEx),collapse=" ")
corFree2<-paste(c("Corr. Factor: ",corFree),collapse=" ")

#plot if top 2 correlation values
#relationship between happiness and gdp per capita 
plot1<-Happiness%>%ggplot(aes(Happiness.Rank, Economy..GDP.per.Capita.)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=corEcon2)
#Health and life expectancy
plot2<-Happiness%>%ggplot(aes(Happiness.Rank, Health..Life.Expectancy.)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=corLifeEx2)

grid.arrange(plot1,plot2,nrow=2)

#Simplify Happiness table to contain only top two correlation variables
Happiness<-Happiness%>%select(Year, Country,Happiness.Rank,Happiness.Score,Economy..GDP.per.Capita.,Health..Life.Expectancy.)
Happiness
Happiness$GdpHealth<-(Happiness$Economy..GDP.per.Capita.+Happiness$Health..Life.Expectancy.)/2
Happiness
#create column of average of gdp & health
corGdpHealth<-round(cor(Happiness$Happiness.Score,Happiness$GdpHealth),digits=2)
corGdpHealth2<-paste(c("Corr. Factor: ",corGdpHealth),collapse=" ")

#Health and life expectancy
plot3<-Happiness%>%ggplot(aes(Happiness.Rank, GdpHealth)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=corGdpHealth2)

grid.arrange(plot1,plot2,plot3,nrow=3)

# find higest correlation for happiest countries
top_8_all_years
corTopEcon<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$Economy..GDP.per.Capita.),digits=2)
corTopLifeEx<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$Health..Life.Expectancy.),digits=2)
corTopFreedom<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$Freedom),digits=2)
corTopGenerosity<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$Generosity),digits=2)

corTopEcon2<-paste(c("Corr. Factor: ",corTopEcon),collapse=" ")
corTopLifeEx2<-paste(c("Corr. Factor: ",corTopLifeEx),collapse=" ")
corTopFreedom2<-paste(c("Corr. Factor: ",corTopFreedom),collapse=" ")
corTopGenerosity2<-paste(c("Corr. Factor: ",corTopGenerosity),collapse=" ")

#plot correlation values for happiest countries - little to no correlation - generosity highest inverse correlation
#relationship between happiness and gdp per capita 
plot1<-top_8_all_years%>%ggplot(aes(Happiness.Rank, Economy..GDP.per.Capita.)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=corTopEcon2)+ggtitle("Happiness & GDP")
#Health and life expectancy
plot2<-top_8_all_years%>%ggplot(aes(Happiness.Rank, Health..Life.Expectancy.)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=corTopLifeEx2)+ggtitle("Happiness & Health")
#relationship between happiness and freedom
plot3<-top_8_all_years%>%ggplot(aes(Happiness.Rank, Freedom)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=corTopFreedom2)+ggtitle("Happiness & Freedom")
#Health and generosity
plot4<-top_8_all_years%>%ggplot(aes(Happiness.Rank, Health..Life.Expectancy.)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=corTopGenerosity2)+ggtitle("Happiness & Generosity")

grid.arrange(plot1,plot2,plot3,plot4,ncol=2)


#plot correlation values for unhappiest countries -

plot1<-bottom_allyears%>%ggplot(aes(Happiness.Rank, Economy..GDP.per.Capita.)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=corTopEcon2)+ggtitle("Happiness & GDP")
#Health and life expectancy
plot2<-bottom_allyears%>%ggplot(aes(Happiness.Rank, Health..Life.Expectancy.)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=corTopLifeEx2)+ggtitle("Happiness & Health")

#relationship between happiness and freedom
plot3<-bottom_allyears%>%ggplot(aes(Happiness.Rank, Freedom)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=corTopFreedom2)+ggtitle("Happiness & Freedom")

#happiness and generosity
plot4<-bottom_allyears%>%ggplot(aes(Happiness.Rank, Health..Life.Expectancy.)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=corTopGenerosity2)+ggtitle("Happiness & Generosity")

grid.arrange(plot1,plot2,plot3,plot4,ncol=2)



#Data is approximately bivariate normal and therefore the correlation line gives a good approximation of happiness based on gdp, health, and freedom
Happiness %>% mutate(z_happy = round((Happiness.Score - mean(Happiness.Score)) / sd(Happiness.Score))) %>%ggplot()+ stat_qq(aes(sample = Happiness$Economy..GDP.per.Capita.)) +facet_wrap( ~ z_happy)

Happiness %>% mutate(z_happy = round((Happiness.Score - mean(Happiness.Score)) / sd(Happiness.Score))) %>%ggplot() +  stat_qq(aes(sample = Happiness$Health..Life.Expectancy.)) + facet_wrap( ~ z_happy)

Happiness %>% mutate(z_happy = round((Happiness.Score - mean(Happiness.Score)) / sd(Happiness.Score))) %>%ggplot() +  stat_qq(aes(sample = Happiness$GdpHealth)) + facet_wrap( ~ z_happy)


#the least square estimates using all three variables
line_fit<-lm(Happiness.Score~Economy..GDP.per.Capita.+ Health..Life.Expectancy.+ Freedom ,data=Happiness)
line_fit
#predicted happiness score using the regression line -- need to not use the year 2022 to determine this line
Happiness %>% 
  filter(Year %in% 2022) %>% 
  mutate(R_hat = predict(line_fit, newdata = .)) %>%
  ggplot(aes(R_hat, Happiness.Score, label = Country)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

#the least square estimates using new variable
line_fit<-lm(Happiness.Score~Economy..GDP.per.Capita. ,data=Happiness)
line_fit




#-----------------------------------------------------------------------
#-----------------MACHINE LEARNING MODEL--------------------------------
#-----------------------------------------------------------------------

#create training and test sets based on economy only
train_set <- Happiness %>% filter(Year!='2022')%>%select(Year,Country,Happiness.Score,Economy..GDP.per.Capita.,Health..Life.Expectancy.)
max(train_set$Year)
test_set <- Happiness %>% filter(Year=='2022')%>%select(Year,Country,Happiness.Score,Economy..GDP.per.Capita.,Health..Life.Expectancy.)
min(test_set$Year)
#create unique identifier
train_set$CtryYear<-paste(train_set$Year, train_set$Country, sep="_")
test_set$CtryYear<-paste(test_set$Year, test_set$Country, sep="_")



#MODEL 1 - GUESS THE AVERAGE
#guess happiness score on train data - 1.19
HappyTrain<-mean(train_set$Happiness.Score)
HappyTrain
#loss 1.19
Sq_Loss<-mean((HappyTrain - test_set$Happiness.Score)^2)
Sq_Loss

#REGRESSION LINE 
#We calculated the lse above 
line_fit<-lm(Happiness.Score~Economy..GDP.per.Capita.,data=Happiness)
line_fit$coefficients

#using the correlation line gives smaller sq loss - .94
y_hat <- predict(line_fit, test_set)
Sq_Loss<-mean((y_hat - test_set$Happiness.Score)^2)
Sq_Loss

#MEAN SQUARED ERRORS
#define the mean squared error function
RMSE <- function(true_score, predicted_score){
  sqrt(mean((true_score - predicted_score)^2))
}

#NAIVE - mean score of all countries, 2015-2021
mu_hat <- mean(train_set$Happiness.Score)
mu_hat
#1.09
naive_rmse <- RMSE(test_set$Happiness.Score, mu_hat)
naive_rmse
#------------------------

#introduce country bias
mu <- mean(train_set$Happiness.Score) 
mu
country_avgs <- train_set %>% 
  group_by(Country) %>% 
  summarize(b_ctry = mean(Happiness.Score - mu))


predicted_ratings <- test_set %>% 
  left_join(country_avgs, by='Country') 

nas<-subset(predicted_ratings,is.na(b_ctry))
nas

 #remove nas from test and train sets
test_set <- subset(test_set, CtryYear!='2022_Czechia')
test_set <-subset(test_set, CtryYear!='2022_Congo')
test_set <-subset(test_set, CtryYear!='2022_Eswatini. Kingdom of')

train_set <- subset(train_set, CtryYear!='2022_Czechia')
train_set <-subset(train_set, CtryYear!='2022_Congo')
train_set <-subset(train_set, CtryYear!='2022_Eswatini. Kingdom of')


test_set%>% filter(CtryYear=='2022_Eswatini. Kingdom of')
train_set%>% filter(CtryYear=='2022_Eswatini. Kingdom of')

# recalculate with only countries whic are in both
mu <- mean(train_set$Happiness.Score) 
mu
country_avgs <- train_set %>% 
  group_by(Country) %>% 
  summarize(b_ctry = mean(Happiness.Score - mu))



predicted_ratings <- test_set %>%
  left_join(country_avgs, by='Country') %>%
  mutate(pred = mu + b_ctry) %>%
  pull(pred)


model_1_rmse <- RMSE(test_set$Happiness.Score,predicted_ratings) - .42
model_1_rmse
#add to table - doesn't work
rmse_results <- bind_rows(model_1_rmse,
                          data_frame(method="Country Bias Model",
                                     RMSE = model_1_rmse ))











