
if(!require(data.table)) install.packages("tinytex", repos = "http://cran.us.r-project.org")
tinytex::install_tinytex(force = TRUE)

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

rm (list=ls())  # remove all variables


#read data
url<-"https://raw.githubusercontent.com/Oanalkd/Happiness/main/Happiness.csv"
Happiness<-read.csv(url)
dim(Happiness)
names(Happiness)
#Create new fields that will be needed
Happiness$CtryYear<-paste(Happiness$Year, Happiness$Country, sep="_")
Happiness$GdpHealth<-(Happiness$Economy..GDP.per.Capita.+Happiness$Health..Life.Expectancy.)/2
Happiness$GdpHealthFree<-(Happiness$Economy..GDP.per.Capita.+Happiness$Health..Life.Expectancy.+Happiness$Freedom)/3

# distinct countries
Happiness%>%summarise(countries=n_distinct(Country))

#Which countries have the highest/lower scores
Happiness$CtryYear[which.max(Happiness$Happiness.Score)]
Happiness$CtryYear[which.min(Happiness$Happiness.Score)]

#Distribution of happiness rating by country AND year
plot1<-Happiness %>% group_by(Country)%>%
  ggplot(aes(Happiness.Score)) + 
  geom_histogram(fill = "light blue")+
  ggtitle("Score by Year and Country")
plot1


#We can see that the distribution is close to normal and therefore can be well described by the mean and standard deviation. 
plot2<-ggplot(data=Happiness,aes(sample=Happiness.Score))+stat_qq()+ggtitle("QQ PLot - Test for Normality")
plot2


#Obtain the mean Happiness score by Country only
MeanHappy<-Happiness%>% group_by(Country)%>% summarise(mean(Happiness.Score))
#Rename columns
colnames(MeanHappy)<-c("Country","Score")

plot3<-MeanHappy %>%ggplot(aes(Score)) +   geom_histogram(fill = "light blue")+  ggtitle("Score by Country")
grid.arrange(plot1,plot2,plot3,nrow=3)

#Top most and bottom most  scores
MeanHappy%>%top_n(5, Score)
MeanHappy%>%top_n(-5, Score)

#mean and sd of happiness score
summary <- Happiness %>%summarize(mean = mean(Happiness$Happiness.Score), sd = sd(Happiness$Happiness.Score))
summary


#Now we look at the overall happiness levels across years.  They are increasing.
MeanHappyYear<-Happiness%>% group_by(Year)%>% summarise(mean(Happiness.Score))
#Rename columns
colnames(MeanHappyYear)<-c("Year","Score")
MeanHappyYear
#Happiness scores are increasing
plot1<-MeanHappyYear%>%ggplot(aes(Year, Score)) + geom_line()
plot1




#We want to see which countries show up in the top 10 for each year.  
top<-MeanHappy%>%arrange(desc(Score))%>%slice(1:10)
top_2015<-Happiness%>%filter(Year==2015)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_2016<-Happiness%>%filter(Year==2016)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_2017<-Happiness%>%filter(Year==2017)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_2018<-Happiness%>%filter(Year==2018)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_2019<-Happiness%>%filter(Year==2019)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_2020<-Happiness%>%filter(Year==2020)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_2021<-Happiness%>%filter(Year==2021)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_2022<-Happiness%>%filter(Year==2022)%>%arrange(desc(Happiness.Score))%>%slice(1:10)
top_allyears<-rbind(top_2015,top_2016,top_2017,top_2018,top_2019,top_2020,top_2021,top_2022)
#Countries which show up in the top 10 happiest countries each year, will show up 8 times in this table
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



#Now we will look at scores by top countries by year, and Rwanda score by year.
p <- ggplot(data=top_8_all_years, aes(x = Year , y = Happiness.Score)) +
  geom_line() +facet_grid(Country~.)
p


#Rwanda by year
p <-Happiness%>%filter(Country=='Rwanda')%>%ggplot(aes(x = Year , y = Happiness.Score)) + geom_line()
p

########################################################################
#CORRELATION BETWEEN FACTORS AND HAPPINESS SCORES ON ALL COUNTRIES
#We seek any relationship between happiness and various other dimensions 
########################################################################

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

#happiness & GdpHealth
plot10<-Happiness%>%ggplot(aes(Happiness.Rank, GdpHealth)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=NULL)

#happiness & GdpHealthFree
plot11<-Happiness%>%ggplot(aes(Happiness.Rank, GdpHealthFree)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=NULL)

#highest correlations
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,plot10, plot11,ncol=3)

#Calculate correlation on top 5 observable correlations
corEcon<-round(cor(Happiness$Happiness.Score,Happiness$Economy..GDP.per.Capita.),digits=2)
corLifeEx<-round(cor(Happiness$Happiness.Score,Happiness$Health..Life.Expectancy.),digits=2)
corFree<-round(cor(Happiness$Happiness.Score,Happiness$Freedom),digits=2)
corGdpHealth<-round(cor(Happiness$Happiness.Score,Happiness$GdpHealth),digits=2)
corGdpHealthFree<-round(cor(Happiness$Happiness.Score,Happiness$GdpHealthFree),digits=2)
corEcon
corLifeEx
corFree
corGdpHealth
corGdpHealthFree

corGdpHealth2<-paste(c("Corr. Factor: ",corGdpHealth),collapse=" ")
corGdpHealthFree2<-paste(c("Corr. Factor: ",corGdpHealthFree),collapse=" ")


#plot if top 2 correlation values
#relationship between happiness and gdp per capita 
plot1<-Happiness%>%ggplot(aes(Happiness.Score, GdpHealth)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=corGdpHealth2)
#Health and life expectancy
plot2<-Happiness%>%ggplot(aes(Happiness.Score, GdpHealthFree)) + 
  geom_point(alpha = 0.5)+theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+labs(x=corGdpHealthFree2)

grid.arrange(plot1,plot2,nrow=2)


# For happiest countries, correlations are small.  The largest is a negative correlation with Generosity
corTopEcon<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$Economy..GDP.per.Capita.),digits=2)
corTopLifeEx<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$Health..Life.Expectancy.),digits=2)
corTopFreedom<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$Freedom),digits=2)
corTopGenerosity<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$Generosity),digits=2)
corTopGdpHealth<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$GdpHealth),digits=2)
corTopGdpHealthFree<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$GdpHealthFree),digits=2)
corTopEcon
corTopLifeEx
corTopFreedom
corTopGenerosity
corTopGdpHealth
corTopGdpHealthFree


# For unhappiest countries, correlations are small.  The largest is a negative correlation with Generosity
corBottomEcon<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$Economy..GDP.per.Capita.),digits=2)
corBottomLifeEx<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$Health..Life.Expectancy.),digits=2)
corBottomFreedom<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$Freedom),digits=2)
corBottomGenerosity<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$Generosity),digits=2)
corBottomGdpHealth<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$GdpHealth),digits=2)
corBottomGdpHealthFree<-round(cor(top_8_all_years$Happiness.Score,top_8_all_years$GdpHealthFree),digits=2)
corBottomEcon
corBottomLifeEx
corBottomFreedom
corBottomGenerosity
corBottomGdpHealth
corBottomGdpHealthFree


########################################################################
#PREDICTING HAPPINESS SCORE 
#We seek various ways of predicting happiness scores
########################################################################


#We create train set to contain years 2015 - 2021, and test_set to contain year 2022
train_set <- Happiness %>% filter(Year!='2022')%>%select(Year,Country,CtryYear, Happiness.Score,Economy..GDP.per.Capita.,Health..Life.Expectancy.)
max(train_set$Year)
test_set <- Happiness %>% filter(Year=='2022')%>%select(Year,Country,CtryYear,Happiness.Score,Economy..GDP.per.Capita.,Health..Life.Expectancy.)
min(test_set$Year)

#MODEL 1 - GUESS THE AVERAGE
#guess happiness score on train data - 1.19
HappyTrain<-mean(train_set$Happiness.Score)
HappyTrain
#loss 1.19
Sq_Loss<-mean((HappyTrain - test_set$Happiness.Score)^2)
Sq_Loss
#create table to store results
Results<-data_frame(Method="Mean Happiness Score",SqLoss=Sq_Loss)


#Data is approximately bivariate normal and therefore the correlation line gives a good approximation of happiness based on gdp, health, and freedom
Happiness %>% mutate(z_happy = round((Happiness.Score - mean(Happiness.Score)) / sd(Happiness.Score))) %>%ggplot()+ stat_qq(aes(sample = Happiness$Economy..GDP.per.Capita.)) +facet_wrap( ~ z_happy)

Happiness %>% mutate(z_happy = round((Happiness.Score - mean(Happiness.Score)) / sd(Happiness.Score))) %>%ggplot() +  stat_qq(aes(sample = Happiness$Health..Life.Expectancy.)) + facet_wrap( ~ z_happy)


#MODEL 2 uses the regression line based on GDP and Health to predict happiness score on the test set - Squared loss = 0.74
line_fit<-lm(Happiness.Score~Economy..GDP.per.Capita.+ Health..Life.Expectancy.,data=test_set)
line_fit

prediction <- line_fit$coef[1] + line_fit$coef[2]*test_set$Happiness.Score
Sq_Loss<-mean((prediction - test_set$Happiness.Score)^2)
Results <- Results%>% add_row(Method="Correlation Line",SqLoss=Sq_Loss)

#Plotting the regression line against test set, year 2022.
Happiness %>% 
  filter(Year %in% 2022) %>% 
  mutate(R_hat = predict(line_fit, newdata = .)) %>%
  ggplot(aes(R_hat, Happiness.Score, label = Country)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()


#MODEL 3 - Using the Mean Squared errors method Naive model

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
Sq_Loss<-mean((mu_hat - test_set$Happiness.Score)^2)
Sq_Loss
Results$RMSE<-NA
Results <- Results%>% add_row(Method="Naive RMSE - RMSE using Mean",SqLoss=Sq_Loss,RMSE=naive_rmse)
Results
#------------------------

#MODEL 4 - Using the Mean Squared errors adding Country bias
mu <- mean(train_set$Happiness.Score) 
mu
country_avgs <- train_set %>% 
  group_by(Country) %>% 
  summarize(b_ctry = mean(Happiness.Score - mu))

predicted_ratings <- test_set %>%
 left_join(country_avgs, by='Country') %>%
  mutate( pred = mu + b_ctry)
 


nas<-subset(predicted_ratings,is.na(b_ctry))
nas

#remove nas from test and train sets
test_set <- subset(test_set, CtryYear!='2022_Czechia')
test_set <-subset(test_set, CtryYear!='2022_Congo')
test_set <-subset(test_set, CtryYear!='2022_Eswatini. Kingdom of')

train_set <- subset(train_set, CtryYear!='2022_Czechia')
train_set <-subset(train_set, CtryYear!='2022_Congo')
train_set <-subset(train_set, CtryYear!='2022_Eswatini. Kingdom of')



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
Sq_Loss<-mean((mu - test_set$Happiness.Score)^2)
Sq_Loss

Results <- Results%>% add_row(Method="RMSE - with Country Bias",SqLoss=Sq_Loss,RMSE=model_1_rmse)
Results











