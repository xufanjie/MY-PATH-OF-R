#Question 1
library(ggplot2)
#Create a new dataframe to get the clean data we need
Length<-c(iris$Sepal.Length,iris$Petal.Length)
Width<-c(iris$Sepal.Width,iris$Petal.Width)
Partsp<-rep('Sepal',length(Length)/2)
Partpt<-rep('Petal',length(Length)/2)
Part<-c(Partsp,Partpt)
Species<-rep(iris$Species,2)
iris1<-data.frame(Species=Species,Part=Part,Length=Length,Width=Width)
#Draw the plot
ggplot(iris1,aes(x=Length,y=Width,col=Part))+
  geom_point()+
  facet_grid(.~Species)

#Q2
#Part 1
library(dplyr)
gapminder<-read.csv('assignment3/gapminderDataFiveYear.csv')
gapminder %>%
     group_by(continent,year)%>%
     summarise(lifeExp=mean(lifeExp)) %>%
     ggplot(aes(x=year,y=lifeExp,col=continent))+
     geom_line()
#Comment: Except for life expectancy of Africa, that of other four continents are converging.
#It seems that Africa has been forgotten by the world.
#Part 2
gapminder %>%
  group_by(continent,year)%>%
  summarise(lifeExp=mean(lifeExp),gdpPercap=mean(gdpPercap)) %>%
  ggplot(aes(x=lifeExp,y=gdpPercap))+
  geom_line()+
  facet_wrap(~continent,scales='free')
#The audience is right, both Africa and Asia once had obvious reversed relation between life expectancy and GDP per capita