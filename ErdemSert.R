dataset <- read.csv("Life_expectancy_dataset.csv")
head(dataset)
sum(is.na(dataset))
library(ggplot2)
library(dplyr)

#First Graph

#This graph shows the Overall Life Expectancy by looking at each continent and using mutate function to find the result
SortedWithContinent <- dataset %>% group_by(Continent)%>%
  select(Male.Life,Female.Life)%>%
summarise(avgMale=mean(Male.Life),avgFemale=mean(Female.Life))%>%
filter(Continent=="Europe"|Continent=="Asia"|
         Continent=="Africa"|Continent=="North America"|
         Continent=="Oceania"|Continent=="South America")

numbers <- c("61.8","73.6","79.1","76.3","74.3","75.1")

SortedWithContinent%>%
  mutate(SortedWithContinent,OverallLifeOfEachContinent=(avgMale+avgFemale)/2)%>%
  ggplot2::ggplot(aes(Continent,OverallLifeOfEachContinent,fill=Continent))+
  geom_bar(position="dodge", stat="identity", colour="black", width=0.3)+
  ggtitle("Overall Life Expectancy in Each Continent")+
  geom_text(aes(label=numbers), size = 4, fontface = "bold", vjust=-0.2) +
  theme(legend.title = element_text(size=14,hjust = 0.5), 
        legend.position = "right", 
        legend.text = element_text(size = 11),
        plot.title = element_text(color = "Red",size = 14,face = "bold",hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))+
  xlab("Continents")+
  ylab("Overall Life Expectancy")




#Second Graph
values <- c("60.1","71.0","76.0","73.9","71.6","72.2")
        
#This graph shows the average of Male Life Expectancy in each Continent.
dataset %>% group_by(Continent)%>%summarise(avgMale=mean(Male.Life))%>%
  ggplot2::ggplot(aes(Continent,avgMale,fill=Continent))+
  geom_bar(position="dodge", stat="identity", colour="black", width=0.3)+
  ggtitle("Average of Male Life Expectancy in Each Continent")+
  geom_text(aes(label=values), size = 4, fontface = "bold", vjust=-0.2) +
  theme(legend.title = element_text(size=14,hjust = 0.5), 
        legend.position = "right", 
        legend.text = element_text(size = 11),
        plot.title = element_text(color = "Red",size = 14,face = "bold",hjust = 0.5),
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12),
       axis.text.x = element_text(size = 10),
       axis.text.y = element_text(size = 10))+
  xlab("Countries")+
  ylab("Male Life Expectancy")


#Third Graph

#This graph shows that 5 Countries that have less Overall Life expectancy than the other countries
dataset %>% 
  dplyr::group_by(Country)%>%
  dplyr::summarise(avgOverall=mean(Overall.Life))%>%arrange(desc(avgOverall))%>%
  top_n(-5,avgOverall)%>%
  ggplot2::ggplot(aes(Country,avgOverall))+
  geom_segment( aes(x=Country, xend=Country, y=0, yend=avgOverall)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)+
  ggtitle("5 Country that have worst Overall Life Expectancy")+
  theme( plot.title = element_text(color = "Red",size = 14,face = "bold",hjust = 0.5),
         legend.title = element_text(size=0),
         legend.position = "right",
         legend.text = element_text(size = 15, color = "Black"),
         axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10))+
  xlab("Countries")+
  ylab("Overall Life Expectancy")



