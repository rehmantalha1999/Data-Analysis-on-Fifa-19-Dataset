library(dplyr)
library(ggplot2)
library(stringr)
data <- read.csv('data.csv')
rel.data <- data %>% select(Name,Age,Nationality,Overall,Potential,Club,Value,Preferred.Foot,
                Position,Height,Weight) %>% filter(Overall>65) %>% filter(Position!='')
head(rel.data)
sum(is.na(rel.data)) #no na values in selected data

ggplot(rel.data) +
  aes(Age, Height) +
  geom_boxplot(color='black', fill='grey60') +
  ggtitle('Age vs Height') +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face='bold'),
        axis.title.y = element_text(size=12),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=12),
        axis.text.x = element_text(size=11),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank())

position.overall <- rel.data %>% group_by(Position) %>% summarise(ovr=mean(Overall))

ggplot(position.overall) + aes(Position,ovr) +
  geom_segment(aes(x=Position,xend=Position,y=70,yend=ovr), size=3, color='grey60') +
  geom_point(size=5, color='black')+
  ylab('Overall')+
  ggtitle(label='Mean Overall', subtitle = 'in each Position') +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size=11),
        plot.title = element_text(hjust = 0.5, size = 16, face='bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 14, face='bold'),
        axis.title.y = element_text(size=12),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=12),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank())

rel.data$Value <- as.numeric(substring(rel.data$Value,1,str_length(rel.data$Value)-1)) 

value.Nationality <- rel.data%>% group_by(Nationality) %>% summarise(Total.Value=sum(Value)) %>% 
  arrange(desc(Total.Value)) %>% top_n(20, Total.Value)
value.Nationality$Total.Value <- value.Nationality$Total.Value/10000

ggplot(value.Nationality) + aes(Nationality,Total.Value) +
  geom_col(position = 'dodge', fill='grey60', color='black', width = 0.7) +
  ylab('Value / Billion Euro') +
  xlab('Country')+
  ggtitle(label='Total Value', subtitle = 'of Players of Each Country')+
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size=11),
        plot.title = element_text(hjust = 0.5, size = 16, face='bold'),
        plot.subtitle = element_text(hjust = 0.5, size = 14, face='bold'),
        axis.title.y = element_text(size=12),
        axis.text.y = element_text(size=11),
        axis.title.x = element_text(size=12),
        panel.background = element_rect(fill = 'white'),
        panel.grid.major = element_blank())
