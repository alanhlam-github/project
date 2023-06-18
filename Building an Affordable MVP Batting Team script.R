mlb=readxl::read_excel('MLB_1985-2016.xlsx')
dim(mlb)

## Clean

mlb_cleaned = mlb |> 
  drop_na() |> 
  dplyr::filter(!duplicated(`Player Name`)) |> 
  rename(Salary = salary, Runs = R)

sum(is.na(mlb_cleaned))
dim(mlb_cleaned)


summary(mlb_cleaned$Salary)

summary(mlb_cleaned$RBI)
summary(mlb_cleaned$Runs)

  ## Explore
 
mlb_mvp = mlb_cleaned |> 
  dplyr::filter(Season %in% c('2016':'2006') & RBI > 13 & Runs > 14 & Salary < 410000) |> 
  select(`Player Name`,Runs,RBI,Salary,Team, everything()) |> arrange(desc(Runs)) |> 
  arrange(desc(RBI))

library(gridExtra)

shapiro.test(mlb_mvp$RBI)
shapiro.test(mlb_mvp$Runs)

mdens1=mlb_mvp %>% 
  ggplot(aes(RBI))+
  geom_histogram(aes(y=..density..))+
  geom_density(col='red',lwd=1)

mdens2=mlb_mvp %>% 
  ggplot(aes(Runs))+
  geom_histogram(aes(y=..density..))+
  geom_density(col='green',lwd=1)

grid.arrange(mdens1,mdens2)

cor.test(mlb_mvp$Runs,mlb_mvp$RBI,method='spearman')

mlb_mvp |> 
  ggplot(aes(x= Runs,y = RBI))+
  geom_smooth(method='auto')+
  geom_point(size=2)+
  labs(title='RBI by Runs and less than $410,000',x='Runs',y='Runs Batted In',caption = 'Source: Major League Baseball data from 1985 to 2016')+
  theme_minimal()+
  theme(legend.position = 'none')+
  theme(plot.title = element_text(hjust = .5))+
  annotate('text',x=111,y=125,label='Carlos Gonzalez',size=3,col='darkred')+
  annotate('text',x=104,y=157,label='Ryan Howard',size=3,col='darkred')+
  annotate('text',x=35,y=120,label="rs = 0.8263",col='blue')

## Recommendation

mlb_mvp %>% 
  head(40)

sessionInfo()