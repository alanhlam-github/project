## Goal and Key Questions

ads=readxl::read_excel('facebookgoogle_ads.xlsx')

ads=ads |> 
  mutate(date2=month(Date))

dim(ads)
tail(ads)

#Total costs per ad
ads |> 
  summarize('Total Facebook Cost per Ad'=sum(`Cost per Facebook Ad`),
            'Total Google Cost per Ad'=sum(`Cost per Google Ad`)) |> 
  datatable(rownames = F,options = list(dom='t')) |> 
  formatCurrency(columns = c(1,2))

## What are the daily and monthly conversions?

ads |> 
  ggplot(aes(x=Date,y=`Facebook Ad Conversions`,col=`Facebook Ad Conversions`))+
  labs(title='Daily Facebook Conversions',y='Conversions',x='Date',col=' ')+
  geom_line(lwd=.5)+
  geom_smooth(method='auto')+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5))+
  scale_color_gradient(high='darkgreen',low='lightgreen')+
  expand_limits(y=20)

ads |> 
  ggplot(aes(x=Date,y=`Google Ad Conversions`,color=`Google Ad Conversions`))+
  labs(title='Daily Google Conversions',y='Conversions',x='Date',col=' ')+
  geom_line(lwd=.5)+
  geom_smooth(method='auto')+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5))+
  scale_color_gradient(high='darkgreen',low='lightgreen')+
  expand_limits(y=10)

#create monthly bar plot of conversions
ads_monthly=ads |> 
  group_by(date2) |> 
  summarise(fb_conversions=sum(`Facebook Ad Conversions`),
            goo_conversions=sum(`Google Ad Conversions`))

m1=ads_monthly |> 
  ggplot(aes(x=date2,y=goo_conversions,fill=goo_conversions))+
  geom_bar(stat='identity')+
  theme_bw()+
  theme(plot.title = element_text(hjust=.5))+
  labs(title='Google Monthly Conversions',x='Date',y='Conversions',fill=' ')+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sept','Oct','Nov','Dec'))+
  expand_limits(y=200)+
  scale_fill_gradient(high='darkgreen',low='lightgreen')+
  geom_smooth(method='auto',se=F)

m2=ads_monthly |> 
  ggplot(aes(x=date2,y=fb_conversions,fill=fb_conversions))+
  geom_bar(stat='identity')+
  theme_bw()+
  theme(plot.title = element_text(hjust=.5))+
  labs(title='Facebook Monthly Conversions',x='Date',y='Conversions',fill=' ')+
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),
                     labels=c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sept','Oct','Nov','Dec'))+
  expand_limits(y=500)+
  scale_fill_gradient(high='darkgreen',low='lightgreen')+
  geom_smooth(method='auto',se=F)

library(gridExtra)

grid.arrange(m2,m1)

## Does one platform outperform the other? If so, by how much?
#remove from report
---
shapiro.test(ads$`Facebook Ad Conversions`)
shapiro.test(ads$`Google Ad Conversions`)

ads |> 
  ggplot(aes(`Facebook Ad Conversions`))+
  geom_histogram(aes(y=..density..))+
  geom_density(color='blue',lwd=1)

ads |> 
  ggplot(aes(`Google Ad Conversions`))+
  geom_histogram(aes(y=..density..))+
  geom_density(color='red',lwd=1)
---

#run stat test
wilcox.test(ads$`Facebook Ad Conversions`,ads$`Google Ad Conversions`,paired=F)

ads |> 
  summarise('Facebook Total Conversions'=sum(`Facebook Ad Conversions`),
            'Google Total Conversions'=sum(`Google Ad Conversions`)) |> 
  datatable(rownames = F,options = list(dom='t'))

(4286-2183)/4286*100

## Relationship between clicks and conversions


#Assumption checks for LM
library(performance)

ads2=ads |> 
  mutate(facebook_clicks=`Facebook Ad Clicks`,
         facebook_conversions=`Facebook Ad Conversions`,facebook_adviews=`Facebook Ad Views`)


fb_model= lm(facebook_conversions~facebook_clicks,data=ads2)

plot(fb_model) 
#LM looks fine

#Regression analysis
summary(fb_model)

### Predictive Model
new_clicks=data.frame(facebook_clicks=c(25,50,100))

predict(fb_model,new_clicks)

#Scatter plot for model
scatter1=ads2 |> 
  ggplot(aes(x=facebook_clicks,y=facebook_conversions))+
  geom_point()+
  geom_smooth(method='lm')+
  labs(title='Facebook Ads - Conversions by Clicks',x='Clicks',y='Conversions',col=' ')+
  theme_bw()+
  theme(plot.title=element_text(hjust=.5))

plotly::ggplotly(scatter1)

sessionInfo()
