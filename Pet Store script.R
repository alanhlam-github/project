knitr::opts_chunk$set(message=F,warning=F,echo = F)

inu=readxl::read_xlsx('pet store.xlsx')

table(is.na(inu))
table(duplicated(inu))
#No NULL values

sum(duplicated(inu$cust_id))/sum(!duplicated(inu$cust_id))*100

inu |> 
  ggplot(aes(x=cust_age))+
  theme_bw()+
  geom_bar(color='darkred')+
  labs(y='Total Count',x='Customer Ages')

inu |> 
  group_by(Age=cust_age) |> 
  summarise(Count=sum(cust_age)) |> 
  arrange(desc(Count))

#Create new DF to sort cust_state in desc order
inu2=data.frame(States=fct_infreq(inu$cust_state))

#Added scale_y_discrete(limits = rev(levels(inu2$States))) to reverse ascending order
inu2 %>%
  ggplot(aes(y=States,fill=inu$cust_state %in% c('New York','Pennsylvania','New Jersey')))+
  theme_bw()+
  geom_bar()+
  scale_y_discrete(limits = rev(levels(inu2$States))) +
  labs(y='States',x='Number of Transactions',title=' States by Transactions')+
  theme(plot.title=element_text(hjust=.5))+
  guides(fill=F)+
  scale_fill_manual(values = c('red','darkgreen'))

#Rename variables in order
inu %>% 
  rename('State'=cust_state) %>% 
  tabyl('State') %>%
  adorn_pct_formatting(digits = 1,affix_sign = T) |> 
  rename('Percent'=percent) |> 
  arrange(desc(n)) |> 
  rename('Customers'=n)

#Create inu3 to order prod category
inu3=data.frame(prod_category=fct_infreq(inu$prod_category),prod_animal_type=inu$prod_animal_type)

inu3 %>% 
  ggplot(aes(x=prod_category,fill=prod_animal_type))+
  geom_bar()+
  theme_bw()+
  labs(title='Items Sold per Unit',x='Product Category',y='Count',fill=' ')+
  theme(plot.title = element_text(hjust=.5))+
  scale_fill_manual(values = c('red','blue'))

inu %>% 
  rename(Product_Category=prod_category) %>% 
  tabyl(Product_Category,prod_animal_type) %>%
  adorn_totals() %>% 
  adorn_percentages('all') %>%
  adorn_pct_formatting(digits = 1,affix_sign = T) %>%
  adorn_ns(position='front') %>% 
  arrange(desc(cat))

inu4=inu %>% 
  group_by('Product Title'=prod_title,'Animal Type'=prod_animal_type) %>%
  summarise(Sales=sum(total_sales)) |> 
  arrange(-Sales)

#Use reorder() to sort descending
inu4 |> 
  ggplot(aes(x=Sales, y=reorder(`Product Title`,Sales),fill=Sales > 119000))+
  theme_bw()+
  geom_bar(stat='identity')+
  labs(title='Items Sold by Dollars',x='Dollars',y='Product Items')+
  scale_fill_manual(values = c('darkred','steelblue'))+
  guides(fill=F)+
  theme(plot.title=(element_text(hjust=.5)))

inu4

sessionInfo()
