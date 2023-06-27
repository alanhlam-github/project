pet=import('https://github.com/alanhlam-github/dataset/blob/main/pet%20store.xlsx')

table(is.na(pet))
table(duplicated(pet))

#Retention rate
sum(duplicated(pet$cust_id))/sum(!duplicated(pet$cust_id))*100

#Customer age
pet |> 
  ggplot(aes(x=cust_age))+
  theme_bw()+
  geom_bar(color='darkred')+
  labs(y='Count',x='Customer Age',title='Total Count of Customers by Age')+
  theme(plot.title = element_text(hjust=.5))

pet |> 
  tabyl(cust_age) |> 
  adorn_pct_formatting(digits = 1,affix_sign = T)|> 
  arrange(desc(n)) |> 
  datatable(colnames=c('Age'=2,'Count'=3,'Percentage'=4),options=list(searching=F))

#Create new DF to sort cust_state in desc order
pet2=data.frame(States=fct_infreq(pet$cust_state))

#Added scale_y_discrete(limits = rev(levels(pet2$States))) to reverse ascending order
pet2 |>
  ggplot(aes(y=States,fill=pet$cust_state %in% c('New York','Pennsylvania','New Jersey')))+
  theme_bw()+
  geom_bar()+
  scale_y_discrete(limits = rev(levels(pet2$States))) +
  labs(y='US State',x='Transactions',title=' States by Transactions')+
  theme(plot.title=element_text(hjust=.5))+
  guides(fill=F)+
  scale_fill_manual(values = c('red','darkgreen'))

#Rename variables in order
pet |> 
  rename('State'=cust_state) |> 
  tabyl('State') |> 
  adorn_pct_formatting(digits = 1,affix_sign = T) |> 
  rename('Percent'=percent) |> 
  arrange(desc(n)) |> 
  rename('Customers'=n) |> 
  datatable()

#Create pet3 to order prod category
pet3=data.frame(prod_category=fct_infreq(pet$prod_category),prod_animal_type=pet$prod_animal_type)

pet3 |> 
  ggplot(aes(x=prod_category,fill=prod_animal_type))+
  geom_bar()+
  theme_bw()+
  labs(title='Products Sold by Count',x='Product Category',y='Count',fill=' ')+
  theme(plot.title = element_text(hjust=.5))+
  scale_fill_manual(values = c('red','lightblue'))

pet |> 
  rename(`Product Category`=prod_category) |> 
  tabyl(`Product Category`,prod_animal_type) |>
  adorn_totals() |> 
  adorn_percentages('all') |>
  adorn_pct_formatting(digits = 1,affix_sign = T) |>
  adorn_ns(position='front') |> 
  arrange(desc(Cat)) |> 
  datatable(rownames = T,colnames = c('Cat'=3,'Dog'=4),options=list(searching=F))

#create DF to round to two decimals
pet_rounded=data.frame(prod_title=pet$prod_title,prod_animal_type=pet$prod_animal_type,total_sales=round(pet$total_sales,2))

pet4=pet_rounded |> 
  group_by('Product Title'=prod_title,'Animal Type'=prod_animal_type) |>
  summarise(Sales=sum(total_sales)) |> 
  arrange(-Sales)

#Use reorder() to sort descending
pet4 |> 
  ggplot(aes(x=Sales, y=reorder(`Product Title`,Sales),fill=Sales > 119000))+
  theme_bw()+
  geom_bar(stat='identity')+
  labs(title='Products Sold by Dollars',x='Dollar Amount',y='Product Items')+
  scale_fill_manual(values = c('darkred','steelblue'))+
  guides(fill=F)+
  theme(plot.title=(element_text(hjust=.5)))

datatable(pet4,colnames=c('Total Sales'='Sales')) |> 
  formatCurrency(columns=c('Total Sales'))

sessionInfo()
