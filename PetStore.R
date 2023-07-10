pet=import('https://github.com/alanhlam-github/dataset/raw/main/pet_store.xlsx')

table(is.na(pet))
table(duplicated(pet))

# Quick Summary
pet |> 
  group_by(prod_category) |> 
  summarise(sales=sum(total_sales)) |> 
  arrange(-sales) |> 
  datatable(rownames=F,colnames=c('Product Category'='prod_category','Total Sales'='sales'),options = list(dom='t')) |> 
  formatCurrency('Total Sales')

# Sales Over Time; line plot daily sales
pet_trend_date=pet |> 
  group_by(date) |> 
  summarise(sales=sum(total_sales))

monthly_sales=pet |> 
  group_by(trans_month) |> 
  summarise(sales=sum(total_sales))

pet_trend_date |> 
  ggplot(aes(x=date,y=sales,col=sales)) + 
  geom_line(stat='identity',lwd=1)+
  theme_bw()+
  labs(title='Daily Sales from January 1 to June 30',x='Date',y='Dollars',col= ' ')+
  theme(plot.title = element_text(hjust=.5))+
  expand_limits(y=25000)+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_gradient(high='darkgreen',low='lightgreen')

#monthly sales barplot
monthly_sales |> 
  ggplot(aes(x=trans_month,y=sales,fill=sales))+
  geom_bar(stat='identity')+
  theme_bw()+
  labs(title='Monthly Sales from January 1 to June 30',x='Date',y='Dollars')+
  theme(plot.title = element_text(hjust=.5))+
  expand_limits(y=600000)+
  scale_x_continuous(breaks=c(1,2,3,4,5,6),
                     labels=c('January','February','March','April','May','June'))+
  guides(fill=F)+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_gradient(high='darkgreen',low='lightgreen')

#Retention rate
sum(duplicated(pet$cust_id))/sum(!duplicated(pet$cust_id))*100

#Customer age
pet |> 
  ggplot(aes(x=cust_age))+
  theme_bw()+
  geom_bar(color='darkred',fill='steelblue')+
  labs(y='Count',x='Customer Age',title='Total Count of Customers by Age')+
  theme(plot.title = element_text(hjust=.5))+
  expand_limits(y=3000)+
  scale_y_continuous(labels = scales::comma)

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
  scale_fill_manual(values = c('red','darkgreen'))+
  scale_x_continuous(labels = scales::comma)

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
  expand_limits(y=12000)+
  labs(title='Product Categories Sold by Count',x='Product Category',y='Count',fill=' ')+
  theme(plot.title = element_text(hjust=.5))+
  scale_fill_manual(values = c('darkorange','steelblue'))+
  scale_y_continuous(labels = scales::comma)

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
pet_rounded=data.frame(prod_title=pet$prod_title,prod_animal_type=pet$prod_animal_type,
                       total_sales=round(pet$total_sales,2))

pet4=pet_rounded |> 
  group_by('Animal Type'=prod_animal_type) |>
  summarise(Sales=sum(total_sales)) |> 
  arrange(-Sales)

#Reorder to sort desc
pet |> 
  ggplot(aes(y=total_sales, x=reorder(prod_animal_type,total_sales),fill=prod_animal_type))+
  theme_bw()+
  expand_limits(y=1000000)+
  geom_bar(stat='identity')+
  labs(title='Animal Product Types Sold by Dollars',x='Animal Product Type',y='Dollars')+
  scale_fill_manual(values = c('darkorange','steelblue'))+
  guides(fill=F)+
  theme(plot.title=(element_text(hjust=.5)))+
  scale_y_continuous(labels = scales::comma)

datatable(pet4,colnames=c('Total Sales'='Sales','Animal Product Type'='Animal Type'),
          options=list(searching=F,dom='t')) |> 
  formatCurrency(columns=c('Total Sales'))


pet_titles=pet_rounded |> 
  group_by(prod_title,prod_animal_type) |> 
  summarise(total_sales=sum(total_sales)) |> 
  arrange(-total_sales)

#Total sales in dollars and sales percentage
total_sales_pct=pet_titles |> 
  group_by(prod_title,prod_animal_type,total_sales) |> 
  summarise(overall=sum(total_sales/1609489)) |> 
  arrange(-overall)

total_sales_pct |> 
  ggplot(aes(x=total_sales,y=reorder(prod_title,total_sales),fill=prod_animal_type))+
  geom_bar(stat='identity')+
  theme_bw()+
  labs(x='Dollars',y='Product Name',title='Product Sold by Dollar Amount',fill=' ')+
  theme(plot.title = element_text(hjust=.5))+
  scale_fill_manual(values = c('darkorange','steelblue'))+
  scale_x_continuous(labels = scales::comma)

datatable(total_sales_pct,colnames = c('Product Name'=2,'Cat or Dog'=3,'Total Sales'=4,'Percentage'=5)) |> 
  formatCurrency('Total Sales') |> 
  formatPercentage('Percentage')

#create pct for reddy beddy overall sales
reddybeddy_overallsales= pet |> 
  filter(prod_title %in% c('Reddy Beddy')) |>
  group_by(cust_state) |> 
  summarise(total_sales=sum(total_sales),n()) |> 
  arrange(-total_sales)

reddybeddy_pctoverallsales= reddybeddy_overallsales |> 
  group_by(cust_state,total_sales) |> 
  summarise(pct=sum(total_sales)/408023.09) |> 
  arrange(-pct)

datatable(reddybeddy_pctoverallsales,colnames=c('State'=2,'Total Sales'=3,'Percentage'=4)) |> 
  formatPercentage('Percentage') |> 
  formatCurrency('Total Sales')

#Group by date for Reddy
reddy_group_date=pet |> 
  filter(prod_title %in% c('Reddy Beddy')) |>
  group_by(date) |> 
  summarise(total_sales=sum(total_sales))

#Line plot
reddy_group_date |> 
  ggplot(aes(x=date,y=total_sales))+
  geom_line(lwd=1)+
  labs(title='Reddy Beddy Daily Sales',x='Date',y='Dollars')+
  theme(plot.title = element_text(hjust=.5))+
  expand_limits(y=8000)+
  scale_y_continuous(labels = scales::comma)

sessionInfo()
