party=import('https://github.com/alanhlam-github/dataset/raw/main/2022%20election%20cycle%20fundraising.xlsx')

party |> 
  tabyl(Party,Chamber) |> 
  adorn_totals() |> 
  adorn_percentages('all') |> 
  adorn_pct_formatting(digits = 1,affix_sign = T) |> 
  adorn_ns(position='front') |> 
  datatable(caption=htmltools::tags$caption(style =
  'caption-side: top; text-align: left;','Table 1: Party Control of Congress'),options = list(dom='t'))

#Check for Normality

d1=party |> 
  ggplot(aes(Raised))+
  geom_histogram(aes(y=..density..))+
  geom_density(col='blue')

d2=party |> 
  ggplot(aes(Spent))+
  geom_histogram(aes(y=..density..))+
  geom_density(col='red')

grid.arrange(d1,d2)

#Correlation Test
cor.test(party$Spent,party$Raised,method='spearman')

#Scatter Plot

t1=party |> 
  ggplot(aes(x=Spent,y=Raised))+
  geom_point()+
  geom_smooth(method='auto')+
  theme_bw()+
  theme(plot.title = element_text(hjust=.5))+
  labs(title = 'Dollars Raised by Dollars Spent', x='Spent',y='Raised')+
  annotate('text', label='rs = 0.92',x=30000000,y=60000000,color='blue')+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

ggplotly(t1)

#Insight
party_best_ROI = party |> 
  mutate(ROI_percentage = (Raised/Spent)) |> 
  select(Member,ROI_percentage, Raised, Spent,Debts,`Cash on Hand`,everything()) |> 
  arrange(desc(ROI_percentage))

datatable(party_best_ROI,caption=htmltools::tags$caption(style = 'caption-side: top; text-align: left;','Table 2: ROI of Each Congress Member'),
          colnames=c('ROI'=3),options = list(pageLength = 5)) |> 
  formatPercentage('ROI') |> 
  formatCurrency(columns=(c('Raised','Spent','Debts','Cash on Hand')))

#Test assumptions

mod1=lm(Raised~Spent,data=party)

plot(mod1)

gv=gvlma(mod1)

summary(gv)

#Assumptions for LM not met.

#Generalized Linear Model
mod2=glm(Raised~Spent,data=party)

check_model(mod2)

summary(mod2)

report(mod2)

#Predicting the Outcome
new_spent=data.frame(Spent=c(500000,1000000,2000000))

predict(mod2,new_spent) |> round(2)


t2=party |>  
  filter(Spent >= 500000 & Spent <= 2000000 & Raised >= 800000 & Raised <= 3000000) |> 
  ggplot(aes(x=Spent,y=Raised))+
  geom_point(size=1)+
  geom_smooth(method='auto')+
  theme_bw()+
  theme(plot.title = element_text(hjust=.5))+
  labs(title = 'Predictive Model', x='Dollars Spent',y='Dollars Raised')+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(labels = scales::comma)

ggplotly(t2)

sessionInfo()