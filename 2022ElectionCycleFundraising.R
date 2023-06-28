party=import('https://github.com/alanhlam-github/dataset/raw/main/2022%20election%20cycle%20fundraising.xlsx')

#Party Control of Congress

party |> 
  tabyl(Party,Chamber) |> 
  adorn_totals() |> 
  adorn_percentages('all') |> 
  adorn_pct_formatting(digits = 1,affix_sign = T) |> 
  adorn_ns(position='front') |> 
  datatable()

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
  labs(title = 'Dollars Raised by Dollars Spent', x='Dollars Spent in Millions',y='Dollars Raised in Millions')+
  annotate('text', label='rs = 0.92',x=30000000,y=60000000,color='blue')

ggplotly(t1)

#Insight
party_best_ROI = party |> 
  mutate(ROI_percentage = (Raised/Spent)*100) |> 
  select(Member,ROI_percentage, Raised, Spent,Debts,`Cash on Hand`,everything()) |> 
  arrange(desc(ROI_percentage))

datatable(party_best_ROI,colnames=c('ROI %'=3),options = list(pageLength = 5)) |> 
  formatRound('ROI %',2) |> 
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
  labs(title = 'Predictive Model', x='Dollars Spent',y='Dollars Raised')

ggplotly(t2)

sessionInfo()