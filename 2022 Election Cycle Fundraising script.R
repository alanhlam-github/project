
```{r}

library(rio)

MVP_party=import('https://github.com/alanhlam-github/dataset/raw/main/2022%20election%20cycle%20fundraising.xlsx')
```


```{r}
library(janitor)

MVP_party |> 
  tabyl(Party,Chamber) |> 
  adorn_totals() |> 
  adorn_percentages('all') |> 
  adorn_pct_formatting(digits = 1,affix_sign = T) |> 
  adorn_ns(position='front')
```

```{r}
library(gridExtra)

d1=MVP_party |> 
  ggplot(aes(Raised))+
  geom_histogram(aes(y=..density..))+
  geom_density(col='blue')

d2=MVP_party |> 
  ggplot(aes(Spent))+
  geom_histogram(aes(y=..density..))+
  geom_density(col='red')

grid.arrange(d1,d2)

```

```{r}
cor.test(MVP_party$Spent,MVP_party$Raised,method='spearman')
```

```{r}
library(plotly)

t1=MVP_party |> 
  ggplot(aes(x=Spent,y=Raised))+
  geom_point()+
  geom_smooth(method='auto')+
  theme_bw()+
  theme(plot.title = element_text(hjust=.5))+
  labs(title = 'Dollars Raised by Dollars Spent', x='Dollars Spent in Millions',y='Dollars Raised in Millions',caption = 'Source: OpenSecrets.org')+
  annotate('text', label='rs = 0.92',x=30000000,y=60000000,color='blue')

ggplotly(t1)

```

```{r}

MVP_party_best_ROI = MVP_party |> 
  mutate(ROI_percentage = (Raised/Spent)*100) |> 
  select(Member,`ROI %`=ROI_percentage, Raised, Spent,Debts,`Cash on Hand`,everything()) |> 
  arrange(desc(`ROI %`))

MVP_party_best_ROI
```

```{r}
mod1=glm(Raised~Spent,data=MVP_party)

check_model(mod1)

summary(mod1)

report(mod1)
```

```{r}
new_spent=data.frame(Spent=c(500000,1000000,2000000))

predict(mod1,new_spent) |> round(2)

```

```{r}
t2=MVP_party %>%  
  filter(Spent >= 500000 & Spent <= 2000000 & Raised >= 800000 & Raised <= 3000000) %>% 
  ggplot(aes(x=Spent,y=Raised))+
  geom_point(size=1)+
  geom_smooth(method='auto')+
  theme_bw()+
  theme(plot.title = element_text(hjust=.5))+
  labs(title = 'Predictive Model', x='Dollars Spent',y='Dollars Raised',caption = 'Source: OpenSecrets.org')

ggplotly(t2)
```

```{r}
sessionInfo()
```
