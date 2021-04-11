ddd <- my_data %>% 
  
  select(mfiid, finsoc, age, assets, stmktcap, pcrdbgdp, kkm, gdp_growth_annual, legal_tradition, currentlegalstatus, year) %>% 
  
  mutate(year = factor(year))

library(mlogit)
library(gmnl)

long.data <- mlogit.data(ddd, choice = "finsoc", shape = "wide")
long.data

my_model <- mlogit(finsoc ~ 1| age + assets + stmktcap + pcrdbgdp + kkm + gdp_growth_annual + legal_tradition + 
                     
                     currentlegalstatus + year, data = long.data, reflevel = "FF" )

summary(my_model)

my_gmnl <- gmnl(finsoc ~ 1| age + assets + stmktcap + pcrdbgdp + kkm + gdp_growth_annual + legal_tradition + 
       
       currentlegalstatus + year, data = long.data, model = "mnl")

summary(my_gmnl)

stargazer(my_model, title = "Regression Results - Multinomial Logit Model- Full Data", 
          font.size = "footnotesize", header = FALSE, omit = c("FS:year.", "SF:year.", "SS:year."))
