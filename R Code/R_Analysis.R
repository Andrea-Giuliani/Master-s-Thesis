#Packages
library(tidyverse)
library(tidytext)
library(readxl)
library(stringr)
library(dplyr) 
library(essurvey) 
library(RCurl) 
library(RColorBrewer)
library(gridExtra)
library(texreg)
library(kableExtra)
library(d3heatmap)
library(formattable)
library(car)
library(gdata)
library(lmtest)
library(olsrr)

#Load Dataset
data<-read.csv("ESS_Dataframe.csv")

#Recode Missing
data<- recode_missings(data)

#Recode N.A. for selected variables
data<-data[is.na(data$impenv)==F,]
data<-data[is.na(data$imbgeco)==F,]
data<-data[is.na(data$gincdif)==F,]
data<-data[is.na(data$trstlgl)==F,]
data<-data[is.na(data$trstplc)==F,]
data<-data[is.na(data$ipstrgv)==F,]
data<-data[is.na(data$ipeqopt)==F,]
data<-data[is.na(data$ipudrst)==F,]
data<-data[is.na(data$trstplt)==F,]
data<-data[is.na(data$freehms)==F,]

#Changes in Public Opinion Environment
#Recoding the variables for environemnt
data$impenv = recode(data$impenv, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=0; 8=0; 9=0")
data<-data[which(data$impenv>0),]
av_impenv<-tapply(data$impenv, data$essround, mean)
years<-c(2002,2004,2006,2008,2010,2012,2014,2016)
plot1<-data.frame(years, av_impenv)
ggplot(plot1, aes(x= years, y= av_impenv)) + 
  geom_point() + geom_line() + labs(title="Importance to care for nature and environment (1=Not important, 6=Very important)", y="Yearly Mean", x = "Years")+ 
  scale_x_continuous(breaks = years) +
  theme_classic()

#Changes in Public Opinion Immigration
av_imbgeco<-tapply(data$imbgeco, data$essround, mean)
years<-c(2002,2004,2006,2008,2010,2012,2014,2016)
plot2<-data.frame(years, av_imbgeco)
ggplot(plot2, aes(x= years, y= av_imbgeco)) + 
  geom_point() + geom_line() + labs(title="Immigration bad or good for country's economy (0=Bad, 10=Good)", y="Yearly Mean", x = "Years")+ 
  scale_x_continuous(breaks =years) +
  theme_classic()

#Changes in Public Opinion Welfare State
#Recoding the variables for Welfare State
data$gincdif = recode(data$gincdif, "1=5; 2=4; 3=3; 4=2; 5=1; 7=0; 8=0; 9=0")
data<-data[which(data$gincdif>0),]
av_gincdif<-tapply(data$gincdif, data$essround, mean)
years<-c(2002,2004,2006,2008,2010,2012,2014,2016)
plot3<-data.frame(years,av_gincdif)
ggplot(plot3, aes(x= years, y= av_gincdif)) + 
  geom_point() + geom_line() + labs(title="Government should reduce differences in income levels (1=Disagree, 5=Agree)", y="Yearly Mean", x = "Years")+ 
  scale_x_continuous(breaks = years) +
  theme_classic()

#Changes in Public Opinion Trust Legal System
av_trstlgl<-tapply(data$trstlgl, data$essround, mean)
years<-c(2002,2004,2006,2008,2010,2012,2014,2016)
plot4<-data.frame(years,av_trstlgl)
ggplot(plot4, aes(x= years, y= av_trstlgl)) + 
  geom_point() + geom_line() + labs(title="Trust in the legal system (0=No trust, 10=Complete Trust)", y="Yearly Mean", x = "Years")+ 
  scale_x_continuous(breaks = years) +
  theme_classic()

#Changes in Public Opinion Trust Police
av_trstplc<-tapply(data$trstplc, data$essround, mean)
years<-c(2002,2004,2006,2008,2010,2012,2014,2016)
plot5<-data.frame(years,av_trstplc)
ggplot(plot5, aes(x= years, y= av_trstplc)) + 
  geom_point() + geom_line() + labs(title="Trust in the police (0=No trust, 10=Complete Trust)", y="Yearly Mean", x = "Years")+ 
  scale_x_continuous(breaks = years) +
  theme_classic()

#Changes in Public Opinion Safety
#Recoding the variables for safety
data$ipstrgv = recode(data$ipstrgv, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=0; 8=0; 9=0")
data<-data[which(data$ipstrgv>0),]
av_ipstrgv<-tapply(data$ipstrgv, data$essround, mean)
years<-c(2002,2004,2006,2008,2010,2012,2014,2016)
plot6<-data.frame(years,av_ipstrgv)
ggplot(plot6, aes(x= years, y= av_ipstrgv)) + 
  geom_point() + geom_line() + labs(title="Important that government is strong and ensures safety (1=Not Important, 6=Very Important)", y="Yearly Mean", x = "Years")+ 
  scale_x_continuous(breaks = years) +
  theme_classic()

#Changes in Public Opinion Equality
#Recoding the variables for equality
data$ipeqopt = recode(data$ipeqopt, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=0; 8=0; 9=0")
data<-data[which(data$ipeqopt>0),]
av_ipeqopt<-tapply(data$ipeqopt, data$essround, mean)
years<-c(2002,2004,2006,2008,2010,2012,2014,2016)
plot7<-data.frame(years,av_ipeqopt)
ggplot(plot7, aes(x= years, y= av_ipeqopt)) + 
  geom_point() + geom_line() + labs(title="Important that people are treated equally, equal opportunities (1=Not Important, 6=Very Important)", y="Yearly Mean", x = "Years")+ 
  scale_x_continuous(breaks = years) +
  theme_classic()

#Changes in Public Opinion Internationality
#Recoding the variables for internationality
data$ipudrst = recode(data$ipudrst, "1=6; 2=5; 3=4; 4=3; 5=2; 6=1; 7=0; 8=0; 9=0")
data<-data[which(data$ipudrst>0),]
av_ipudrst<-tapply(data$ipudrst, data$essround, mean)
years<-c(2002,2004,2006,2008,2010,2012,2014,2016)
plot8<-data.frame(years,av_ipudrst)
ggplot(plot8, aes(x= years, y= av_ipudrst)) + 
  geom_point() + geom_line() + labs(title="Important to be open to internationality (1=Not Important, 6=Very Important)", y="Yearly Mean", x = "Years")+ 
  scale_x_continuous(breaks = years) +
  theme_classic()

#Changes in Public Opinion Gays and Lesbians Rights
#Recoding the variables for gays and lesbians rights
data$freehms = recode(data$freehms, "1=5; 2=4; 3=3; 4=2; 5=1; 7=0; 8=0; 9=0")
data<-data[which(data$freehms>0),]
av_freehms<-tapply(data$freehms, data$essround, mean)
years<-c(2002,2004,2006,2008,2010,2012,2014,2016)
plot9<-data.frame(years,av_freehms)
ggplot(plot9, aes(x= years, y= av_freehms)) + 
  geom_point() + geom_line() + labs(title="Importance of gays and lesbians rights (1=Not Important, 5=Very Important)", y="Yearly Mean", x = "Years")+ 
  scale_x_continuous(breaks = years) +
  theme_classic()

#Changes in Public Opinion Trust in Politicians
av_trstplt<-tapply(data$trstplt, data$essround, mean)
years<-c(2002,2004,2006,2008,2010,2012,2014,2016)
plot10<-data.frame(years,av_trstplt)
ggplot(plot10, aes(x= years, y= av_trstplt)) + 
  geom_point() + geom_line() + labs(title="Trust in politicians (0=No trust, 10=Complete Trust)", y="Yearly Mean", x = "Years")+ 
  scale_x_continuous(breaks = years) +
  theme_classic()

#Additional information regarding expenditure
#Environment
av_impenv<-av_impenv[1:8]
years<-c(2002, 2004, 2006,2008,2010,2012,2014,2016)
exp_impenv<-c(43717.5, 48285.6, 54596.4, 53059, 56647.3, 59096.2, 56584.5, 57411.8)
data_env<-data.frame(av_impenv,exp_impenv,years)

#Immigration
av_imbgeco<-av_imbgeco[1:8]
exp_imbgeco<-c(1039.6, 1174.7, 1472.7, 1225, 1373, 2374.5, 1711, 3077.3)
data_imm<-data.frame(av_imbgeco, exp_imbgeco, years)

#Welfare State
av_gincdif<-av_gincdif[1:8]
exp_gincdif<-c(10340.6, 35661.2, 34881.6, 45093, 48828, 61584.7, 67682.9, 56265)
data_ws<-data.frame(av_gincdif, exp_gincdif, years)

#Trust in the legal system
av_trstlgl<-av_trstlgl[1:8]
exp_trstlgl<-c(1039.6, 1174.7, 1472.7, 1225, 1373, 2374.5, 1711, 3077.3)
data_tls<-data.frame(av_trstlgl, exp_trstlgl, years)

#Trust in the police
av_trstplc<-av_trstplc[1:8]
exp_trstplc<-c(1039.6, 1174.7, 1472.7, 1225, 1373, 2374.5, 1711, 3077.3)
data_plc<-data.frame(av_trstplc, exp_trstplc, years)

#Safety
av_ipstrgv<-av_ipstrgv[1:8]
exp_ipstrgv<-c(1039.6, 1174.7, 1472.7, 1225, 1373, 2374.5, 1711, 3077.3)
data_saf<-data.frame(av_ipstrgv, exp_ipstrgv, years)

#Equality
av_ipeqopt<-av_ipeqopt[1:8]
exp_ipeqopt<-c(1039.6, 1174.7, 1472.7, 1225, 1373, 2374.5, 1711, 3077.3)
data_eq<-data.frame(av_ipeqopt, exp_ipeqopt, years)

#Internationality
av_ipudrst<-av_ipudrst[1:8]
exp_ipudrst<-c(3804.3, 3291, 3764.1, 6651.4, 8610.3, 10941.8, 15145, 8134.6)
data_in<-data.frame(av_ipudrst, exp_ipudrst, years)

#Gays and Lesbians Rights
av_freehms<-av_freehms[1:8]
exp_freehms<-c(592.4,1071.4,1361.7, 441.11, 507.6, 586.6, 305.3, 295.8)
data_gl<-data.frame(av_freehms, exp_freehms, years)

#Trust in Politicians
av_trstplt<-av_trstplt[1:8]
exp_trstplt<-c(1039.6, 1174.7, 1472.7, 1225, 1373, 2374.5, 1711, 3077.3)
data_plt<-data.frame(av_trstplt, exp_trstplt, years)

#Plot How Expenditure Changed Over The Years
data_exp<-data.frame(exp_gincdif, exp_imbgeco, exp_impenv, exp_ipeqopt, exp_ipstrgv, exp_trstlgl, exp_trstplc, 
                     exp_ipudrst, exp_freehms, exp_trstplt, years)
clean_data_exp <- data_exp %>% gather(variable, value, -years)
ggplot(data=clean_data_exp, aes(x= years, y=value, group=variable, colour=variable)) +
  geom_line() +
  geom_point( size=4, shape=21, fill="white") +
  labs(x="Years",y="Value", title="Changes in the Yearly Expenditure of the European Union per Sector") +
  facet_grid(variable~., scales="free")+
  scale_color_hue(labels = c("Gays and Lesbians Rights", "Welfare", "Immigration", "Environment", "Equality", 
                             "Safety", "Internationality", "Trust Legal System", "Trust Police", "Trust Politicians"))+
  scale_x_continuous(breaks = years)+
  theme_classic()

#Preparing Data for Creating a Heatmap
data_hm<-data.frame(data_env, data_eq, data_imm, data_plc, data_saf, data_tls, data_ws, data_in, data_gl, data_plt)
data_hm[3] <- NULL
data_hm[5] <- NULL
data_hm[7] <- NULL
data_hm[9] <- NULL
data_hm[11] <- NULL
data_hm[13] <- NULL
data_hm[15] <- NULL
data_hm[17] <- NULL
data_hm[19] <- NULL
data_hm[21] <- NULL

#Creating the Dataset for the OLS Model
data_ols<-data.frame(data_hm, years)

#Generating the Heatmap
data_hm<-scale(data_hm)
rownames(data_hm)<-c(2002, 2004,2006,2008,2010,2012,2014,2016)
colnames(data_hm)<-c("Opinion Environment", "Exp. Environment", "Opinion Equality", "Exp. Equality",
                     "Opinion Immigration", "Exp. Immigration", "Opinion Police", "Exp. Police", 
                     "Opinion Safety", "Exp. Safety", "Opinion Legal System", "Exp. Legal System", 
                     "Opinion Welfare", "Exp. Welfare", "Opinion Internationality", "Exp. Internationality",
                     "Opinion Gays & Lesbians", "Exp. Gays & Lesbians", "Opinion Politicians", "Exp. Politicians")
d3heatmap(data_hm, scale="none", Rowv= FALSE, xaxis_font_size="6pt", yaxis_font_size="10pt")

#Adding GDP, GDP per Capita, Education Level as Covariates Based on Literature 
data_ols$gdp<-c(9827.5, 13808.95, 15408.6, 19163.62, 17009.6, 17316.99, 18669.3, 16553.08)
data_ols$gdp_capita<-c(31328.5, 32215.9, 33745.9, 34720, 33729.2, 34117.7, 34617.4, 35935.1)
data_ols$edu<-c(0.17, 0.16,0.154,0.148,0.14,0.127,0.121,0.107)
percent(data_ols$edu)

#Restructuring the Dataset
sample_public_opinion <- data_ols %>% 
  pivot_longer(
    cols = c(
      "av_impenv", 
      "av_ipeqopt", 
      "av_imbgeco", 
      "av_trstplc",
      "av_ipstrgv",
      "av_trstlgl", 
      "av_gincdif", 
      "av_ipudrst",
      "av_freehms",
      "av_trstplt"
    ),
    names_to = "public_opinion_cat",
    values_to = "public_opinion"
  )

sample_expenditure <- data_ols %>% 
  pivot_longer(
    cols = c(
      "exp_impenv", 
      "exp_ipeqopt", 
      "exp_imbgeco", 
      "exp_trstplc",
      "exp_ipstrgv",
      "exp_trstlgl", 
      "exp_gincdif", 
      "exp_ipudrst",
      "exp_freehms",
      "exp_trstplt"
    ),
    names_to = "expenditure_cat",
    values_to = "expenditure"
  ) %>% 
  select(
    years, 
    expenditure_cat, 
    expenditure
  )

data_ols_tidy <- sample_expenditure %>% 
  bind_cols(sample_public_opinion)

data_ols_tidy[4:14] <- NULL

#Turning public opinion into percentages
data_ols_tidy <- data_ols_tidy %>% 
  mutate(
    public_opinion_percent = case_when(
      public_opinion_cat == "av_freehms" ~ public_opinion/5,
      public_opinion_cat == "av_gincdif" ~ public_opinion/5, 
      public_opinion_cat == "av_imbgeco" ~ public_opinion/10,
      public_opinion_cat == "av_impenv" ~ public_opinion/6,
      public_opinion_cat == "av_ipeqopt" ~ public_opinion/6,
      public_opinion_cat == "av_ipstrgv" ~ public_opinion/6,
      public_opinion_cat == "av_ipudrst" ~ public_opinion/6,
      public_opinion_cat == "av_trstlgl" ~ public_opinion/10,
      public_opinion_cat == "av_trstplc" ~ public_opinion/10,
      public_opinion_cat == "av_trstplt" ~ public_opinion/10
      )
  )

#Conducting graphical analysis of evolution of public opinion and expenditure per sector
df <- data_ols_tidy

start_year <- df %>% 
  filter(years == min(years)) %>% 
  transmute(
    public_opinion_cat, 
    index_public_opinion = public_opinion,
    index_expenditure = expenditure / gdp
  )

df <- df %>% 
  inner_join(
    start_year, 
    by = "public_opinion_cat"
  )

df <- df %>%
  mutate(
    index_public_opinion = public_opinion / index_public_opinion,
    index_expenditure = (expenditure / gdp ) / index_expenditure
  )

df <- df %>% 
  distinct(
    years, 
    index_public_opinion,
    index_expenditure,
    public_opinion_cat
  ) %>% 
  pivot_longer(cols = c(index_public_opinion, index_expenditure))

#Environment
df %>% 
  filter(public_opinion_cat == "av_impenv") %>% 
  arrange(name) %>% 
  ggplot(aes(x = years, y = value, color = name)) + 
  geom_line() + 
  scale_color_discrete(name = "Variables", labels = c("Expenditure", "Public Opinion"))+
  labs(x="Years", y="Values", title="Expenditure and Public Opinion for Environment") +
  theme_classic() 

#Immigration
df %>% 
  filter(public_opinion_cat == "av_imbgeco") %>% 
  arrange(name) %>% 
  ggplot(aes(x = years, y = value, color = name)) + 
  geom_line() + 
  scale_color_discrete(name = "Variables", labels = c("Expenditure", "Public Opinion"))+
  labs(x="Years", y="Values", title="Expenditure and Public Opinion for Immigration") +
  theme_classic() 

#Welfare State
df %>% 
  filter(public_opinion_cat == "av_gincdif") %>% 
  arrange(name) %>% 
  ggplot(aes(x = years, y = value, color = name)) + 
  geom_line() + 
  scale_color_discrete(name = "Variables", labels = c("Expenditure", "Public Opinion"))+
  labs(x="Years", y="Values", title="Expenditure and Public Opinion for Welfare State") +
  theme_classic() 

#Trust in the Legal System
df %>% 
  filter(public_opinion_cat == "av_trstlgl") %>% 
  arrange(name) %>% 
  ggplot(aes(x = years, y = value, color = name)) + 
  geom_line() + 
  scale_color_discrete(name = "Variables", labels = c("Expenditure", "Public Opinion"))+
  labs(x="Years", y="Values", title="Expenditure and Public Opinion for Trust in Legal System") +
  theme_classic() 

#Trust in the Police
df %>% 
  filter(public_opinion_cat == "av_trstplc") %>% 
  arrange(name) %>% 
  ggplot(aes(x = years, y = value, color = name)) + 
  geom_line() + 
  scale_color_discrete(name = "Variables", labels = c("Expenditure", "Public Opinion"))+
  labs(x="Years", y="Values", title="Expenditure and Public Opinion for Trust in Police") +
  theme_classic() 

#Safety
df %>% 
  filter(public_opinion_cat == "av_ipstrgv") %>% 
  arrange(name) %>% 
  ggplot(aes(x = years, y = value, color = name)) + 
  geom_line() + 
  scale_color_discrete(name = "Variables", labels = c("Expenditure", "Public Opinion"))+
  labs(x="Years", y="Values", title="Expenditure and Public Opinion for Safety") +
  theme_classic() 

#Equality
df %>% 
  filter(public_opinion_cat == "av_ipeqopt") %>% 
  arrange(name) %>% 
  ggplot(aes(x = years, y = value, color = name)) + 
  geom_line() + 
  scale_color_discrete(name = "Variables", labels = c("Expenditure", "Public Opinion"))+
  labs(x="Years", y="Values", title="Expenditure and Public Opinion for Equality") +
  theme_classic() 

#Internationality
df %>% 
  filter(public_opinion_cat == "av_ipudrst") %>% 
  arrange(name) %>% 
  ggplot(aes(x = years, y = value, color = name)) + 
  geom_line() + 
  scale_color_discrete(name = "Variables", labels = c("Expenditure", "Public Opinion"))+
  labs(x="Years", y="Values", title="Expenditure and Public Opinion for Internationality") +
  theme_classic() 

#Trust in Politicians
df %>% 
  filter(public_opinion_cat == "av_trstplt") %>% 
  arrange(name) %>% 
  ggplot(aes(x = years, y = value, color = name)) + 
  geom_line() + 
  scale_color_discrete(name = "Variables", labels = c("Expenditure", "Public Opinion"))+
  labs(x="Years", y="Values", title="Expenditure and Public Opinion for Trust in Politicians") +
  theme_classic() 

#Gay and Lesbian Rights
df %>% 
  filter(public_opinion_cat == "av_freehms") %>% 
  arrange(name) %>% 
  ggplot(aes(x = years, y = value, color = name)) + 
  geom_line() + 
  scale_color_discrete(name = "Variables", labels = c("Expenditure", "Public Opinion"))+
  labs(x="Years", y="Values", title="Expenditure and Public Opinion for Gays and Lesbians' Rights") +
  theme_classic() 

#Creating Log Variables
data_ols_tidy$lexpenditure<-log(data_ols_tidy$expenditure)
data_ols_tidy$lgdp<-log(data_ols_tidy$gdp)
data_ols_tidy$lgdp_capita<-log(data_ols_tidy$gdp_capita)
d <- data_ols_tidy %>% select(lexpenditure, public_opinion_percent, lgdp, lgdp_capita, edu)
head(d)

#OLS without fixed effects
ols1<-lm(lexpenditure~public_opinion_percent+lgdp+lgdp_capita+edu, data=data_ols_tidy)
screenreg(ols1, custom.coef.names=c("Intercept","Public Opinion", "GDP", "GDP per capita", "Education"),
          custom.model.names = c("Model without Fixed Effects"))

#Informative plots
plot(ols1)

#Checking for collinearity between GDP and GDP per capita
ols_vif_tol(ols1)

#OLS with fixed effects
ols2<-lm(lexpenditure~public_opinion_percent+lgdp+lgdp_capita+edu+factor(public_opinion_cat), data=data_ols_tidy)
screenreg(ols2, custom.coef.names=c("Intercept","Public Opinion", "GDP", "GDP per capita", "Education", "Welfare State",
                                    "Immigration", "Environment", "Equality", "Safety", "Internationality", "Trust Legal System",
                                    "Trust Police", "Trust Politicians"),
          custom.model.names = c("Model with Fixed Effects"))

#Informative plots
plot(ols2)

#Fit the model
fit <- lm(lexpenditure~public_opinion_percent+lgdp+lgdp_capita+edu+factor(public_opinion_cat), data=data_ols_tidy)

#Obtain predicted and residual values
d$predicted <- predict(fit)
d$residuals <- residuals(fit)

#Plot for checking the predictive power of the model 
ggplot(d, aes(x = public_opinion_percent, y = lexpenditure)) +
  geom_segment(aes(xend = public_opinion_percent, yend = predicted)) +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  labs(x="Public Opinion", y="Log of Expenditure", title="Predictive Power of the Ordinary Least Squares Model with Fixed Effects")+
  theme_classic()

#Difference-in-Differences
#Data
Difindif<-read.csv("ESS_Germany_France_Dataframe.csv")

#Recode Missing
Difindif<-recode_missings(Difindif)

#Order by Country
Difindif%>%arrange(cntry)

#Recode N.A. for selected variables
Difindif<-Difindif[is.na(Difindif$imbgeco)==F,]

#Dummy for being German
Difindif<-Difindif%>%
  mutate(German=ifelse(cntry == "DE", 1, 0))

#Recode essround
Difindif$essround = recode(Difindif$essround, "1=2002; 2=2004; 3=2006; 4=2008; 5=2010; 6=2012; 7=2014; 8=2016")

#Dummy for the treatment
Difindif = Difindif %>%
  mutate(Postcrisis = essround > 2014)

#Rename variables
Difindif<-Difindif%>%
  rename(Years=essround,
         Immigration=imbgeco,
         Country=cntry)

#Graphic representation to check for parallel trends assumption
ggplot(Difindif, aes(Years, Immigration, color = Country)) +
  stat_summary(geom = 'line') +
  geom_vline(xintercept = 2014) +
  theme_minimal()+
  labs(x="Years", y="Public Opinion for Immigration", title="Testing the Parallel Trends Assumption")

#Check for outliers problem
ggplot(Difindif, aes(x=Immigration)) + geom_histogram()+
  labs(x="Public Opinion for Immigration", y="Frequency", title="Histogram to Check for Possible Outliers in the Model")+
  theme_classic()

#Difference-in-differences
model = lm(Immigration ~ German*Postcrisis, data = Difindif %>%
             filter(Immigration<=10))
screenreg(model, custom.coef.names = c("Intercept", "Being German", "Post Refugee Crisis", "Interaction Term"),
          custom.model.names = c("Difference-in-Differences"))
