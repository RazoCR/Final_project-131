
train=read.csv("~/Desktop/Final_project_131/Data/processed/train.csv")

test=read.csv("~/Desktop/Final_project_131/Data/processed/test.csv")

K_folds=read.csv("~/Desktop/Final_project_131/Data/processed/test.csv")


## EDA
train_nu=select_if(train,is.numeric)

## train correlation 

train_nu %>% 
  cor() %>% 
  corrplot(type = "lower")

## correlation on factor of diagnosis 

M_nu=train_nu[train[2]=='M',]

B_nu=train_nu[train[2]=='B',]

M_nu %>% 
  cor() %>% 
  corrplot(type = "lower")

B_nu %>% 
  cor() %>% 
  corrplot(type = "lower")

## see relationship look for reduce demnsion 

M_c=M_nu%>%
  cor() 

B_c=B_nu%>%
  cor() 



corr_M <- as.data.frame(as.table(M_c))
#corr=corr[lower.tri(corr,diag=TRUE)] <- NA 
corr_M=corr_M[!duplicated(corr_M$Freq), ]

corr_B<- as.data.frame(as.table(B_c))
#corr=corr[lower.tri(corr,diag=TRUE)] <- NA 
corr_B=corr_B[!duplicated(corr_B$Freq), ]


sig_m=corr_M %>%
  filter(Freq>.7 )

sig_b=corr_B %>%
  filter(Freq>.7 )


sig_M2 = sig_m%>% group_by(Var2) %>% summarise(n=n())
sig_B2 = sig_b%>% group_by(Var2) %>% summarise(n=n())



view(sig_M2)
view(sig_B2)

sig_M2 %>%
  filter(n>1)

sig_B2 %>%
  filter(n>1)


sig_m %>% filter(Freq>.98)

sig_b %>% filter(Freq>.98)

ggplot(train, aes(x =area_mean , y = radius_mean, colour = factor(diagnosis)))+ 
  geom_point(size=2.5)

ggplot(train, aes(x =area_mean , y = perimeter_mean, colour = factor(diagnosis)))+ 
  geom_point(size=2.5)

ggplot(train, aes(x =concave.points_mean , y = concavity_mean, colour = factor(diagnosis)))+ 
  geom_point(size=2.5)

ggplot(train, aes(x =area_worst , y = radius_worst, colour = factor(diagnosis)))+ 
  geom_point(size=2.5)

ggplot(train, aes(x =area_worst , y = texture_mean, colour = factor(diagnosis)))+ 
  geom_point(size=2.5)



c1=corr_M %>% filter(Freq<.20)

c2=corr_B %>% filter(Freq<.20)

sig_M2 = c1%>% group_by(Var2) %>% summarise(n=n())
sig_B2 = c2%>% group_by(Var2) %>% summarise(n=n())

## texture mean ,radius mean ,perimeter mean , area mean ,compactness mean, concavity mean

## Scatter plot to see the relaionship with high correlated relationships 

train %>% 
  ggplot(aes(x = radius_mean,colour=factor(diagnosis))) +
  geom_histogram()
train %>% 
  ggplot(aes(x = texture_mean,colour=factor(diagnosis))) +
  geom_histogram()

train %>% 
  ggplot(aes(x = area_mean,colour=factor(diagnosis))) +
  geom_histogram()

train %>% 
  ggplot(aes(x = compactness_mean,colour=factor(diagnosis))) +
  geom_histogram()
