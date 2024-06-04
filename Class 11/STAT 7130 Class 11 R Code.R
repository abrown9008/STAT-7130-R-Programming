## STAT 7130 Class 11 R Code ##

library(tidyverse)

## Read in Baseball Data ##

baseball <- readxl::read_xlsx("Baseball Data.xlsx")

## Check Data Integrity ##

summary(baseball)

## Select Subset of Columns ##

baseball_sub <- baseball %>%
  dplyr::select(Wins,HR,RBI,SB,CS,BB,SO,BA)

## Create Scatterplot Matrix ##

## Base Method ##

pairs(baseball_sub)

## This is sort of useful for a small number of variables, but even for
## a medium amount of variables, it isn't super practical 

## Two Alternatives ##

## (1): Correlation Matrix Output to Excel ##

cm <- cor(baseball_sub)

cm

openxlsx::write.xlsx(cm,"cm.xlsx")

## This is a lot more useful! How can we do the same thing inside of R?? ##

## (2): Correlation Heat Map ##

library(GGally)

GGally::ggcorr(baseball_sub) +
  ggtitle("Visualizing Correlation Matrix as a Heatmap") +
  theme(plot.title=element_text(hjust=0.5))

## Cool right? ##

## GGally has another useful function that provides much more info ##

GGally::ggpairs(baseball_sub) +
  ggtitle("More Informative Scatterplot Matrix") + 
  theme(plot.title=element_text(hjust=0.5))

## Academic-Style Approach to Regression Analysis ##

## Fit the Full Model ##

mod <- lm(Wins~.,data=baseball_sub)

## Check Normality Assumption ##

mod$residuals %>%
  ggpubr::ggqqplot()

mod$residuals %>%
  rstatix::shapiro_test()

## Check Constant Variance Assumption ##

ggplot() + geom_point(aes(fitted(mod),rstudent(mod))) +
  theme_classic()

lmtest::bptest(mod)

## Last Assumption: Multicollinearity ##

car::vif(mod)

## Typically, VIF > 10 is the cutoff ##

## One Strategy: Centering Variables about their Means ##

baseball_1 <- baseball_sub %>%
  dplyr::select(-Wins)

baseball_2 <- as_tibble(apply(baseball_1,2,FUN=function(x){
  
  return(scale(x,scale=FALSE))
  
}))

baseball_2$Wins <- baseball_sub$Wins

## Refit Model and Recheck Assumptions ##

mod1 <- lm(Wins~.,data=baseball_2)

## Check Normality Assumption ##

mod1$residuals %>%
  ggpubr::ggqqplot()

mod1$residuals %>%
  rstatix::shapiro_test()

## Check Constant Variance Assumption ##

ggplot() + geom_point(aes(fitted(mod1),rstudent(mod1))) +
  theme_classic() 

lmtest::bptest(mod1)

## Last Assumption: Multicollinearity ##

car::vif(mod1)

## Didn't Work ##

## Alternative Strategy: Drop HR or RBI ##

baseball_3 <- baseball_sub %>%
  dplyr::select(-HR)

## Refit Model and Recheck Assumptions ##

mod2 <- lm(Wins~.,data=baseball_3)

## Check Normality Assumption ##

mod2$residuals %>%
  ggpubr::ggqqplot()

mod2$residuals %>%
  rstatix::shapiro_test()

## Check Constant Variance Assumption ##

ggplot() + geom_point(aes(fitted(mod2),rstudent(mod2))) +
  theme_classic()

lmtest::bptest(mod2)

## Last Assumption: Multicollinearity ##

car::vif(mod2)

## Okay Great! Now we can check out the ANOVA Table ##

summary(mod2)

## The Output is a bit rough and for sure not tidy ##

## ANOVA Table ##

mod2 %>%
  moderndive::get_regression_table()

## Fit Indices ##

mod2 %>%
  moderndive::get_regression_summaries()

## Get the same thing but in plain English using report ##

report::report(mod2)

## One Visual of Regression Coefficients ##

p <- mod2 %>%
  moderndive::get_regression_table() %>%
  dplyr::mutate(Sig = if_else(p_value < 0.05,"Significant",
                              "Non-Significant")) %>%
  ggplot(aes(x=term,y=estimate)) +
  geom_errorbar(aes(ymin=lower_ci,ymax=upper_ci,color=factor(Sig)),width=0.1) +
  geom_point(aes(color=factor(Sig))) +
  geom_hline(aes(yintercept = 0),color="black",linetype="dashed") +
  labs(x = "Variable",
       y = "Estimated Coefficient",
       color = "Statistical \n Significance") +
  theme_classic() + coord_flip() +
  ggtitle("Confidence Intervals for Regression Analysis") +
  theme(plot.title = element_text(hjust=0.5))

p

## Okay so for such wildly different scales, this might not be so effective as 
## a static plot. But could we make it dynamic? Especially for use in a website??

library(plotly)

plotly::ggplotly(p)

## Dummy Coding Example ##

## Let's see if we can predict bill length using bill depth and species ##

## Fit the Model ##

penguins <- palmerpenguins::penguins

pmod <- lm(bill_length_mm~bill_depth_mm + species,data=penguins)

summary(pmod)

## What R does by default is chooses the alphabetically first level to be the one left out ##

## If we want to change that, we can do it using relevel ##

penguins$species <- relevel(penguins$species,ref="Gentoo")

pmod <- lm(bill_length_mm~bill_depth_mm + species,data=penguins)

summary(pmod)

report::report(pmod)

## Check Assumptions ##

## Check Normality Assumption ##

pmod$residuals %>%
  ggpubr::ggqqplot()

pmod$residuals %>%
  rstatix::shapiro_test()

## Check Constant Variance Assumption ##

ggplot() + geom_point(aes(fitted(pmod),rstudent(pmod))) +
  theme_classic()

## Last Assumption: Multicollinearity ##

car::vif(pmod)

## Visualize the Fitted Models ##

predicted_df <- tibble(pred = predict(pmod, penguins), 
                       species=penguins$species,
                       bill_depth_mm = penguins$bill_depth_mm)

ggplot(penguins, aes(bill_depth_mm,bill_length_mm)) + 
  geom_point(aes(color=species)) +
  geom_line(data = predicted_df, 
            aes(y=pred,x=bill_depth_mm,color=species)) +
  labs(x = 'Bill Depth (in mm)',
       y = 'Bill Length (in mm)',
       color = "Penguin Species") +
  theme_classic()
