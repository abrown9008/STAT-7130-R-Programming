## STAT 7130 Tidy Data Analysis R Code ##

library(tidyverse)

## Compare Mean Batting Average Between 1985 & 1995 ##

library(Lahman)

## Filter Out those with 0 or 1 BA ##

bats <- Batting |>
  battingStats() |>
  filter(between(BA,0.001,0.999),
                yearID == 1985 | yearID == 1995) |>
  select(yearID,BA)

## Check that it worked ##

table(bats$yearID)
sum(table(bats$yearID))

## Assess Normality Assumption ##

## One Traditional Approach ##

## Histograms ##

bats |>
  ggplot(aes(x=BA)) + 
  geom_histogram(aes(y=after_stat(density)),bins=15,
                 fill='white',color='blue') + 
  geom_density() + facet_wrap(~yearID) +
  theme_minimal()

## QQ Plots ##

bats85 <- bats |> filter(yearID == 1985)
bats95 <- bats |> filter(yearID == 1995)

qqnorm(bats85$BA)
qqline(bats85$BA)

qqnorm(bats95$BA)
qqline(bats95$BA)

## Shapiro-Wilk Normality Test ##

shapiro.test(bats85$BA)

shapiro.test(bats95$BA)

## Equality of Variances ##

var.test(bats85$BA,bats95$BA)

## Perform t-test ##

t.test(BA~yearID,data=bats)

## Perform Wilcoxon Signed Rank Test ##

wilcox.test(BA~yearID,data=bats)

## Visualize BA Difference using a Boxplot ##

bats |>
  ggplot() + geom_boxplot(aes(x=as.factor(yearID),y=BA)) +
  theme_minimal()

## Different Approach Using rstatix & ggpubr ##

library(ggpubr)
library(rstatix)

## Let's first examine summary statistics ##

bats |>
  group_by(yearID) |>
  get_summary_stats(BA,type='common')

## Check Normality Assumption ##

bats$year <- as.factor(bats$yearID)

## Overlay Histograms ##

bats |>
  gghistogram(x = "BA",bins=15,
            add = "mean", rug = TRUE,
            color = "year", fill = "year")

## Overlay Density ##

bats |>
  ggdensity(x = "BA",
          add = "mean", rug = TRUE,
          color = "year", fill = "year")

## QQ Plots ##

bats |>
  ggqqplot(x = "BA",facet.by="year")

## Shapiro-Wilk Normality Test ##

bats |>
  group_by(year) |>
  shapiro_test(BA)

## Equality of Variances ##

bats |>
  levene_test(BA~year)

## Perform the Tests ##

ttest <- bats |>
  t_test(BA~year,var.equal=FALSE) |>
  add_significance()

wtest <- bats |>
  wilcox_test(BA~year) |>
  add_significance()

## Publication Ready Graphical Comparison!! ##

bxp <- bats |>
  ggboxplot(x = "year", y = "BA", 
  ylab = "Batting Average", xlab = "Year"
)

bxp

# Add p-value and significance levels

ttest <- ttest |> 
  add_xy_position(x = "year")

bxp + 
  stat_pvalue_manual(ttest, tip.length = 0) +
  labs(subtitle = get_test_label(ttest, detailed = TRUE))

## Now, add a third year, 2005 ##

## Compare Mean Batting Average Between 1985, 1995 & 2005 ##

## Filter Out those with 0 or 1 BA ##

bats <- Batting |>
  battingStats() |>
  filter(between(BA,0.001,0.999),
                yearID == 1985 | yearID == 1995 | yearID == 2005) |>
  select(yearID,BA)

table(bats$yearID)
sum(table(bats$yearID))

## Let's first examine summary statistics ##

bats |>
  group_by(yearID) |>
  get_summary_stats(BA,type='common')

## For ANOVA, we can check the normality assumption 2 ways ##

## (1) Individually Assessing Each Group ##

## Check Normality Assumption ##

bats$year <- as.factor(bats$yearID)

## Overlay Histograms ##

bats |>
  gghistogram(x = "BA",bins=15,
              add = "mean", rug = TRUE,
              color = "year", fill = "year")

## Overlay Density ##

bats |>
  ggdensity(x = "BA",
                    add = "mean", rug = TRUE,
                    color = "year", fill = "year")

## QQ Plots ##

bats |>
  ggqqplot(x = "BA",facet.by="year")

## Shapiro-Wilk Normality Test ##

bats |>
  group_by(year) |>
  shapiro_test(BA)

## Levene's Test ##

bats |>
  levene_test(BA~year)

## (2) Check Assumption on Model Residuals ##

## Build the linear model

mod  <- aov(BA ~ year, data = bats)
mod

# Create a QQ plot of residuals

mod$residuals |>
  ggqqplot()

## Shapiro-Wilk ##

mod$residuals |>
  shapiro_test()

## Visual Assessment of Constant Variance ##

## The way we learned in regression ##

ggplot() + geom_point(aes(fitted(mod),rstudent(mod))) + theme_classic()

## Doesn't really work as well for ANOVA! ##

## Levene's Test would be better ##

library(car)

leveneTest(BA~year,data=bats)

## or ##

bats |>
  levene_test(BA~year)

## Non-base Method of Building ANOVA Model ##

mod1 <- bats |>
  anova_test(BA~year)

mod1

## Kruskal-Wallis using rstatix ##

npmod <- bats |>
  kruskal_test(BA~year)

npmod

## Tukey's HSD Post-Hoc Test ##

hsd <- bats |>
  tukey_hsd(BA~year)

hsd

## Dunn's Nonparametric Post-Hoc Test ##

dunn <- bats |>
  dunn_test(BA~year)

dunn

## Creating Publication Ready Graphic ##

hsd <- hsd |> 
  add_xy_position(x = "year")

bats |>
  ggboxplot(x = "year", y = "BA") +
    stat_pvalue_manual(hsd, hide.ns = TRUE) +
    labs(
    subtitle = get_test_label(mod1, detailed = TRUE),
    caption = get_pwc_label(hsd)
  )

## Build Mean Plot with Error Bars ##

summz <- bats |>
  group_by(yearID) |>
  get_summary_stats(BA)

summz |>
  ggplot(aes(factor(yearID),mean)) +
  geom_errorbar(aes(ymin=mean-ci,ymax=mean+ci),width=0.1) +
  geom_point() +
  labs(x = "Year",
       y = "Batting Average") +
  theme_classic() + 
  ggtitle("Mean Batting Average by Year",subtitle="with error bars") +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

## Two-Way ANOVA Example ##

library(datarium)

data('jobsatisfaction')

jobsatisfaction |>
  glimpse()

## Obtain Summary Statistics ##

jobsatisfaction |>
  group_by(gender, education_level) |>
  get_summary_stats(score,type='common')

## Create Boxplot to Visualize Group Differences ##

bxp <- jobsatisfaction |>
  ggboxplot(
  x = "gender", y = "score",
  color = "education_level", palette = "jco"
)

bxp

## Checking Assumptions ##

## Normality ##

# Build the linear model

model <- aov(score ~ gender*education_level,
             data = jobsatisfaction)

model |>
  broom::tidy()

# Create a QQ plot of residuals

model$residuals |>
  ggqqplot()

# Compute Shapiro-Wilk test of normality

model$residuals |>
  shapiro_test()

# Check Normality Assumption by Group #

jobsatisfaction |>
  group_by(gender, education_level) |>
  shapiro_test(score)

# Examine Individual QQ-Plots #

jobsatisfaction |>
  ggqqplot("score", ggtheme = theme_bw()) +
  facet_grid(gender ~ education_level)

# Constant Variance Assumption #

jobsatisfaction |> 
  levene_test(score ~ gender*education_level)

# New Model using rstatix #

two_way_aov <- jobsatisfaction |> 
  anova_test(score ~ gender * education_level)

two_way_aov

# Post Hoc Tests #

## Because we have a significant interaction effect, this means that 
## the difference between males and females is different across the 
## levels of education_level...
## Thus, running separate one-way ANOVAs for each gender may be appropriate 

jobsatisfaction |>
  group_by(gender) |>
  anova_test(score ~ education_level, error = model)

## Note, it is advisable to use the Bonferroni p-value adjustment here ##

## Now, let's compare education level by gender using the estimated marginal means

library(emmeans)

pwc <- jobsatisfaction |> 
  group_by(gender) |>
  emmeans_test(score ~ education_level, p.adjust.method = "bonferroni") 

pwc

## Nice Visual for Publication ##

# Visualization: box plots with p-values

pwc <- pwc |> 
  add_xy_position(x = "gender")

bxp +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(two_way_aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

## Means w/Error Bars ##

two_way_summz <- jobsatisfaction |>
  group_by(gender, education_level) |>
  get_summary_stats(score,type='common')

two_way_summz |>
ggplot(aes(x=gender, y=mean, color=education_level)) + 
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width=.1) +
  geom_point() +
  theme_classic()