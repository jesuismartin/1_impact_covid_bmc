### 16.2.2021
## Rerunning all regressions - Contract change issue
library(tidyverse)
library(corrplot)
library(Hmisc)
library(MASS)
library(epiDisplay)
library(mice)
library(VIM)
library(skimr)

df <- read.csv("~/Desktop/PhD/Papers/Paper 1/BMC Public Health/R/Covid-19/data/Wave4_edu_na.csv", header=TRUE)

df$Ovrl_HO <- as.factor(df$Ovrl_HO)

skim(df$Ovrl_HO)
tab1(df$Ovrl_HO)

## Data cleaning
df$Gender <- as.factor(df$Gender)
df$Liv.situation <- as.factor(df$Liv.situation)
df$Contract.change <- as.factor(df$Contract.change)
df <- df %>%
  mutate_if(is.character, as.factor)
df$Work.wors <- as.ordered(df$Work.wors)
df$Work.imp <- as.ordered(df$Work.imp)
df <- df %>%
  mutate_at(vars(Privat.wors, Privat.imp), as.ordered)
df <- df %>%
  mutate_at(vars(Work.wors.3, Work.imp.3, Privat.wors.3, Privat.imp.3), as.factor)

df$Contract.change <- relevel(df$Contract.change, "No change")
df$Ovrl_HO <- relevel(df$Ovrl_HO, "No HO")
df$Work_time <- relevel(df$Work_time, "Decreased")
df$Freetime <- relevel(df$Freetime, "Decreased")
df$Caring_duties <- relevel(df$Caring_duties, "Decreased")
levels(df$Age.cat)

## Correlation matrix
df2 <- df %>%
  dplyr::select(Gender, Age.num, Country, Liv.situation, Contract.change, Ovrl_HO, Work.quant, Care.quant, Free.quant, Work.wors, Work.imp, Privat.wors, Privat.imp, mwb.cat, srh)

df2[,] = lapply(df2[,], as.numeric)

colnames(df2) <- c("Gender", "Age", "Country", "Living situation", "Contract change", "Home-office", "Work time", "Caring duties", "Leisure time", "Work worsened", "Work improved", "Private worsened", "Private improved", "MWB", "SRH")
col_order <- c("Gender", "Age", "Country", "Living situation", "Contract change", "Work time", "Leisure time", "Caring duties", "Home-office", "Work worsened", "Work improved", "Private worsened", "Private improved", "MWB", "SRH")
df2 <- df2[, col_order]

mat1 <- rcorr(as.matrix(df2))

mat2 <- cor(df2)

corrplot(mat2, method = "number", type = "upper", tl.col = "black")
corrplot(mat1$r, method = "number", type = "upper", tl.col = "black", 
         p.mat = mat1$P, sig.level = 0.01, insig = "blank")

## Merging datasets for education
df$education <- Covid_wave456$s103.6

nf <- df %>%
  select(ID.3, ID.2, edu)
nf <- nf %>% 
  rename(
    ID.3 = ID2020,
    ID.2 = ID2018
  )
mf <- merge(df, nf, by.x = "ID.3", by.y = "ID.3", all.x = TRUE)

summary(df2$Education)

## Imputing missing data for Education
library(mice)
library(VIM)
df$edu.na <- Wave4_education$s103.6
md.pattern(df)
md.pairs(df)
marginplot(df2[,c("Education", "MWB")])

df1 <- df %>%
  dplyr::select(-c(ID.2, ID.3, ho.before, ho.after, Work_time, Caring_duties, Freetime, Age.cat, Work.wors.3, Work.imp.3, Privat.wors.3, Privat.imp.3))

impute <- mice(df1)
print(impute)
impute$imp$edu

new_df <- complete(impute, 1)

tab1(df$edu.na)
df$edu.b <- new_df$edu

## Wave 4 analysis with imputed education data
new_df2 <- new_df
new_df2[,] = lapply(new_df2[,], as.numeric)

colnames(new_df2) <- c("Gender", "Living situation", "Work worsened", "Work improved", "Private worsened", "Private improved", "Contract change", "Work time", "Caring duties", "Leisure time", "MWB", "Home-office",  "SRH", "Country", "Age", "Education")
col_order <- c("Gender", "Age", "Country", "Education", "Living situation", "Contract change", "Work time", "Leisure time", "Caring duties", "Home-office", "Work worsened", "Work improved", "Private worsened", "Private improved", "MWB", "SRH")
new_df2 <- new_df2[, col_order]

mat2 <- rcorr(as.matrix(new_df2))
mat2

corrplot(mat2$r, method = "number", type = "upper", tl.col = "black", 
         p.mat = mat2$P, sig.level = 0.01, insig = "blank")

## Ordinal regression + education

df$edu.na <- recode(df$edu.na, "2" = 1, "3" = 2, "4" = 2, "5" = 3, "6" = 3)

df$edu.na <- factor(df$edu.na, levels = c("1", "2", "3"),
                     labels = c("Primary", "Secondary", "Tertiary"))
df$srh <- as.ordered(df$srh)
df$mwb.cat <- as.ordered(df$mwb.cat)
df$edu.na <- Wave4_education$s103.6

# Work life worsened
ord1 <- polr(Work.wors ~ Gender + Country + Age.cat + Liv.situation + relevel(Contract.change, ref = "No change") + relevel(Ovrl_HO, ref = "None") + relevel(Work_time, ref = "Unchanged") + relevel(Freetime, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = df, Hess = TRUE)
exp(coefficients(ord1))
exp(confint(ord1))

# Work life improved
ord2 <- polr(Work.imp ~ Gender + Country + Age.cat + edu_cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = df, Hess = TRUE)
summary(ord2)
exp(coefficients(ord2))
exp(confint(ord2))

# Private life worsened
ord3 <- polr(Privat.wors ~ Gender + Country + Age.cat + edu_cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = df, Hess = TRUE)
summary(ord3)
exp(coefficients(ord3))
exp(confint(ord3))

# Private life improved
ord4 <- polr(Privat.imp ~ Gender + Country + Age.cat + edu_cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = df, Hess = TRUE)
summary(ord4)
exp(coefficients(ord4))
exp(confint(ord4))

# Mental well-being
ord5 <- polr(mwb.cat ~ Gender + Country + Age.cat + edu_cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged") + Work.wors.3 + Work.imp.3 + Privat.wors.3 + Privat.imp.3, data = df, Hess = TRUE)
summary(ord5)
exp(coefficients(ord5))
exp(confint(ord5))

# Self-rated health
ord6 <- polr(srh ~ Gender + Country + Age.cat + edu_cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged") + Work.wors.3 + Work.imp.3 + Privat.wors.3 + Privat.imp.3, data = df, Hess = TRUE)
summary(ord6)
exp(coefficients(ord6))
exp(confint(ord6))

write.csv(df, file = "Wave4_edu.csv")
df <- df3

df$Contract.change <- as.numeric(df$Contract.change)
df$Contract.change[df$Contract.change == 2] <- 5
df$Contract.change[df$Contract.change == 4] <- 2
df$Contract.change[df$Contract.change == 1] <- 4
df$Contract.change[df$Contract.change == 5] <- 1
df$Contract.change <- factor(df$Contract.change, levels = c("1", "2", "3", "4"),
                             labels = c("No change", "Short-time reduced", "Short-time 0", "Job loss"))

df$Ovrl_HO <- as.numeric(df$Ovrl_HO)
df$Ovrl_HO[df$Ovrl_HO == 3] <- 4
df$Ovrl_HO[df$Ovrl_HO == 2] <- 3
df$Ovrl_HO[df$Ovrl_HO == 1] <- 2
df$Ovrl_HO[df$Ovrl_HO == 4] <- 1
df$Ovrl_HO <- factor(df$Ovrl_HO, levels = c("1", "2", "3"),
                             labels = c("None", "Experienced", "New"))

tab1(new_df2$"Home-office")
table(df3$Work.imp)

cor(new_df2$"Home-office", new_df2$"Work improved")
tab1(df$edu.na)
rm(col_order)

xtabs(~Gender + Country, data = df)
724+905
234+255
905/1629
255/489
table(df$Gender)
1160/2118

prop.test((x = c(724)))
?chisq.test

tabx <- table(df$Gender, df$Country)
tabx
chi <- chisq.test(tabx)
summary(chi)
mean(df$)