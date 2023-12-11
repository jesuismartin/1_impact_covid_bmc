library(dplyr)
library(ggplot2)
library(lattice)
library(haven)
library(car)
library(cowplot)

Data2020wave4_cleaned <- read_sav("~/Desktop/Data2020wave4_cleaned.sav")
View(Data2020wave4_cleaned)
df <- Data2020wave4_cleaned

# Change to missing values
df$Gender[df$Gender == 3] <- NA
df$Work.position[df$Work.position == 100] <- NA
df$Liv.situation[df$Liv.situation == 100] <- NA
summary(df)
table(y$Age_new)
y <- df[complete.cases(df[ , 7]),]

z <- y

y$ho.diff <- y$ho.after - y$ho.before

complete.cases(df$Gender)

# Data cleaning
y <- as.data.frame(y)

# Converting numeric to factors
y[y$Employed == 1,]$Employed <- "Yes"
y[y$Employed == 0,]$Employed <- "No"
y$Employed <- as.factor(y$Employed)
y$Employed.before <- NULL

y$Gender <- z$Gender
y$Gender <- as.numeric(y$Gender)
y[!is.na(y$Gender) & y$Gender == 1,]$Gender <- "Male"
y[!is.na(y$Gender) & y$Gender == 2,]$Gender <- "Female"
y$Gender <- as.factor(y$Gender)

y$Age.cat <- as.numeric(y$Age.cat)
y[y$Age.cat == 1,]$Age.cat <- "18-30"
y[y$Age.cat == 2,]$Age.cat <- "31-40"
y[y$Age.cat == 3,]$Age.cat <- "41-50"
y[y$Age.cat == 4,]$Age.cat <- "51-60"
y[y$Age.cat == 5,]$Age.cat <- "61-65"
y$Age.cat <- as.factor(y$Age.cat)

y$Country_n <- as.numeric(y$Country_n)
y$Country_n <- ifelse(test = y$Country_n == 1, yes = "Switzerland", no = "Germany")
y$Country_n <- as.factor(y$Country_n)

y$Liv.situation <- z$Liv.situation
y[!is.na(y$Liv.situation) & y$Liv.situation == 1,]$Liv.situation <- "Alone"
y[!is.na(y$Liv.situation) & y$Liv.situation == 2,]$Liv.situation <- "Partner/Family/Flatshare"
y[!is.na(y$Liv.situation) & y$Liv.situation == 3,]$Liv.situation <- "Partner/Family/Flatshare"
y$Liv.situation <- as.factor(y$Liv.situation)

y$Ovrl_HO <- as.numeric(y$Ovrl_HO)
y[!is.na(y$Ovrl_HO) & y$Ovrl_HO == 1,]$Ovrl_HO <- "No HO"
y[!is.na(y$Ovrl_HO) & y$Ovrl_HO == 2,]$Ovrl_HO <- "Experienced HO"
y[!is.na(y$Ovrl_HO) & y$Ovrl_HO == 3,]$Ovrl_HO <- "New HO"
y$Ovrl_HO <- as.factor(y$Ovrl_HO)

y[!is.na(y$Caring.outside) & y$Caring.outside == 0,]$Caring.outside <- "No"
y[!is.na(y$Caring.outside) & y$Caring.outside == 1,]$Caring.outside <- "Yes"
y$Caring.outside <- as.factor(y$Caring.outside)

y$Contract.change <- as.numeric(y$Contract.change)
y[!is.na(y$Contract.change) & y$Contract.change == 0,]$Contract.change <- "No change"
y[!is.na(y$Contract.change) & y$Contract.change == 1,]$Contract.change <- "Short-time reduced"
y[!is.na(y$Contract.change) & y$Contract.change == 2,]$Contract.change <- "Short-time 0"
y[!is.na(y$Contract.change) & y$Contract.change == 3,]$Contract.change <- "Job loss"
y$Contract.change <- as.factor(y$Contract.change)

table(y$Work_time)
y$Work_time <- as.numeric(y$Work_time)
y[!is.na(y$Work_time) & y$Work_time == 1,]$Work_time <- "Decreased"
y[!is.na(y$Work_time) & y$Work_time == 2,]$Work_time <- "Unchanged"
y[!is.na(y$Work_time) & y$Work_time == 3,]$Work_time <- "Increased"
y$Work_time <- as.factor(y$Work_time)
xtabs(~ Work.quant + Work_time, data = y)

y$Freetime <- as.numeric(y$Freetime)
y[!is.na(y$Freetime) & y$Freetime == 1,]$Freetime <- "Decreased"
y[!is.na(y$Freetime) & y$Freetime == 2,]$Freetime <- "Unchanged"
y[!is.na(y$Freetime) & y$Freetime == 3,]$Freetime <- "Increased"
y$Freetime <- as.factor(y$Freetime)
xtabs(~ Free.quant + Freetime, data = y)

y$Caring_duties <- as.numeric(y$Caring_duties)
y[!is.na(y$Caring_duties) & y$Caring_duties == 1,]$Caring_duties <- "Decreased"
y[!is.na(y$Caring_duties) & y$Caring_duties == 2,]$Caring_duties <- "Unchanged"
y[!is.na(y$Caring_duties) & y$Caring_duties == 3,]$Caring_duties <- "Increased"
y$Caring_duties <- as.factor(y$Caring_duties)
xtabs(~ Care.quant + Caring_duties, data = y)

# Missing values
nrow(y[is.na(y$Liv.situation) | is.na(y$Gender),])
y[is.na(y$Liv.situation) | is.na(y$Gender),]
nrow(is.na(y))
table(is.na(y))

xtabs(~ Work.wors + Contract.change, data = y)
summary(y$Gender)
table(y$Country_n)

library(haven)
as_factor(y$Country_n,
          levels = c("Switzerland", "Germany", "Other", "Brazil"),
          ordered = FALSE,
          ...
)

# Plots
par(mfrow = c(1,2))
ho.bf.plot <- ggplot(y, aes(factor(Gender), ho.before))+
  geom_jitter()+
  theme_classic()
ho.bf.plot

ho.af.plot <- ggplot(y, aes(factor(Gender), ho.after))+
  geom_jitter()+
  theme_classic()
ho.af.plot

ho.diff.plot <- ggplot(y, aes(ho.diff))+
  geom_bar(aes(fill = Gender))
ho.diff.plot

mwb.gender.plot <- ggplot(y, aes(mwb, fill = factor(Gender)))+
  geom_density(alpha = .5)+
  theme_classic()
mwb.gender.plot

mwb.pw.plot <- ggplot(y, aes(Privat.wors, mwb))+
  geom_jitter(aes(color = factor(Country)))+
  geom_smooth(method = "lm", se = T)+
  theme_classic()
mwb.pw.plot

mwb.ww.plot <- ggplot(y, aes(Work.wors, mwb))+
  geom_jitter(aes(color = factor(Country)))+
  geom_smooth(method = "lm", se = T)+
  theme_classic()
mwb.ww.plot

mwb.age.plot <- ggplot(y, aes(Age.cat, mwb))+
  geom_boxplot()+
  theme_classic()
mwb.age.plot

srh.age.plot <- ggplot(y, aes(Age.cat, srh))+
  geom_jitter(aes(color = factor(Country)))+
  theme_classic()+
  xlab("Age groups")+
  ylab("Self-rated health")
srh.age.plot

srh.pw.plot <- ggplot(y, aes(Privat.wors, srh))+
  geom_jitter(aes(color = factor(Country)))+
  theme_classic()
srh.pw.plot

mwb.soc.plot <- ggplot(y, aes(mwb, soc)) +
  geom_point(alpha = 0.2) +
  geom_rug(alpha = 0.01)+
  theme_classic()+
  geom_smooth(method = "lm", se = T)
mwb.soc.plot     

# Regression analysis
mod1 <- lm(Work.wors ~ Gender + Country + Age.cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = y)
summary(mod1)
exp(coefficients(mod1))
exp(confint(mod1))

mod2 <- lm(Work.imp ~ Gender + Country + Age.cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = y)
summary(mod2)
exp(coefficients(mod2))
exp(confint(mod2))

mod3 <- lm(Privat.wors ~ Gender + Country + Age.cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = y)
summary(mod3)
exp(coefficients(mod3))
exp(confint(mod3))

mod4 <- lm(Privat.imp ~ Gender + Country + Age.cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = y)
summary(mod4)
exp(coefficients(mod4))
exp(confint(mod4))

mod5 <- lm(mwb ~ Work.wors + Work.imp + Privat.wors + Privat.imp + Gender + Country + Age.cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = y)
summary(mod5)
exp(coefficients(mod5))
exp(confint(mod5))
plot(mod5)

mod6 <- lm(srh ~ Work.wors + Work.imp + Privat.wors + Privat.imp + Gender + Country + Age.cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = y)
summary(mod6)
exp(coefficients(mod6))
exp(confint(mod6))

# Save the output of regression analysis
library(devtools)
install_github("dgrtwo/broom")
library(broom)

tidy_mod6 <- tidy(mod6)
tidy_mod1
write.csv(tidy_mod6, "tidy_mod6.csv")

# Ordinal regression analysis
library(MASS)
o$Work.wors <- as.ordered(o$Work.wors)
o$Work.imp <- as.ordered(o$Work.imp)
xtabs(~Work.wors+Contract.change, o)

ord1 <- polr(Work.wors ~ Gender + Country + Age.cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = o, Hess = TRUE)
summary(ord1)
exp(coefficients(ord1))
exp(confint(ord1))

ctable <- coef(summary(ord1))
ctable
p <- pnorm(abs(ctable[, "t value"]), lower.tail = F) * 2
ctable <- cbind(ctable, "p value" = p)
ci <- confint(ord1)
ci
exp(coef(ord1))
exp(cbind(OR = coef(ord1), ci))

ord2 <- polr(Work.imp ~ Gender + Country + Age.cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = o, Hess = TRUE)
summary(ord2)
exp(coefficients(ord2))
exp(confint(ord2))