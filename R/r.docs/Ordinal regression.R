# Data cleaning
library(tidyverse)
library(leaps)
library(ggbeeswarm)
library(tableone)
library(mclust)
library(patchwork)
library(MASS)
library(car)
library(tinytex)
library(corrplot)
library(Hmisc)
library(sjlabelled)
library(tibble)
library(broom)

write.csv(mf, file = "Wave4_education.csv")
df <- read.csv("~/Desktop/PhD/Papers/Paper 1/BMC Public Health/R/Covid-19/data/data.csv", header=TRUE)

df$Ovrl_HO <- as.factor(df$Ovrl_HO)
aggregate(df$mwb, list(df$Ovrl_HO), FUN = sd)

res <- aov(mwb ~ Ovrl_HO, data = df)
summary(res)

group_by(df, Ovrl_HO) %>%
  summarise(
    count = n(),
    mean = mean(mwb, na.rm = TRUE),
    sd = sd(mwb, na.rm = TRUE)
  )

# Data cleaning
df <- data[complete.cases(data),]
m$Work.wors <- as.ordered(m$Work.wors)
m$Work.imp <- as.ordered(m$Work.imp)
m$Privat.wors <- as.ordered(m$Privat.wors)
m$Privat.imp <- as.ordered(m$Privat.imp)
m$hd <- as.numeric(m$hd)

m$Work.wors <- as.factor(m$Work.wors)
m$Work.imp <- as.numeric(m$Work.imp)
m$Privat.imp <- as.numeric(m$Privat.imp)
m$Privat.wors <- as.numeric(m$Privat.wors)

m$Age.num <- 2020 - m$Year.birth

m <- mutate(m, Work.wors.3 = Work.wors)
m <- mutate(m, Work.imp.3 = Work.imp)
m <- mutate(m, Privat.wors.3 = Privat.wors)
m <- mutate(m, Privat.imp.3 = Privat.imp)
m$Work.wors.3 <- recode(m$Work.wors.3, "1:2 = 1")
m$Work.wors.3 <- recode(m$Work.wors.3, "3 = 2")
m$Work.wors.3 <- recode(m$Work.wors.3, "4:5 = 3")
m$Work.imp.3 <- recode(m$Work.imp.3, "1:2 = 1")
m$Work.imp.3 <- recode(m$Work.imp.3, "3 = 2")
m$Work.imp.3 <- recode(m$Work.imp.3, "4:5 = 3")
m$Privat.wors.3 <- recode(m$Privat.wors.3, "1:2 = 1")
m$Privat.wors.3 <- recode(m$Privat.wors.3, "3 = 2")
m$Privat.wors.3 <- recode(m$Privat.wors.3, "4:5 = 3")
m$Privat.imp.3 <- recode(m$Privat.imp.3, "1:2 = 1")
m$Privat.imp.3 <- recode(m$Privat.imp.3, "3 = 2")
m$Privat.imp.3 <- recode(m$Privat.imp.3, "4:5 = 3")

m$Work.wors.3 <- as.numeric(m$Work.wors.3)
m$Work.imp.3 <- as.numeric(m$Work.imp.3)
m$Privat.wors.3 <- as.numeric(m$Privat.wors.3)
m$Privat.imp.3 <- as.numeric(m$Privat.imp.3)

# Regression
.plot_bic <- function(fit) {
  tbl <- summary(fit)
  df <- tbl$which %>% 
    data.frame(check.names = FALSE) %>% 
    mutate(BIC = tbl$bic) %>% 
    mutate(i = order(BIC)) %>% 
    pivot_longer(-c(i, BIC)) 
  p1 <- ggplot(df, aes(name, i, fill = value)) + 
    geom_tile(col = "grey") + 
    scale_fill_manual("selected", values = c("white", "black")) +
    coord_equal() + theme_void(9) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  p2 <- ggplot(df, aes(1, i, fill = BIC)) +
    geom_tile(col = "grey") +
    scale_fill_viridis_c(trans = "reverse") +
    coord_equal() + theme_void(9) 
  wrap_plots(p1, p2, nrow = 1) +
    plot_layout(guides = "collect") &
    scale_y_reverse() &
    theme(legend.key.size = unit(0.75, "lines")) 
}

fit
fit <- regsubsets(Privat.imp ~ ., f, force.in = 1)
(bic1 <- .plot_bic(fit))

# Ordinal regression analysis

# Work life worsened
ord1 <- polr(Work.wors ~ Gender + Country + Age.cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = m, Hess = TRUE)
summary(ord1)
exp(coefficients(ord1))
exp(confint(ord1))
vif(ord1)

# Calculating p value
ctable1 <- coef(summary(ord1))
ctable1
p <- pnorm(abs(ctable1[, "t value"]), lower.tail = F) * 2
ctable1 <- cbind(ctable1, "p value" = p)
ci1 <- confint(ord1)
ci1
exp(coef(ord1))
orci1 <- exp(cbind(OR = coef(ord1), ci1))
orci1

dat1 <- data.frame(orci1, round(orci1, 2))
dat2 <- data.frame(p,round(p, 4))
write.csv(dat1, file = "work_wors.csv")
write.csv(dat2, file = "work_wors.p.csv")

# Work life improved
ord2 <- polr(Work.imp ~ Gender + Country + Age.cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = m, Hess = TRUE)
summary(ord2)
exp(coefficients(ord2))
exp(confint(ord2))

ctable2 <- coef(summary(ord2))
ctable2
p <- pnorm(abs(ctable2[, "t value"]), lower.tail = F) * 2
ctable2 <- cbind(ctable2, "p value" = p)
ci2 <- confint(ord2)
ci2
exp(coef(ord2))
orci2 <- exp(cbind(OR = coef(ord2), ci2))
orci2
round(orci1, 2)

dat1 <- data.frame(orci2, round(orci2, 2))
dat2 <- data.frame(p,round(p, 4))
write.csv(dat1, file = "work_imp.csv")
write.csv(dat2, file = "work_imp.p.csv")

# Private life worsened
ord3 <- polr(Privat.wors ~ Gender + Country + Age.cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = m, Hess = TRUE)
summary(ord3)
exp(coefficients(ord3))
exp(confint(ord3))

ctable3 <- coef(summary(ord3))
ctable3
p <- pnorm(abs(ctable3[, "t value"]), lower.tail = F) * 2
ctable2 <- cbind(ctable3, "p value" = p)
ci3 <- confint(ord3)
ci3
exp(coef(ord3))
orci3 <- exp(cbind(OR = coef(ord3), ci3))
orci3

dat1 <- data.frame(orci3, round(orci3, 2))
dat2 <- data.frame(p,round(p, 4))
write.csv(dat1, file = "privat_wors.csv")
write.csv(dat2, file = "privat_wors.p.csv")

# Privat life improved
ord4 <- polr(Privat.imp ~ Gender + Country + Age.cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged"), data = m, Hess = TRUE)
summary(ord4)
exp(coefficients(ord4))
exp(confint(ord4))

ctable4 <- coef(summary(ord4))
ctable4
p <- pnorm(abs(ctable4[, "t value"]), lower.tail = F) * 2
ctable4 <- cbind(ctable4, "p value" = p)
ci4 <- confint(ord4)
ci4
exp(coef(ord4))
orci4 <- exp(cbind(OR = coef(ord4), ci4))
orci4
round(orci4, 2)

dat1 <- data.frame(orci4, round(orci4, 2))
dat2 <- data.frame(p,round(p, 4))
write.csv(dat1, file = "privat_imp.csv")
write.csv(dat2, file = "privat_imp.p.csv")

# Mental well-being
ord5 <- polr(mwb.cat ~ Gender + Country + Age.cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged") + work.wors.3 + work.imp.3 + privat.wors.3 + privat.imp.3, data = y, Hess = TRUE)
summary(ord5)
exp(coefficients(ord5))
exp(confint(ord5))

ctable5 <- coef(summary(ord5))
ctable5
p <- pnorm(abs(ctable5[, "t value"]), lower.tail = F) * 2
ctable5 <- cbind(ctable5, "p value" = p)
ci5 <- confint(ord5)
ci5
exp(coef(ord5))
orci5 <- exp(cbind(OR = coef(ord5), ci5))
orci5
round(orci5, 2)
dat2 <- data.frame(p,round(p, 4))
dat1 <- data.frame(orci5, round(orci5, 2))
write.csv(dat1, file = "mwb.csv")
write.csv(dat2, file = "mwb.p.csv")

# Self rated health
ord6 <- polr(srh ~ Gender + Country + Age.cat + relevel(Contract.change, ref = "No change") + Liv.situation + relevel(Ovrl_HO, ref = "No HO") + relevel(Freetime, ref = "Unchanged") + relevel(Work_time, ref = "Unchanged") + relevel(Caring_duties, ref = "Unchanged") + work.wors.3 + work.imp.3 + privat.wors.3 + privat.imp.3, data = y, Hess = TRUE)
summary(ord6)
exp(coefficients(ord6))
exp(confint(ord6))

ctable6 <- coef(summary(ord6))
ctable6
p <- pnorm(abs(ctable6[, "t value"]), lower.tail = F) * 2
p
ctable6 <- cbind(ctable6, "p value" = p)
ci6 <- confint(ord6)
ci6
exp(coef(ord6))
orci6 <- exp(cbind(OR = coef(ord6), ci6))
orci6
round(orci6, 2)
dat2 <- data.frame(p,round(p, 4))
dat1 <- data.frame(orci6, round(orci6, 2))
write.csv(dat2, file = "srh.p.csv")
vif(ord6)

# Summary
library(epiDisplay)
summary(c(df$Liv.situation, df$Age))
tab1(df$Age.cat, sort.group = "decreasing")
tab1(df$mwb.cat)
tab1(df$Health_recode)
by(y$Age.cat, y$Liv.situation, summary)
ggplot(y, aes(x= Liv.situation, y = Age.cat))+
  geom_jitter()

write.csv(m, file = "data.csv")


## Correlation analysis
df1 <- mf %>%
  dplyr::select(Gender, Age.num, Country, Liv.situation, Contract.change, Work.quant, Care.quant, Free.quant, ho.diff, Work.wors, Work.imp, Privat.wors, Privat.imp, mwb, srh)

df1$Contract.change <- ec$Contract.change.n

df2 <- df1
df2[,] = lapply(df2[,], as.numeric)

df1$Work.quant <- as.factor(df1$Work.quant)

colnames(df2) <- c("Gender", "Age", "Country", "Living situation", "Contract change", "Work time", "Caring duties", "Leisure time", "Home-office", "Work worsened", "Work improved", "Private worsened", "Private improved", "MWB", "SRH")
col_order <- c("Gender", "Age", "Country", "Living situation", "Contract change", "Work time", "Caring duties", "Leisure time", "Home-office", "Work worsened", "Work improved", "Private worsened", "Private improved", "MWB", "SRH")
df2 <- df2[, col_order]

mat1 <- rcorr(as.matrix(df2))
mat1
mat2 <- cor(df2)
cor(df2$`Work worsened`, df2$"Contract change")
corrplot(mat2, method = "number", type = "upper", tl.col = "black")
corrplot(mat1, method = "number", type = "upper", order="alphabet")
corrplot(mat1$r, method = "number", type = "upper", tl.col = "black", 
         p.mat = mat1$P, sig.level = 0.001, insig = "blank")
