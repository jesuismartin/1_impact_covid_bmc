### 10.2.2021
## Graphs for BMC Public Health

library(tidyverse)
library(likert)
library(patchwork)
library(wesanderson)
library(gridExtra)
library(cowplot)
library(scales)
library(epiDisplay)

df <- data

## Graph 1: Perceived impact 
# Using Likert package

# Factor levels
lt <- df %>%
  dplyr::select(Work.wors, Work.imp, Privat.wors, Privat.imp)
lt$Privat.wors <- factor(df$Privat.wors, levels = c("1", "2", "3", "4", "5"),
                         labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree"))
lt$Privat.imp <- factor(df$Privat.imp, levels = c("1", "2", "3", "4", "5"),
                         labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree"))
lt$Work.wors <- factor(df$Work.wors, levels = c("1", "2", "3", "4", "5"),
                         labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree"))
lt$Work.imp <- factor(df$Work.imp, levels = c("1", "2", "3", "4", "5"),
                         labels = c("Strongly disagree", "Disagree", "Neither/nor", "Agree", "Strongly agree"))
summary(lt)
lt <- as.data.frame(lt)
table(lt)
# Version 1: 5-point scale with Likert
names(lt) <- c("Work worsened", "Work improved", "Private worsened", "Private improved")
l <- likert(lt)

g1 <- plot(l, type = "bar", centered = FALSE, group.order = c("Work worsened", "Work improved", "Private worsened", "Private improved")) +
  labs(x = NULL, y = NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "right") +
  theme_classic()
g1
tiff("Plot1.tiff", units="in", width=8, height=4, res=300)
dev.off()

# Using pivot_longer & ggplot
pivot1 <- lt %>% pivot_longer('Work worsened' : 'Private improved', names_to = "Variable", values_to = "Likert")
pivot1$Variable <- as.factor(pivot1$Variable)
pivot1$value <- as.numeric(pivot1$Likert)

ggplot(pivot1, aes(fill = Likert, y = value, x = Variable)) +
  geom_bar(position = "fill", stat = "identity", alpha = 1) +
  theme_bw()+
  coord_flip() +
  scale_fill_brewer(palette = "Blues")+
  scale_y_continuous(labels=percent)

## Suggestion from Jakob
library(scales)
# plot
ggplot(pivot, aes(y = value, x = Variable)) +
  coord_flip() + 
  geom_bar(aes(fill = Likert), stat="identity") + 
  theme_bw() + 
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette = "Spectral")+
  scale_y_continuous(labels=percent)

### 11.2.2021
## Graph 2:
# Version 1: ggplot
wt <- df %>%
  dplyr::select(Work_time, Freetime, Caring_duties, Ovrl_HO, Contract.change)

wt$Work_time <- as.factor(wt$Work_time)
wt$Freetime <- as.factor(wt$Freetime)
wt$Caring_duties <- as.factor(wt$Caring_duties)
wt$Ovrl_HO <- as.factor(df$Ovrl_HO)
wt$Contract.change <- as.factor(df$Contract.change)

pivot <- wt %>% pivot_longer(Work_time : Caring_duties, names_to = "domain", values_to = "value")
pivot$value <- factor(pivot$value, levels = c("1", "2", "3"),
                         labels = c("Decreased", "Unchanged", "Increased"))

pivot$domain <- as.factor(pivot$domain)
ggplot(pivot, aes(x = value, fill = domain)) +
  geom_bar(alpha = 0.6, position = "fill", width = 0.7) +
  theme_classic() +
  scale_y_continuous(labels=percent)

plot(pivot$domain)
plot(df$Work_time)
summary(wt)
summary(pivot)

# Version 2: Likert
wt <- df %>%
  dplyr::select(Work_time, Freetime, Caring_duties)
names(wt) <- c("Work time", "Leisure time", "Caring duties")

wt$`Work time` <- as.factor(wt$`Work time`)
wt$`Leisure time` <- as.factor(wt$`Leisure time`)
wt$`Caring duties` <- as.factor(wt$`Caring duties`)
wt$'Leisure time' <- factor(wt$'Leisure time', levels = c("Decreased", "Unchanged", "Increased"))
wt$'Work time' <- factor(wt$'Work time', levels = c("Decreased", "Unchanged", "Increased"))
wt$'Caring duties' <- factor(wt$'Caring duties', levels = c("Decreased", "Unchanged", "Increased"))
                          
wt <- as.data.frame(wt)
summary(wt)
l2 <- likert(wt)
l2
g2 <- plot(l2, type = "bar", centered = FALSE, group.order = c("Work time", "Leisure time", "Caring duties")) +
  labs(x = NULL, y = NULL) +
  theme(plot.title = element_text(hjust = 0.5),
  legend.position = "right") +
  theme_classic()
g2

## Integration of g1 and g2
plot_grid(g1, g2, ncol = 1, align = "v")
tiff("Plot1.tiff", units = "in", width = 6, height = 4, res = 300)
dev.off()

### 12.2.2021
## Graph 3: Contract change
ec <- df %>%
  dplyr::select(Contract.change)
ec$Contract.change <- as.factor(ec$Contract.change)
ec$Contract.change <- as.numeric(ec$Contract.change)

summary(ec)

ec$Contract.change <- factor(ec$Contract.change, levels = c("No change", "Short-time reduced", "Short-time 0", "Job loss"))

g3 <- ggplot(ec, aes(Contract.change, fill = Contract.change)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), show.legend = FALSE, width = 0.7) +
  scale_fill_manual(values = c("#D8B365", "grey90", "grey90", "#5AB4AC")) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent(round((..count..)/sum(..count..),2), accuracy = 1L)), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 10), breaks = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)) +
  labs(y = NULL, x = NULL) +
  theme_classic()
g3
tiff("Plot2.tiff", units = "in", width = 6, height = 4, res = 300)
dev.off()

## Graph 4: Home-office
ho <- df %>%
  dplyr::select(Ovrl_HO)
ho$Ovrl_HO <- as.factor(ho$Ovrl_HO)
ho$Ovrl_HO.n <- as.numeric(ho$Ovrl_HO)

ho$Ovrl_HO <- factor(ho$Ovrl_HO, levels = c("No HO", "Experienced HO", "New HO"),
                     labels = c("None", "Experienced", "New"))
                              

g4 <- ggplot(ho, aes(Ovrl_HO, fill = Ovrl_HO)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), show.legend = FALSE, width = 0.7) +
  scale_fill_manual(values = c("#D8B365", "grey90", "#5AB4AC")) +
  geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent(round((..count..)/sum(..count..),2), accuracy = 1L)), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5)) +
  labs(y = NULL, x = NULL) +
  theme_classic()
g4
tiff("Plot3.tiff", units = "in", width = 6, height = 4, res = 300)
dev.off()

## Integration of g3 and g4
plot_grid(g3, g4, ncol = 1, align = "h")

## Saving Corrpplot as tiff
plot <- corrplot(mat2$r, method = "number", type = "upper", tl.col = "black", 
                 p.mat = mat2$P, sig.level = 0.01, insig = "blank")
tiff("Corr.tiff", units="in", width=10, height=10, res=300)
dev.off()
