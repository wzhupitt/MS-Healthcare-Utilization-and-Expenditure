# By Efficacy (Figure 2) ####
## Doctor visits (not used) ####
DOCVIT <- glm (DOCVIT ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
               family = poisson, 
               data = as.data.frame(Events))
DOCVIT_RR <- cbind (round (exp(coefficients(DOCVIT)), 2), 
                    round (exp(confint.default(DOCVIT)),2), 
                    (summary(DOCVIT))$coefficients[,4])
DOCVIT_RR <- as.data.frame(DOCVIT_RR[2:3, ])
efficacy <- c("Standard-efficacy (12419)", "High-efficacy (109)")
newnames <- c("RR", "lower", "upper", "p", "label")
DOCVIT_RR$efficacy <- efficacy
setnames(DOCVIT_RR, newnames)

# make plot
p <- ggplot(data = DOCVIT_RR, aes(x = RR, y = label)) +
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=2.5) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#68080e", "#080c68")) +
  geom_vline(xintercept=1,linetype="dashed") +
  scale_x_continuous(name ="",
                     limits = c(0, 2.5), 
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5), 
                     labels = c("0", "0.5", "1", "1.5", "2", "2.5"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
DOCVIT_RR <- DOCVIT_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))
DOCVIT_RR$p <- specify_decimal(DOCVIT_RR$p, 3)
DOCVIT_RR$RR <- specify_decimal(DOCVIT_RR$RR, 2)
DOCVIT_RR$lower <- specify_decimal(DOCVIT_RR$lower, 2)
DOCVIT_RR$upper <- specify_decimal(DOCVIT_RR$upper, 2)
DOCVIT_RR <- DOCVIT_RR %>%
  mutate (p = ifelse (p<0.001, "<0.001", p)) %>% 
  mutate (ci = paste0 (lower, "-", upper))

# make table
data_table <- ggplot(data = DOCVIT_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0, size = 10*5/14) +
  # geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 2.5, label = ci), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 10*5/14) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.8, 3.7), 
                     labels = c("DMT", "IRR", "95% CI", expression(paste(italic("P")))),
                     position = "top")+
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 10)), 
                  ncol = 2,
                  widths = c(2, 1))

# make table
data_table <- ggplot(data = DOCVIT_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 10*5/14) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 10)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="DVEff.svg", width=6.3, height=1.4, units = "in", dpi=300)


## All doctor visits ####
DOCVITAll <- glm (DOCVITAll ~ 
                    DMTEff1 + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
                  family = poisson, 
                  data = as.data.frame(Events))
DOCVITAll_RR <- cbind (round (exp(coefficients(DOCVITAll)), 2), 
                       round (exp(confint.default(DOCVITAll)),2), 
                       (summary(DOCVITAll))$coefficients[,4])
DOCVITAll_RR <- as.data.frame(DOCVITAll_RR[2:3, ])
efficacy <- c("Standard-efficacy (12419)", "High-efficacy (109)")
newnames <- c("RR", "lower", "upper", "p", "label")
DOCVITAll_RR$efficacy <- efficacy
setnames(DOCVITAll_RR, newnames)


# make plot
p <- ggplot(data = DOCVITAll_RR, aes(x = RR, y = label)) +
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=2.5) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#68080e", "#080c68")) +
  geom_vline(xintercept=1,linetype="dashed") +
  scale_x_continuous(name ="",
                     limits = c(0, 2.5), 
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5), 
                     labels = c("0", "0.5", "1", "1.5", "2", "2.5"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
DOCVITAll_RR <- DOCVITAll_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))
DOCVITAll_RR$p <- specify_decimal(DOCVITAll_RR$p, 3)
DOCVITAll_RR$RR <- specify_decimal(DOCVITAll_RR$RR, 2)
DOCVITAll_RR$lower <- specify_decimal(DOCVITAll_RR$lower, 2)
DOCVITAll_RR$upper <- specify_decimal(DOCVITAll_RR$upper, 2)
DOCVITAll_RR <- DOCVITAll_RR %>%
  mutate (p = ifelse (p<0.001, "<0.001", p))

# make table
data_table <- ggplot(data = DOCVITAll_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 10*5/14)+
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 10),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 10)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="DVAEff.svg", width=6.3, height=1.4, units = "in", dpi=300)

## Neuro visits ####
NEUVIT <- glm (NEUVIT ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
               family = poisson, 
               data = as.data.frame(Events))
NEUVIT_RR <- cbind (round (exp(coefficients(NEUVIT)), 2), 
                    round (exp(confint.default(NEUVIT)),2), 
                    (summary(NEUVIT))$coefficients[,4])
NEUVIT_RR <- as.data.frame(NEUVIT_RR[2:3, ])
efficacy <- c("Standard-efficacy (12419)", "High-efficacy (109)")
newnames <- c("RR", "lower", "upper", "p", "label")
NEUVIT_RR$efficacy <- efficacy
setnames(NEUVIT_RR, newnames)


# make plot
p <- ggplot(data = NEUVIT_RR, aes(x = RR, y = label)) +
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=2.5) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#68080e", "#080c68")) +
  geom_vline(xintercept=1,linetype="dashed") +
  scale_x_continuous(name ="",
                     limits = c(0, 5), 
                     breaks = c(0, 1, 2, 3, 4, 5), 
                     labels = c("0", "1", "2", "3", "4", "5"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
NEUVIT_RR <- NEUVIT_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))
NEUVIT_RR$p <- specify_decimal(NEUVIT_RR$p, 3)
NEUVIT_RR$RR <- specify_decimal(NEUVIT_RR$RR, 2)
NEUVIT_RR$lower <- specify_decimal(NEUVIT_RR$lower, 2)
NEUVIT_RR$upper <- specify_decimal(NEUVIT_RR$upper, 2)
NEUVIT_RR <- NEUVIT_RR %>%
  mutate (p = ifelse (p<0.001, "<0.001", p))

# make table
data_table <- ggplot(data = NEUVIT_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 10*5/14)+
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 10),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 10)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="NVEff.svg", width=6.3, height=1.4, units = "in", dpi=300)


## ER visits ####
ER <- glm (ER ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
           family = poisson, 
           data = as.data.frame(Events))
ER_RR <- cbind (round (exp(coefficients(ER)), 2), 
                round (exp(confint.default(ER)),2), 
                (summary(ER))$coefficients[,4])
ER_RR <- as.data.frame(ER_RR[2:3, ])
efficacy <- c("Standard-efficacy (12419)", "High-efficacy (109)")
newnames <- c("RR", "lower", "upper", "p", "label")
ER_RR$efficacy <- efficacy
setnames(ER_RR, newnames)


# make plot
p <- ggplot(data = ER_RR, aes(x = RR, y = label)) +
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=2.5) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#68080e", "#080c68")) +
  geom_vline(xintercept=1,linetype="dashed") +
  scale_x_continuous(name ="",
                     limits = c(0, 2.5), 
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5), 
                     labels = c("0", "0.5", "1", "1.5", "2", "2.5"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
ER_RR <- ER_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))
ER_RR$p <- specify_decimal(ER_RR$p, 3)
ER_RR$RR <- specify_decimal(ER_RR$RR, 2)
ER_RR$lower <- specify_decimal(ER_RR$lower, 2)
ER_RR$upper <- specify_decimal(ER_RR$upper, 2)
ER_RR <- ER_RR %>%
  mutate (p = ifelse (p<0.001, "<0.001", p))

# make table
data_table <- ggplot(data = ER_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 10*5/14) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 10),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 10)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="EREff.svg", width=6.3, height=1.4, units = "in", dpi=300)


## All Hospitalization ####
HSPAll <- glm (HSPAll ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
               family = poisson, 
               data = as.data.frame(Events))
HSPAll_RR <- cbind (round (exp(coefficients(HSPAll)), 2), 
                    round (exp(confint.default(HSPAll)),2), 
                    (summary(HSPAll))$coefficients[,4])
HSPAll_RR <- as.data.frame(HSPAll_RR[2:3, ])
efficacy <- c("Standard-efficacy (12419)", "High-efficacy (109)")
newnames <- c("RR", "lower", "upper", "p", "label")
HSPAll_RR$efficacy <- efficacy
setnames(HSPAll_RR, newnames)


# make plot
p <- ggplot(data = HSPAll_RR, aes(x = RR, y = label)) +
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=2.5) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#68080e", "#080c68")) +
  geom_vline(xintercept=1,linetype="dashed") +
  scale_x_continuous(name ="",
                     limits = c(0, 2.5), 
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5), 
                     labels = c("0", "0.5", "1", "1.5", "2", "2.5"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
HSPAll_RR <- HSPAll_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))
HSPAll_RR$p <- specify_decimal(HSPAll_RR$p, 3)
HSPAll_RR$RR <- specify_decimal(HSPAll_RR$RR, 2)
HSPAll_RR$lower <- specify_decimal(HSPAll_RR$lower, 2)
HSPAll_RR$upper <- specify_decimal(HSPAll_RR$upper, 2)
HSPAll_RR <- HSPAll_RR %>%
  mutate (p = ifelse (p<0.001, "<0.001", p))

# make table
data_table <- ggplot(data = HSPAll_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 10*5/14) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 10),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 10)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="HPAEff.svg", width=6.3, height=1.4, units = "in", dpi=300)


## All MS-related Hospitalization ####
HSPMS <- glm (HSPMS ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
              family = poisson, 
              data = as.data.frame(Events))
HSPMS_RR <- cbind (round (exp(coefficients(HSPMS)), 2), 
                   round (exp(confint.default(HSPMS)),2), 
                   (summary(HSPMS))$coefficients[,4])
HSPMS_RR <- as.data.frame(HSPMS_RR[2:3, ])
efficacy <- c("Standard-efficacy (12419)", "High-efficacy (109)")
newnames <- c("RR", "lower", "upper", "p", "label")
HSPMS_RR$efficacy <- efficacy
setnames(HSPMS_RR, newnames)


# make plot
p <- ggplot(data = HSPMS_RR, aes(x = RR, y = label)) +
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=2.5) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#68080e", "#080c68")) +
  geom_vline(xintercept=1,linetype="dashed") +
  scale_x_continuous(name ="",
                     limits = c(0, 4), 
                     breaks = c(0, 1, 2, 3, 4, 5), 
                     labels = c("0", "1", "2", "3", "4", "5"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
HSPMS_RR <- HSPMS_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))
HSPMS_RR$p <- specify_decimal(HSPMS_RR$p, 3)
HSPMS_RR$RR <- specify_decimal(HSPMS_RR$RR, 2)
HSPMS_RR$lower <- specify_decimal(HSPMS_RR$lower, 2)
HSPMS_RR$upper <- specify_decimal(HSPMS_RR$upper, 2)
HSPMS_RR <- HSPMS_RR %>%
  mutate (p = ifelse (p<0.001, "<0.001", p))

# make table
data_table <- ggplot(data = HSPMS_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 10*5/14) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 10),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 10)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="HPMEff.svg", width=6.3, height=1.4, units = "in", dpi=300)


## Hospitalization (not used) ####
HSP <- glm (HSP ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
            family = poisson, 
            data = as.data.frame(Events))
HSP_RR <- cbind (round (exp(coefficients(HSP)), 2), 
                 round (exp(confint.default(HSP)),2), 
                 (summary(HSP))$coefficients[,4])
HSP_RR <- as.data.frame(HSP_RR[2:3, ])
efficacy <- c("Standard-efficacy (12419)", "High-efficacy (109)")
newnames <- c("RR", "lower", "upper", "p", "label")
HSP_RR$efficacy <- efficacy
setnames(HSP_RR, newnames)
HSP_RR$p <- specify_decimal(HSP_RR$p, 3)
HSP_RR <- HSP_RR %>%
  mutate (p = ifelse (p<0.001, "<0.001", p))

# make plot
p <- ggplot(data = HSP_RR, aes(x = RR, y = label)) +
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=4) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#68080e", "#080c68")) +
  geom_vline(xintercept=1,linetype="dashed") +
  scale_x_continuous(name ="",
                     limits = c(0, 2.5), 
                     breaks = c(0, 0.5, 1, 1.5, 2, 2.5), 
                     labels = c("0", "0.5", "1", "1.5", "2", "2.5"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
HSP_RR <- HSP_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))

# make table
data_table <- ggplot(data = HSP_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label, size = 10), hjust = 0) +
  geom_text(aes(x = 2, label = RR, size = 10), hjust = 0.0) +
  geom_text(aes(x = 2.5, label = lower, size = 10), hjust = 0.0) +
  geom_text(aes(x = 3, label = upper, size = 10), hjust = 0) +
  geom_text(aes(x = 3.5, label = p, size = 10), hjust = 0) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 16),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 12)), 
                  ncol = 2,
                  widths = c(2, 1))

## MRIBRAIN ####
MRIBRAIN <- glm (MRIBRAIN ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
                 family = poisson, 
                 data = as.data.frame(Events))
MRIBRAIN_RR <- cbind (round (exp(coefficients(MRIBRAIN)), 2), 
                      round (exp(confint.default(MRIBRAIN)),2), 
                      (summary(MRIBRAIN))$coefficients[,4])
MRIBRAIN_RR <- as.data.frame(MRIBRAIN_RR[2:3, ])
efficacy <- c("Standard-efficacy (12419)", "High-efficacy (109)")
newnames <- c("RR", "lower", "upper", "p", "label")
MRIBRAIN_RR$efficacy <- efficacy
setnames(MRIBRAIN_RR, newnames)


# make plot
p <- ggplot(data = MRIBRAIN_RR, aes(x = RR, y = label)) +
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=2.5) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#68080e", "#080c68")) +
  geom_vline(xintercept=1,linetype="dashed") +
  scale_x_continuous(name ="",
                     limits = c(0, 5), 
                     breaks = c(0, 1, 2, 3, 4, 5), 
                     labels = c("0", "1", "2", "3", "4", "5"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
MRIBRAIN_RR <- MRIBRAIN_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))
MRIBRAIN_RR$p <- specify_decimal(MRIBRAIN_RR$p, 3)
MRIBRAIN_RR$RR <- specify_decimal(MRIBRAIN_RR$RR, 2)
MRIBRAIN_RR$lower <- specify_decimal(MRIBRAIN_RR$lower, 2)
MRIBRAIN_RR$upper <- specify_decimal(MRIBRAIN_RR$upper, 2)
MRIBRAIN_RR <- MRIBRAIN_RR %>%
  mutate (p = ifelse (p<0.001, "<0.001", p))

# make table
data_table <- ggplot(data = MRIBRAIN_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 10*5/14) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 10),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 10)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="MRBEff.svg", width=6.3, height=1.4, units = "in", dpi=300)

## MRISPINE ####
MRISPINE <- glm (MRISPINE ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
                 family = poisson, 
                 data = as.data.frame(Events))
MRISPINE_RR <- cbind (round (exp(coefficients(MRISPINE)), 2), 
                      round (exp(confint.default(MRISPINE)),2), 
                      (summary(MRISPINE))$coefficients[,4])
MRISPINE_RR <- as.data.frame(MRISPINE_RR[2:3, ])
efficacy <- c("Standard-efficacy (12419)", "High-efficacy (109)")
newnames <- c("RR", "lower", "upper", "p", "label")
MRISPINE_RR$efficacy <- efficacy
setnames(MRISPINE_RR, newnames)


# make plot
p <- ggplot(data = MRISPINE_RR, aes(x = RR, y = label)) +
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=2.5) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#68080e", "#080c68")) +
  geom_vline(xintercept=1,linetype="dashed") +
  scale_x_continuous(name ="",
                     limits = c(0, 5), 
                     breaks = c(0, 1, 2, 3, 4, 5), 
                     labels = c("0", "1", "2", "3", "4", "5"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
MRISPINE_RR <- MRISPINE_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))
MRISPINE_RR$p <- specify_decimal(MRISPINE_RR$p, 3)
MRISPINE_RR$RR <- specify_decimal(MRISPINE_RR$RR, 2)
MRISPINE_RR$lower <- specify_decimal(MRISPINE_RR$lower, 2)
MRISPINE_RR$upper <- specify_decimal(MRISPINE_RR$upper, 2)
MRISPINE_RR <- MRISPINE_RR %>%
  mutate (p = ifelse (p<0.001, "<0.001", p))

# make table
data_table <- ggplot(data = MRISPINE_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 10*5/14) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 10),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 10)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="MRSEff.svg", width=6.3, height=1.4, units = "in", dpi=300)

## STEROID (not used) ####
STEROID <- glm (STEROID ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
                family = poisson, 
                data = as.data.frame(Events))
STEROID_RR <- cbind (round (exp(coefficients(STEROID)), 2), 
                     round (exp(confint.default(STEROID)),2), 
                     (summary(STEROID))$coefficients[,4])
STEROID_RR <- as.data.frame(STEROID_RR[2:3, ])
efficacy <- c("Standard-efficacy (12419)", "High-efficacy (109)")
newnames <- c("RR", "lower", "upper", "p", "label")
STEROID_RR$efficacy <- efficacy
setnames(STEROID_RR, newnames)
STEROID_RR$p <- specify_decimal(STEROID_RR$p, 3)
STEROID_RR <- STEROID_RR %>%
  mutate (p = ifelse (p<0.001, "<0.001", p))

# make plot
p <- ggplot(data = STEROID_RR, aes(x = RR, y = label)) +
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=4) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#68080e", "#080c68")) +
  geom_vline(xintercept=1,linetype="dashed") +
  coord_cartesian (xlim=c(-0.2, 5)) +
  scale_x_continuous(name ="",
                     breaks = c(-0.2, 1, 2, 3, 4, 5), 
                     labels = c("0", "1", "2", "3", "4", "5"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
STEROID_RR <- STEROID_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))

# make table
data_table <- ggplot(data = STEROID_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label, size = 10), hjust = 0) +
  geom_text(aes(x = 2, label = RR, size = 10), hjust = 0.0) +
  geom_text(aes(x = 2.5, label = lower, size = 10), hjust = 0.0) +
  geom_text(aes(x = 3, label = upper, size = 10), hjust = 0) +
  geom_text(aes(x = 3.5, label = p, size = 10), hjust = 0) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 16),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 12)), 
                  ncol = 2,
                  widths = c(2, 1))


## Relapse ####
RELAP <- glm (RELAP ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
              family = poisson, 
              data = as.data.frame(Events))
RELAP_RR <- cbind (round (exp(coefficients(RELAP)), 2), 
                   round (exp(confint.default(RELAP)),2), 
                   (summary(RELAP))$coefficients[,4])
RELAP_RR <- as.data.frame(RELAP_RR[2:3, ])
efficacy <- c("Standard-efficacy (12419)", "High-efficacy (109)")
newnames <- c("RR", "lower", "upper", "p", "label")
RELAP_RR$efficacy <- efficacy
setnames(RELAP_RR, newnames)


# make plot
p <- ggplot(data = RELAP_RR, aes(x = RR, y = label)) +
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=2.5) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#68080e", "#080c68")) +
  geom_vline(xintercept=1,linetype="dashed") +
  scale_x_continuous(name ="",
                     limits = c(0, 10), 
                     breaks = c(0, 2, 4, 6, 8, 10), 
                     labels = c("0", "2", "4", "6", "8", "10"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
RELAP_RR <- RELAP_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))
RELAP_RR$p <- specify_decimal(RELAP_RR$p, 3)
RELAP_RR$RR <- specify_decimal(RELAP_RR$RR, 2)
RELAP_RR$lower <- specify_decimal(RELAP_RR$lower, 2)
RELAP_RR$upper <- specify_decimal(RELAP_RR$upper, 2)
RELAP_RR <- RELAP_RR %>%
  mutate (p = ifelse (p<0.001, "<0.001", p))

# make table
data_table <- ggplot(data = RELAP_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 10*5/14) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 10),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 10)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="REEff.svg", width=6.3, height=1.4, units = "in", dpi=300)

## Poly-med ####
PMED <- glm (PMED ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
             family = poisson, 
             data = as.data.frame(Events))
PMED_RR <- cbind (round (exp(coefficients(PMED)), 2), 
                  round (exp(confint.default(PMED)),2), 
                  (summary(PMED))$coefficients[,4])
PMED_RR <- as.data.frame(PMED_RR[2:3, ])
efficacy <- c("Standard-efficacy (12419)", "High-efficacy (109)")
newnames <- c("RR", "lower", "upper", "p", "label")
PMED_RR$efficacy <- efficacy
setnames(PMED_RR, newnames)


# make plot
p <- ggplot(data = PMED_RR, aes(x = RR, y = label)) +
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=2.5) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#68080e", "#080c68")) +
  geom_vline(xintercept=1,linetype="dashed") +
  scale_x_continuous(name ="",
                     limits = c(0, 5), 
                     breaks = c(0, 1, 2, 3, 4, 5), 
                     labels = c("0", "1", "2", "3", "4", "5"), expand = c(0,0)) + 
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
PMED_RR <- PMED_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))
PMED_RR$p <- specify_decimal(PMED_RR$p, 3)
PMED_RR$RR <- specify_decimal(PMED_RR$RR, 2)
PMED_RR$lower <- specify_decimal(PMED_RR$lower, 2)
PMED_RR$upper <- specify_decimal(PMED_RR$upper, 2)
PMED_RR <- PMED_RR %>%
  mutate (p = ifelse (p<0.001, "<0.001", p))

# make table
data_table <- ggplot(data = PMED_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 10*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 10*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 10*5/14) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 10),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 10)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="PMEff.svg", width=6.3, height=1.4, units = "in", dpi=300)


# ####
# By DMT class, ND as reference (Figure 3)####
## Doctor visits (not used) ####
DOCVIT <- glm (DOCVIT ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
               family = poisson, 
               data = as.data.frame(Events))
DOCVIT_RR <- cbind (round (exp(coefficients(DOCVIT)), 2), 
                    round (exp(confint.default(DOCVIT)),2), 
                    (summary(DOCVIT))$coefficients[,4])
DOCVIT_RR <- as.data.frame(DOCVIT_RR[2:10, ])
dmt <- c("Interferon-beta (2912)", 
         "B cell depletion (45)", 
         "Cladribine (10)",
         "Glatiramer acetate (3994)",
         "Natalizumab (47)",
         "S1P-R modulator (1352)",
         "Teriflunomide (1153)",
         "Dimethyl fumarate (3058)",
         "Alemtuzumab (7)")
newnames <- c("RR", "lower", "upper", "p", "label")
DOCVIT_RR$dmt <- dmt
setnames(DOCVIT_RR, newnames)
DOCVIT_RR$p <- specify_decimal(DOCVIT_RR$p, 3)
DOCVIT_RR <- DOCVIT_RR %>% 
  mutate(label = factor (label, levels = c("Natalizumab (47)",
                                           "Cladribine (10)",
                                           "B cell depletion (45)",
                                           "Alemtuzumab (7)",
                                           "Teriflunomide (1153)",
                                           "S1P-R modulator (1352)",
                                           "Interferon-beta (2912)",
                                           "Glatiramer acetate (3994)",
                                           "Dimethyl fumarate (3058)"))) %>% 
  mutate(p = ifelse (p<0.0001, "<0.0001", p))

# make plot
p <- ggplot(data = DOCVIT_RR, aes(x = RR, y = label))+
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=3) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#66903c",
                                 "#bea991",
                                 "#00a3d7",
                                 "#644a8c",
                                 "#cf3637",
                                 "#ecd121",
                                 "#5c6268",
                                 "#56364d",
                                 "#5c4b36")) +
  geom_vline(xintercept=1,linetype="dashed")+
  coord_cartesian (xlim=c(-0.3, 15))+
  scale_x_continuous(name ="", 
                     breaks = c(-0.3, 1, 5, 10, 15), 
                     labels = c("0", "1", "5", "10", "15"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
DOCVIT_RR <- DOCVIT_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))

# make table
data_table <- ggplot(data = DOCVIT_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label, size = 10), hjust = 0) +
  geom_text(aes(x = 2, label = RR, size = 10), hjust = 0.0) +
  geom_text(aes(x = 2.5, label = lower, size = 10), hjust = 0.0) +
  geom_text(aes(x = 3, label = upper, size = 10), hjust = 0) +
  geom_text(aes(x = 3.5, label = p, size = 10), hjust = 0) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 16),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 12)), 
                  ncol = 2,
                  widths = c(2, 1))

## All doctor visits ####
DOCVITAll <- glm (DOCVITAll ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
                  family = poisson, 
                  data = as.data.frame(Events))
DOCVITAll_RR <- cbind (round (exp(coefficients(DOCVITAll)), 2), 
                       round (exp(confint.default(DOCVITAll)),2), 
                       (summary(DOCVITAll))$coefficients[,4])
DOCVITAll_RR <- as.data.frame(DOCVITAll_RR[2:10, ])
dmt <- c("Interferon-beta (2912)", 
         "B cell depletion (45)", 
         "Cladribine (10)",
         "Glatiramer acetate (3994)",
         "Natalizumab (47)",
         "S1P-R modulator (1352)",
         "Teriflunomide (1153)",
         "Dimethyl fumarate (3058)",
         "Alemtuzumab (7)")
eff <- c("1", "2", "2", "1", "2", "1", "1", "1", "2")
newnames <- c("RR", "lower", "upper", "p", "label")
DOCVITAll_RR$dmt <- dmt
setnames(DOCVITAll_RR, newnames)
DOCVITAll_RR$eff <- eff
DOCVITAll_RR <- DOCVITAll_RR %>% 
  mutate(label = factor (label, levels = c("Natalizumab (47)",
                                           "Cladribine (10)",
                                           "B cell depletion (45)",
                                           "Alemtuzumab (7)",
                                           "Teriflunomide (1153)",
                                           "S1P-R modulator (1352)",
                                           "Interferon-beta (2912)",
                                           "Glatiramer acetate (3994)",
                                           "Dimethyl fumarate (3058)")))

# make plot
p <- ggplot(data = DOCVITAll_RR, aes(x = RR, y = label))+
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=1) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#66903c",
                                 "#bea991",
                                 "#00a3d7",
                                 "#644a8c",
                                 "#cf3637",
                                 "#ecd121",
                                 "#001887",
                                 "#ff8900",
                                 "#008347")) +
  geom_vline(xintercept=1,linetype="dashed")+
  coord_cartesian (xlim=c(-0.5, 15))+
  scale_x_continuous(name ="", 
                     breaks = c(-0.5, 1, 5, 10, 15), 
                     labels = c("0", "1", "5", "10", "15"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
DOCVITAll_RR$p <- specify_decimal(DOCVITAll_RR$p, 4)
DOCVITAll_RR <- DOCVITAll_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))%>% 
  mutate (p = ifelse (p<0.0001, "<0.0001", p))
DOCVITAll_RR$RR <- specify_decimal(DOCVITAll_RR$RR, 2)
DOCVITAll_RR$lower <- specify_decimal(DOCVITAll_RR$lower, 2)
DOCVITAll_RR$upper <- specify_decimal(DOCVITAll_RR$upper, 2)



# make table
data_table <- ggplot(data = DOCVITAll_RR, aes(y = label, color = eff)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 8*5/14)+
  scale_colour_manual(values = c("#080c68", "#68080e")) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 8),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 8)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="DVACat.svg", width=5.0, height=2.7, units = "in", dpi=300)

## Neuro visits ####
NEUVIT <- glm (NEUVIT ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
               family = poisson, 
               data = as.data.frame(Events))
NEUVIT_RR <- cbind (round (exp(coefficients(NEUVIT)), 2), 
                    round (exp(confint.default(NEUVIT)),2), 
                    (summary(NEUVIT))$coefficients[,4])
NEUVIT_RR <- as.data.frame(NEUVIT_RR[2:10, ])
dmt <- c("Interferon-beta (2912)", 
         "B cell depletion (45)", 
         "Cladribine (10)",
         "Glatiramer acetate (3994)",
         "Natalizumab (47)",
         "S1P-R modulator (1352)",
         "Teriflunomide (1153)",
         "Dimethyl fumarate (3058)",
         "Alemtuzumab (7)")
dmteff <- c("1", "2", "2", "1", "2", "1", "1", "1", "2")
eff <- c("1", "2", "2", "1", "2", "1", "1", "1", "2")
newnames <- c("RR", "lower", "upper", "p", "label")
NEUVIT_RR$dmt <- dmt
setnames(NEUVIT_RR, newnames)
NEUVIT_RR$eff <- eff
NEUVIT_RR <- NEUVIT_RR %>% 
  mutate(label = factor (label, levels = c("Natalizumab (47)",
                                           "Cladribine (10)",
                                           "B cell depletion (45)",
                                           "Alemtuzumab (7)",
                                           "Teriflunomide (1153)",
                                           "S1P-R modulator (1352)",
                                           "Interferon-beta (2912)",
                                           "Glatiramer acetate (3994)",
                                           "Dimethyl fumarate (3058)")))

# make plot
p <- ggplot(data = NEUVIT_RR, aes(x = RR, y = label))+
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=1) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#66903c",
                                 "#bea991",
                                 "#00a3d7",
                                 "#644a8c",
                                 "#cf3637",
                                 "#ecd121",
                                 "#001887",
                                 "#ff8900",
                                 "#008347")) +
  geom_vline(xintercept=1,linetype="dashed")+
  coord_cartesian (xlim=c(-0.5, 15))+
  scale_x_continuous(name ="", 
                     breaks = c(-0.5, 1, 5, 10, 15), 
                     labels = c("0", "1", "5", "10", "15"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
NEUVIT_RR$p <- specify_decimal(NEUVIT_RR$p, 4)
NEUVIT_RR <- NEUVIT_RR %>%
  mutate (upper = ifelse (upper>=100, ">100", upper)) %>% 
  mutate(p = ifelse (p<0.0001, "<0.0001", p))
NEUVIT_RR$RR <- specify_decimal(NEUVIT_RR$RR, 2)
NEUVIT_RR$lower <- specify_decimal(NEUVIT_RR$lower, 2)
NEUVIT_RR$upper <- specify_decimal(NEUVIT_RR$upper, 2)



# make table
data_table <- ggplot(data = NEUVIT_RR, aes(y = label, color = eff)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 8*5/14)+
  scale_colour_manual(values = c("#080c68", "#68080e")) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 8),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 8)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="NVCat.svg", width=5.0, height=2.7, units = "in", dpi=300)

## ER visits ####
ER <- glm (ER ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
           family = poisson, 
           data = as.data.frame(Events))
ER_RR <- cbind (round (exp(coefficients(ER)), 2), 
                round (exp(confint.default(ER)),2), 
                (summary(ER))$coefficients[,4])
ER_RR <- as.data.frame(ER_RR[2:10, ])
dmt <- c("Interferon-beta (2912)", 
         "B cell depletion (45)", 
         "Cladribine (10)",
         "Glatiramer acetate (3994)",
         "Natalizumab (47)",
         "S1P-R modulator (1352)",
         "Teriflunomide (1153)",
         "Dimethyl fumarate (3058)",
         "Alemtuzumab (7)")
eff <- c("1", "2", "2", "1", "2", "1", "1", "1", "2")
newnames <- c("RR", "lower", "upper", "p", "label")
ER_RR$dmt <- dmt
setnames(ER_RR, newnames)
ER_RR$eff <- eff
ER_RR <- ER_RR %>% 
  mutate(label = factor (label, levels = c("Natalizumab (47)",
                                           "Cladribine (10)",
                                           "B cell depletion (45)",
                                           "Alemtuzumab (7)",
                                           "Teriflunomide (1153)",
                                           "S1P-R modulator (1352)",
                                           "Interferon-beta (2912)",
                                           "Glatiramer acetate (3994)",
                                           "Dimethyl fumarate (3058)")))

# make plot
p <- ggplot(data = ER_RR, aes(x = RR, y = label))+
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=1) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#66903c",
                                 "#bea991",
                                 "#00a3d7",
                                 "#644a8c",
                                 "#cf3637",
                                 "#ecd121",
                                 "#001887",
                                 "#ff8900",
                                 "#008347")) +
  geom_vline(xintercept=1,linetype="dashed")+
  coord_cartesian (xlim=c(-0.3, 4))+
  scale_x_continuous(name ="", 
                     breaks = c(-0.3, 1, 2, 3, 4), 
                     labels = c("0", "1", "2", "3", "4"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
ER_RR$p <- specify_decimal(ER_RR$p, 4)
ER_RR <- ER_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper)) %>% 
  mutate(p = ifelse (p<0.0001, "<0.0001", p))
ER_RR$RR <- specify_decimal(ER_RR$RR, 2)
ER_RR$lower <- specify_decimal(ER_RR$lower, 2)
ER_RR$upper <- specify_decimal(ER_RR$upper, 2)


# make table
data_table <- ggplot(data = ER_RR, aes(y = label, color = eff)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 8*5/14)+
  scale_colour_manual(values = c("#080c68", "#68080e")) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 8),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 8)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="ERCat.svg", width=5.0, height=2.7, units = "in", dpi=300)

## All Hospitalizations ####
HSPAll <- glm (HSPAll ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
               family = poisson, 
               data = as.data.frame(Events))
HSPAll_RR <- cbind (round (exp(coefficients(HSPAll)), 2), 
                    round (exp(confint.default(HSPAll)),2), 
                    (summary(HSPAll))$coefficients[,4])
HSPAll_RR <- as.data.frame(HSPAll_RR[2:10, ])
dmt <- c("Interferon-beta (2912)", 
         "B cell depletion (45)", 
         "Cladribine (10)",
         "Glatiramer acetate (3994)",
         "Natalizumab (47)",
         "S1P-R modulator (1352)",
         "Teriflunomide (1153)",
         "Dimethyl fumarate (3058)",
         "Alemtuzumab (7)")
eff <- c("1", "2", "2", "1", "2", "1", "1", "1", "2")
newnames <- c("RR", "lower", "upper", "p", "label")
HSPAll_RR$dmt <- dmt
setnames(HSPAll_RR, newnames)
HSPAll_RR$eff <- eff
HSPAll_RR <- HSPAll_RR %>% 
  mutate(label = factor (label, levels = c("Natalizumab (47)",
                                           "Cladribine (10)",
                                           "B cell depletion (45)",
                                           "Alemtuzumab (7)",
                                           "Teriflunomide (1153)",
                                           "S1P-R modulator (1352)",
                                           "Interferon-beta (2912)",
                                           "Glatiramer acetate (3994)",
                                           "Dimethyl fumarate (3058)")))

# make plot
p <- ggplot(data = HSPAll_RR, aes(x = RR, y = label))+
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=1) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#66903c",
                                 "#bea991",
                                 "#00a3d7",
                                 "#644a8c",
                                 "#cf3637",
                                 "#ecd121",
                                 "#001887",
                                 "#ff8900",
                                 "#008347")) +
  geom_vline(xintercept=1,linetype="dashed")+
  coord_cartesian (xlim=c(-0.1, 2))+
  scale_x_continuous(name ="", 
                     breaks = c(-0.1, 0.5, 1, 1.5, 2), 
                     labels = c("0", "0.5", "1", "1.5", "2"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
HSPAll_RR$p <- specify_decimal(HSPAll_RR$p, 4)
HSPAll_RR <- HSPAll_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper)) %>% 
  mutate(p = ifelse (p<0.0001, "<0.0001", p))
HSPAll_RR$RR <- specify_decimal(HSPAll_RR$RR, 2)
HSPAll_RR$lower <- specify_decimal(HSPAll_RR$lower, 2)
HSPAll_RR$upper <- specify_decimal(HSPAll_RR$upper, 2)


# make table
data_table <- ggplot(data = HSPAll_RR, aes(y = label, color = eff)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 8*5/14)+
  scale_colour_manual(values = c("#080c68", "#68080e")) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 8),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 8)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="HPACat.svg", width=5.0, height=2.7, units = "in", dpi=300)

## All MS hospitalizations ####
HSPMS <- glm (HSPMS ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
              family = poisson, 
              data = as.data.frame(Events))
HSPMS_RR <- cbind (round (exp(coefficients(HSPMS)), 2), 
                   round (exp(confint.default(HSPMS)),2), 
                   (summary(HSPMS))$coefficients[,4])
HSPMS_RR <- as.data.frame(HSPMS_RR[2:10, ])
dmt <- c("Interferon-beta (2912)", 
         "B cell depletion (45)", 
         "Cladribine (10)",
         "Glatiramer acetate (3994)",
         "Natalizumab (47)",
         "S1P-R modulator (1352)",
         "Teriflunomide (1153)",
         "Dimethyl fumarate (3058)",
         "Alemtuzumab (7)")
eff <- c("1", "2", "2", "1", "2", "1", "1", "1", "2")
newnames <- c("RR", "lower", "upper", "p", "label")
HSPMS_RR$dmt <- dmt
setnames(HSPMS_RR, newnames)
HSPMS_RR$eff <- eff
HSPMS_RR <- HSPMS_RR %>% 
  mutate(label = factor (label, levels = c("Natalizumab (47)",
                                           "Cladribine (10)",
                                           "B cell depletion (45)",
                                           "Alemtuzumab (7)",
                                           "Teriflunomide (1153)",
                                           "S1P-R modulator (1352)",
                                           "Interferon-beta (2912)",
                                           "Glatiramer acetate (3994)",
                                           "Dimethyl fumarate (3058)")))

# make plot
p <- ggplot(data = HSPMS_RR, aes(x = RR, y = label))+
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=1) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#66903c",
                                 "#bea991",
                                 "#00a3d7",
                                 "#644a8c",
                                 "#cf3637",
                                 "#ecd121",
                                 "#001887",
                                 "#ff8900",
                                 "#008347")) +
  geom_vline(xintercept=1,linetype="dashed")+
  coord_cartesian (xlim=c(-0.2, 7.5))+
  scale_x_continuous(name ="", 
                     breaks = c(-0.2, 1, 2.5, 5, 7.5), 
                     labels = c("0", "1", "2.5", "5", "7.5"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
HSPMS_RR$p <- specify_decimal(HSPMS_RR$p, 4)
HSPMS_RR <- HSPMS_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper)) %>% 
  mutate(p = ifelse (p<0.0001, "<0.0001", p))
HSPMS_RR$RR <- specify_decimal(HSPMS_RR$RR, 2)
HSPMS_RR$lower <- specify_decimal(HSPMS_RR$lower, 2)
HSPMS_RR$upper <- specify_decimal(HSPMS_RR$upper, 2)


# make table
data_table <- ggplot(data = HSPMS_RR, aes(y = label, color = eff)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 8*5/14)+
  scale_colour_manual(values = c("#080c68", "#68080e")) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 8),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 8)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="HPMCat.svg", width=5.0, height=2.7, units = "in", dpi=300)

## Hospitalizations (not used) ####
HSP <- glm (HSP ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
            family = poisson, 
            data = as.data.frame(Events))
HSP_RR <- cbind (round (exp(coefficients(HSP)), 2), 
                 round (exp(confint.default(HSP)),2), 
                 (summary(HSP))$coefficients[,4])
HSP_RR <- as.data.frame(HSP_RR[2:10, ])
dmt <- c("Interferon-beta (2912)", 
         "B cell depletion (45)", 
         "Cladribine (10)",
         "Glatiramer acetate (3994)",
         "Natalizumab (47)",
         "S1P-R modulator (1352)",
         "Teriflunomide (1153)",
         "Dimethyl fumarate (3058)",
         "Alemtuzumab (7)")
newnames <- c("RR", "lower", "upper", "p", "label")
HSP_RR$dmt <- dmt
setnames(HSP_RR, newnames)
HSP_RR$p <- specify_decimal(HSP_RR$p, 3)
HSP_RR <- HSP_RR %>% 
  mutate(label = factor (label, levels = c("Natalizumab (47)",
                                           "Cladribine (10)",
                                           "B cell depletion (45)",
                                           "Alemtuzumab (7)",
                                           "Teriflunomide (1153)",
                                           "S1P-R modulator (1352)",
                                           "Interferon-beta (2912)",
                                           "Glatiramer acetate (3994)",
                                           "Dimethyl fumarate (3058)"))) %>% 
  mutate(p = ifelse (p<0.001, "<0.001", p))

# make plot
p <- ggplot(data = HSP_RR, aes(x = RR, y = label))+
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=3) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#66903c",
                                 "#bea991",
                                 "#00a3d7",
                                 "#644a8c",
                                 "#cf3637",
                                 "#ecd121",
                                 "#5c6268",
                                 "#56364d",
                                 "#5c4b36")) +
  geom_vline(xintercept=1,linetype="dashed")+
  coord_cartesian (xlim=c(-0.05, 2))+
  scale_x_continuous(name ="", 
                     breaks = c(-0.05, 0.5, 1, 1.5, 2), 
                     labels = c("0", "0.5", "1", "1.5", "2"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
HSP_RR <- HSP_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))

# make table
data_table <- ggplot(data = HSP_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label, size = 10), hjust = 0) +
  geom_text(aes(x = 2, label = RR, size = 10), hjust = 0.0) +
  geom_text(aes(x = 2.5, label = lower, size = 10), hjust = 0.0) +
  geom_text(aes(x = 3, label = upper, size = 10), hjust = 0) +
  geom_text(aes(x = 3.5, label = p, size = 10), hjust = 0) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 16),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 12)), 
                  ncol = 2,
                  widths = c(2, 1))

## MRIBRAIN ####
MRIBRAIN <- glm (MRIBRAIN ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
                 family = poisson, 
                 data = as.data.frame(Events))
MRIBRAIN_RR <- cbind (round (exp(coefficients(MRIBRAIN)), 2), 
                      round (exp(confint.default(MRIBRAIN)),2), 
                      (summary(MRIBRAIN))$coefficients[,4])
MRIBRAIN_RR <- as.data.frame(MRIBRAIN_RR[2:10, ])
dmt <- c("Interferon-beta (2912)", 
         "B cell depletion (45)", 
         "Cladribine (10)",
         "Glatiramer acetate (3994)",
         "Natalizumab (47)",
         "S1P-R modulator (1352)",
         "Teriflunomide (1153)",
         "Dimethyl fumarate (3058)",
         "Alemtuzumab (7)")
eff <- c("1", "2", "2", "1", "2", "1", "1", "1", "2")
newnames <- c("RR", "lower", "upper", "p", "label")
MRIBRAIN_RR$dmt <- dmt
setnames(MRIBRAIN_RR, newnames)
MRIBRAIN_RR$eff <- eff

MRIBRAIN_RR <- MRIBRAIN_RR %>% 
  mutate(label = factor (label, levels = c("Natalizumab (47)",
                                           "Cladribine (10)",
                                           "B cell depletion (45)",
                                           "Alemtuzumab (7)",
                                           "Teriflunomide (1153)",
                                           "S1P-R modulator (1352)",
                                           "Interferon-beta (2912)",
                                           "Glatiramer acetate (3994)",
                                           "Dimethyl fumarate (3058)")))

# make plot
p <- ggplot(data = MRIBRAIN_RR, aes(x = RR, y = label))+
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=1) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#66903c",
                                 "#bea991",
                                 "#00a3d7",
                                 "#644a8c",
                                 "#cf3637",
                                 "#ecd121",
                                 "#001887",
                                 "#ff8900",
                                 "#008347")) +
  geom_vline(xintercept=1,linetype="dashed")+
  coord_cartesian (xlim=c(-0.5, 15))+
  scale_x_continuous(name ="", 
                     breaks = c(-0.5, 1, 5, 10, 15), 
                     labels = c("0", "1", "5", "10", "15"), expand = c(0,0)) +
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
MRIBRAIN_RR$p <- specify_decimal(MRIBRAIN_RR$p, 4)
MRIBRAIN_RR <- MRIBRAIN_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper)) %>% 
  mutate(p = ifelse (p<0.0001, "<0.0001", p))
MRIBRAIN_RR$RR <- specify_decimal(MRIBRAIN_RR$RR, 2)
MRIBRAIN_RR$lower <- specify_decimal(MRIBRAIN_RR$lower, 2)
MRIBRAIN_RR$upper <- specify_decimal(MRIBRAIN_RR$upper, 2)


# make table
data_table <- ggplot(data = MRIBRAIN_RR, aes(y = label, color = eff)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 8*5/14)+
  scale_colour_manual(values = c("#080c68", "#68080e")) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 8),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 8)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="MRBCat.svg", width=5.0, height=2.7, units = "in", dpi=300)


## MRISPINE ####
MRISPINE <- glm (MRISPINE ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
                 family = poisson, 
                 data = as.data.frame(Events))
MRISPINE_RR <- cbind (round (exp(coefficients(MRISPINE)), 2), 
                      round (exp(confint.default(MRISPINE)),2), 
                      (summary(MRISPINE))$coefficients[,4])
MRISPINE_RR <- as.data.frame(MRISPINE_RR[2:10, ])
dmt <- c("Interferon-beta (2912)", 
         "B cell depletion (45)", 
         "Cladribine (10)",
         "Glatiramer acetate (3994)",
         "Natalizumab (47)",
         "S1P-R modulator (1352)",
         "Teriflunomide (1153)",
         "Dimethyl fumarate (3058)",
         "Alemtuzumab (7)")
eff <- c("1", "2", "2", "1", "2", "1", "1", "1", "2")
newnames <- c("RR", "lower", "upper", "p", "label")
MRISPINE_RR$dmt <- dmt
setnames(MRISPINE_RR, newnames)
MRISPINE_RR$eff <- eff
MRISPINE_RR <- MRISPINE_RR %>% 
  mutate(label = factor (label, levels = c("Natalizumab (47)",
                                           "Cladribine (10)",
                                           "B cell depletion (45)",
                                           "Alemtuzumab (7)",
                                           "Teriflunomide (1153)",
                                           "S1P-R modulator (1352)",
                                           "Interferon-beta (2912)",
                                           "Glatiramer acetate (3994)",
                                           "Dimethyl fumarate (3058)")))

# make plot
p <- ggplot(data = MRISPINE_RR, aes(x = RR, y = label))+
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=1) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#66903c",
                                 "#bea991",
                                 "#00a3d7",
                                 "#644a8c",
                                 "#cf3637",
                                 "#ecd121",
                                 "#001887",
                                 "#ff8900",
                                 "#008347")) +
  geom_vline(xintercept=1,linetype="dashed")+
  coord_cartesian (xlim=c(-0.5, 18))+
  scale_x_continuous(name ="", 
                     breaks = c(-0.5, 1, 6, 12, 18), 
                     labels = c("0", "1", "6", "12", "18"), expand = c(0,0)) +  
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
MRISPINE_RR$p <- specify_decimal(MRISPINE_RR$p, 4)
MRISPINE_RR <- MRISPINE_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper)) %>% 
  mutate(p = ifelse (p<0.0001, "<0.0001", p))
MRISPINE_RR$RR <- specify_decimal(MRISPINE_RR$RR, 2)
MRISPINE_RR$lower <- specify_decimal(MRISPINE_RR$lower, 2)
MRISPINE_RR$upper <- specify_decimal(MRISPINE_RR$upper, 2)


# make table
data_table <- ggplot(data = MRISPINE_RR, aes(y = label, color = eff)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 8*5/14)+
  scale_colour_manual(values = c("#080c68", "#68080e")) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 8),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 8)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="MRSCat.svg", width=5.0, height=2.7, units = "in", dpi=300)

## STEROID (not used) ####
STEROID <- glm (STEROID ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
                family = poisson, 
                data = as.data.frame(Events))
STEROID_RR <- cbind (round (exp(coefficients(STEROID)), 2), 
                     round (exp(confint.default(STEROID)),2), 
                     (summary(STEROID))$coefficients[,4])
STEROID_RR <- as.data.frame(STEROID_RR[2:10, ])
dmt <- c("Interferon-beta (2912)", 
         "B cell depletion (45)", 
         "Cladribine (10)",
         "Glatiramer acetate (3994)",
         "Natalizumab (47)",
         "S1P-R modulator (1352)",
         "Teriflunomide (1153)",
         "Dimethyl fumarate (3058)",
         "Alemtuzumab (7)")
newnames <- c("RR", "lower", "upper", "p", "label")
STEROID_RR$dmt <- dmt
setnames(STEROID_RR, newnames)
STEROID_RR$p <- specify_decimal(STEROID_RR$p, 3)
STEROID_RR <- STEROID_RR %>% 
  mutate(label = factor (label, levels = c("Natalizumab (47)",
                                           "Cladribine (10)",
                                           "B cell depletion (45)",
                                           "Alemtuzumab (7)",
                                           "Teriflunomide (1153)",
                                           "S1P-R modulator (1352)",
                                           "Interferon-beta (2912)",
                                           "Glatiramer acetate (3994)",
                                           "Dimethyl fumarate (3058)"))) %>% 
  mutate(p = ifelse (p<0.001, "<0.001", p))

# make plot
p <- ggplot(data = STEROID_RR, aes(x = RR, y = label))+
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=3) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#66903c",
                                 "#bea991",
                                 "#00a3d7",
                                 "#644a8c",
                                 "#cf3637",
                                 "#ecd121",
                                 "#5c6268",
                                 "#56364d",
                                 "#5c4b36")) +
  geom_vline(xintercept=1,linetype="dashed")+
  coord_cartesian (xlim=c(-0.2, 9))+
  scale_x_continuous(name ="", 
                     breaks = c(-0.2, 1, 3, 6, 9), 
                     labels = c("0", "1", "3", "6", "9"), expand = c(0,0)) +  
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
STEROID_RR <- STEROID_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper))

# make table
data_table <- ggplot(data = STEROID_RR, aes(y = label)) +
  geom_text(aes(x = 0, label = label, size = 10), hjust = 0) +
  geom_text(aes(x = 2, label = RR, size = 10), hjust = 0.0) +
  geom_text(aes(x = 2.5, label = lower, size = 10), hjust = 0.0) +
  geom_text(aes(x = 3, label = upper, size = 10), hjust = 0) +
  geom_text(aes(x = 3.5, label = p, size = 10), hjust = 0) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 16),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 12)), 
                  ncol = 2,
                  widths = c(2, 1))

## Poly-med ####
PMED <- glm (PMED ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
             family = poisson, 
             data = as.data.frame(Events))
PMED_RR <- cbind (round (exp(coefficients(PMED)), 2), 
                  round (exp(confint.default(PMED)),2), 
                  (summary(PMED))$coefficients[,4])
PMED_RR <- as.data.frame(PMED_RR[2:10, ])
dmt <- c("Interferon-beta (2912)", 
         "B cell depletion (45)", 
         "Cladribine (10)",
         "Glatiramer acetate (3994)",
         "Natalizumab (47)",
         "S1P-R modulator (1352)",
         "Teriflunomide (1153)",
         "Dimethyl fumarate (3058)",
         "Alemtuzumab (7)")
eff <- c("1", "2", "2", "1", "2", "1", "1", "1", "2")
newnames <- c("RR", "lower", "upper", "p", "label")
PMED_RR$dmt <- dmt
setnames(PMED_RR, newnames)
PMED_RR$eff <- eff
PMED_RR <- PMED_RR %>% 
  mutate(label = factor (label, levels = c("Natalizumab (47)",
                                           "Cladribine (10)",
                                           "B cell depletion (45)",
                                           "Alemtuzumab (7)",
                                           "Teriflunomide (1153)",
                                           "S1P-R modulator (1352)",
                                           "Interferon-beta (2912)",
                                           "Glatiramer acetate (3994)",
                                           "Dimethyl fumarate (3058)")))

# make plot
p <- ggplot(data = PMED_RR, aes(x = RR, y = label))+
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=1) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#66903c",
                                 "#bea991",
                                 "#00a3d7",
                                 "#644a8c",
                                 "#cf3637",
                                 "#ecd121",
                                 "#001887",
                                 "#ff8900",
                                 "#008347")) +
  geom_vline(xintercept=1,linetype="dashed")+
  coord_cartesian (xlim=c(0, 20))+
  scale_x_continuous(name ="", 
                     breaks = c(0, 1, 5.0, 10.0, 15, 20), 
                     labels = c("0", "1", "5", "10", "15", "20"), expand = c(0,0)) +  
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
PMED_RR$p <- specify_decimal(PMED_RR$p, 4)
PMED_RR <- PMED_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper)) %>% 
  mutate(p = ifelse (p<0.0001, "<0.0001", p))
PMED_RR$RR <- specify_decimal(PMED_RR$RR, 2)
PMED_RR$lower <- specify_decimal(PMED_RR$lower, 2)
PMED_RR$upper <- specify_decimal(PMED_RR$upper, 2)


# make table
data_table <- ggplot(data = PMED_RR, aes(y = label, color = eff)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 8*5/14)+
  scale_colour_manual(values = c("#080c68", "#68080e")) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 8),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 8)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="PMCat.svg", width=5.0, height=2.7, units = "in", dpi=300)

## Relapse ####
RELAP <- glm (RELAP ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + COHORT_DRT + ELIWS + offset(log(DURATION)), 
              family = poisson, 
              data = as.data.frame(Events))
RELAP_RR <- cbind (round (exp(coefficients(RELAP)), 2), 
                   round (exp(confint.default(RELAP)),2), 
                   (summary(RELAP))$coefficients[,4])
RELAP_RR <- as.data.frame(RELAP_RR[2:10, ])
dmt <- c("Interferon-beta (2912)", 
         "B cell depletion (45)", 
         "Cladribine (10)",
         "Glatiramer acetate (3994)",
         "Natalizumab (47)",
         "S1P-R modulator (1352)",
         "Teriflunomide (1153)",
         "Dimethyl fumarate (3058)",
         "Alemtuzumab (7)")
eff <- c("1", "2", "2", "1", "2", "1", "1", "1", "2")
newnames <- c("RR", "lower", "upper", "p", "label")
RELAP_RR$dmt <- dmt
setnames(RELAP_RR, newnames)
RELAP_RR$eff <- eff
RELAP_RR <- RELAP_RR %>% 
  mutate(label = factor (label, levels = c("Natalizumab (47)",
                                           "Cladribine (10)",
                                           "B cell depletion (45)",
                                           "Alemtuzumab (7)",
                                           "Teriflunomide (1153)",
                                           "S1P-R modulator (1352)",
                                           "Interferon-beta (2912)",
                                           "Glatiramer acetate (3994)",
                                           "Dimethyl fumarate (3058)")))

# make plot
p <- ggplot(data = RELAP_RR, aes(x = RR, y = label))+
  geom_point(aes(colour = factor(label), fill = factor(label)), shape=16, size=1) +
  geom_pointrange(aes(colour = factor(label), xmin = lower, xmax = upper)) +
  scale_colour_manual(values = c("#66903c",
                                 "#bea991",
                                 "#00a3d7",
                                 "#644a8c",
                                 "#cf3637",
                                 "#ecd121",
                                 "#5c6268",
                                 "#56364d",
                                 "#5c4b36")) +
  geom_vline(xintercept=1,linetype="dashed")+
  coord_cartesian (xlim=c(-0.5, 15))+
  scale_x_continuous(name ="", 
                     breaks = c(-0.5, 1, 5, 10, 15), 
                     labels = c("0", "1", "5", "10", "15"), expand = c(0,0)) +  
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.ticks.y = element_blank(),
        plot.margin = margin(0, 20, 10, 10))
print(p)

# set upper boundary
RELAP_RR$p <- specify_decimal(RELAP_RR$p, 4)
RELAP_RR <- RELAP_RR %>%
  mutate (upper = ifelse (upper>100, ">100", upper)) %>% 
  mutate(p = ifelse (p<0.0001, "<0.0001", p))
RELAP_RR$RR <- specify_decimal(RELAP_RR$RR, 2)
RELAP_RR$lower <- specify_decimal(RELAP_RR$lower, 2)
RELAP_RR$upper <- specify_decimal(RELAP_RR$upper, 2)


# make table
data_table <- ggplot(data = RELAP_RR, aes(y = label, color = eff)) +
  geom_text(aes(x = 0, label = label), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 2, label = RR), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 2.5, label = lower), hjust = 0.0, size = 8*5/14) +
  geom_text(aes(x = 3, label = upper), hjust = 0, size = 8*5/14) +
  geom_text(aes(x = 3.5, label = p), hjust = 0, size = 8*5/14) +
  scale_colour_manual(values = c("#080c68", "#68080e")) +
  scale_x_continuous(name = "",
                     limits = c(0, 4), 
                     breaks = c(0.5, 2.1, 2.6, 3.1, 3.7), 
                     labels = c("DMT", "IRR", "LCI", "UCI", expression(paste(italic("P")))),
                     position = "top")+
  theme(text = element_text(size = 8),
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank())
print(data_table)

# Bind table and plot
bind <- ggarrange(data_table, 
                  p+theme(axis.text = element_text(size = 8)), 
                  ncol = 2,
                  widths = c(2, 1))
ggsave(bind, file="RECat.svg", width=5.0, height=2.7, units = "in", dpi=300)

# Generalized linear model for costs ####
library (sjPlot)
library (lm4)

# Costs - Efficacy (Figure 4) ####

## Trend for costs ####
Allcosts <- read.csv("Allcosts.csv") %>% 
  mutate (medoop6 = ifelse (medoop6 < 0, 0, medoop6)) %>%
  mutate (medoop12 = ifelse (medoop12 < 0, 0, medoop12)) %>%
  mutate (rxoop6 = ifelse (rxoop6 < 0, 0, rxoop6)) %>%
  mutate (rxoop12 = ifelse (rxoop12 < 0, 0, rxoop12)) %>% 
  mutate (oop6 = medoop6 + rxoop6) %>% 
  mutate (oop12 = medoop12 + rxoop12)

Costper <- Allcosts %>% 
  mutate (rxper = rxcharge12/charge12) %>% 
  group_by(DMTEff1) %>% 
  summarize(mean_rxper = mean(rxper, na.rm = TRUE))

### Medcharge trend ####
Medcharge_long <- Allcosts %>% dplyr::select (c(2:7,9:13,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("medcharge"),
               names_to = "time",
               names_prefix = "medcharge",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs + 0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(time == 0|time == 6|time == 12)


Medcharge_long_6 <- Medcharge_long %>% filter(time == 6)
Mclm_6 <- glm (costs_adjust ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
               data = Medcharge_long_6,
               family = Gamma (link = "log"))
Mclm_6_r <- cbind (coefficients(Mclm_6),
                   confint.default(Mclm_6),
                   (summary(Mclm_6)$coefficients[,4]))
write.csv(Mclm_6_r, "Mclm_6_r.csv")


Medcharge_long_12 <- Medcharge_long %>% filter(time == 12)
Mclm_12 <- glm (costs_adjust ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
                data = Medcharge_long_12,
                family = Gamma (link = "log"))
Mclm_12_r <- cbind (coefficients(Mclm_12),
                    confint.default(Mclm_12),
                    (summary(Mclm_12)$coefficients[,4]))
write.csv(Mclm_12_r, "Mclm_12_r.csv")

p <- ggplot(Medcharge_long, aes(x=time, y=costs_adjust, group = factor(DMTEff1), color = factor (DMTEff1)))+
  geom_point(stat = "summary", fun = "mean", size = 3) +
  scale_color_manual(name = "", 
                     labels = c("No DMT (16487)", "Standard-efficacy DMT (12419)", "High-efficacy DMT (109)"), 
                     values = c("#1b1e23", "#080c68", "#68080e")) +
  geom_line(stat = "summary", fun = "mean", size = 1) +
  geom_ribbon(aes(group = factor(DMTEff1), fill = factor(DMTEff1)), 
              stat = "summary", fun.data = "mean_cl_normal",  show.legend = FALSE, alpha = 0.2) +
  scale_fill_manual("", values = c("#1b1e23", "#080c68", "#68080e")) +
  theme_classic()+
  labs(title="",
       x ="Months", 
       y ="USD ($)") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.line = element_line(size = 0.8),
        panel.grid.major.y = element_line(size = 0.5, color = "gray90"),
        legend.text = element_text(size = 8),
        legend.position = "right") +
  coord_cartesian (ylim = c(0, 80000)) +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 3), expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
print(p)
ggsave(p, file="MCEff.svg", width=6.6, height=2.7, units = "in", dpi=300)


## Medopp trend ####
Medoop_long <- Allcosts %>% dplyr::select (c(2:7,14:18,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("medoop"),
               names_to = "time",
               names_prefix = "medoop",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs + 0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(time == 0|time == 6|time == 12)

Medoop_long_6 <- Medoop_long %>% filter(time == 6)
Molm_6 <- glm (costs_adjust ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
               data = Medoop_long_6,
               family = gaussian (link = "log"))
Molm_6_r <- cbind (coefficients(Molm_6),
                   confint.default(Molm_6),
                   (summary(Molm_6)$coefficients[,4]))
write.csv(Molm_6_r, "Molm_6_r.csv")


Medoop_long_12 <- Medoop_long %>% filter(time == 12)
Molm_12 <- glm (costs_adjust ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
                data = Medoop_long_12,
                family = gaussian (link = "log"))
Molm_12_r <- cbind (coefficients(Molm_12),
                    confint.default(Molm_12),
                    (summary(Molm_12)$coefficients[,4]))
write.csv(Molm_12_r, "Molm_12_r.csv")

p <- ggplot(Medoop_long, aes(x=time, y=costs_adjust, group = factor(DMTEff1), color = factor (DMTEff1)))+
  geom_point(stat = "summary", fun = "mean", size = 3) +
  scale_color_manual(name = "", 
                     labels = c("No DMT (16487)", "Standard-efficacy DMT (12419)", "High-efficacy DMT (109)"), 
                     values = c("#1b1e23", "#080c68", "#68080e")) +
  geom_line(stat = "summary", fun = "mean", size = 1) +
  geom_ribbon(aes(group = factor(DMTEff1), fill = factor(DMTEff1)), 
              stat = "summary", fun.data = "mean_cl_normal",  show.legend = FALSE, alpha = 0.2) +
  scale_fill_manual("", values = c("#1b1e23", "#080c68", "#68080e")) +
  theme_classic()+
  labs(title="",
       x ="Months", 
       y ="USD ($)") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.line = element_line(size = 0.8),
        panel.grid.major.y = element_line(size = 0.5, color = "gray90"),
        legend.text = element_text(size = 8),
        legend.position = "right") +
  coord_cartesian (ylim = c(0, 1200)) +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 3), expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
print(p)
ggsave(p, file="MOEff.svg", width=6.6, height=2.7, units = "in", dpi=300)


### Charge trend ####
Charge_long <- Allcosts %>% dplyr::select (c(2:7,39:43,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("charge"),
               names_to = "time",
               names_prefix = "charge",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs+0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(time == 0|time == 6|time == 12)

Charge_long_6 <- Charge_long %>% filter(time == 6)
Clm_6 <- glm (costs_adjust ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
              data = Charge_long_6,
              family = Gamma (link = "log"))

Clm_6_r <- cbind (coefficients(Clm_6),
                  confint.default(Clm_6),
                  (summary(Clm_6)$coefficients[,4]))
write.csv(Clm_6_r, "Clm_6_r.csv")


Charge_long_12 <- Charge_long %>% filter(time == 12)
Clm_12 <- glm (costs_adjust ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
               data = Charge_long_12,
               family = Gamma (link = "log"))
Clm_12_r <- cbind (coefficients(Clm_12),
                   confint.default(Clm_12),
                   (summary(Clm_12)$coefficients[,4]))
write.csv(Clm_12_r, "Clm_12_r.csv")

p <- ggplot(Charge_long, aes(x=time, y=costs_adjust, group = factor(DMTEff1), color = factor (DMTEff1)))+
  geom_point(stat = "summary", fun = "mean", size = 3) +
  scale_color_manual(name = "", 
                     labels = c("No DMT (16487)", "Standard-efficacy DMT (12419)", "High-efficacy DMT (109)"), 
                     values = c("#1b1e23", "#080c68", "#68080e")) +
  geom_line(stat = "summary", fun = "mean", size = 1) +
  geom_ribbon(aes(group = factor(DMTEff1), fill = factor(DMTEff1)), 
              stat = "summary", fun.data = "mean_cl_normal",  show.legend = FALSE, alpha = 0.2) +
  scale_fill_manual("", values = c("#1b1e23", "#080c68", "#68080e")) +
  theme_classic()+
  labs(title="",
       x ="Months", 
       y ="USD ($)") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.line = element_line(size = 0.8),
        panel.grid.major.y = element_line(size = 0.5, color = "gray90"),
        legend.text = element_text(size = 8),
        legend.position = "right") +
  coord_cartesian (ylim = c(0, 120000)) +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 3), expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
print(p)
ggsave(p, file="TCEff.svg", width=6.6, height=2.7, units = "in", dpi=300)


## Oop trend ####
Oop_long <- Allcosts %>% dplyr::select (c(2:7,44:48,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("oop"),
               names_to = "time",
               names_prefix = "oop",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs+0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(time == 0|time == 6|time == 12)

Oop_long_6 <- Oop_long %>% filter(time == 6)
Olm_6 <- glm (costs_adjust ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
              data = Oop_long_6,
              family = Gamma (link = "log"))
Olm_6_r <- cbind (coefficients(Olm_6),
                  confint.default(Olm_6),
                  (summary(Olm_6)$coefficients[,4]))
write.csv(Olm_6_r, "Olm_6_r.csv")


Oop_long_12 <- Oop_long %>% filter(time == 12)
Olm_12 <- glm (costs_adjust ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
               data = Oop_long_12,
               family = Gamma (link = "log"))
Olm_12_r <- cbind (coefficients(Olm_12),
                   confint.default(Olm_12),
                   (summary(Olm_12)$coefficients[,4]))
write.csv(Olm_12_r, "Olm_12_r.csv")

p <- ggplot(Oop_long, aes(x=time, y=costs_adjust, group = factor(DMTEff1), color = factor (DMTEff1)))+
  geom_point(stat = "summary", fun = "mean", size = 3) +
  scale_color_manual(name = "", 
                     labels = c("No DMT (16487)", "Standard-efficacy DMT (12419)", "High-efficacy DMT (109)"), 
                     values = c("#1b1e23", "#080c68", "#68080e")) +
  geom_line(stat = "summary", fun = "mean", size = 1) +
  geom_ribbon(aes(group = factor(DMTEff1), fill = factor(DMTEff1)), 
              stat = "summary", fun.data = "mean_cl_normal",  show.legend = FALSE, alpha = 0.2) +
  scale_fill_manual("", values = c("#1b1e23", "#080c68", "#68080e")) +
  theme_classic()+
  labs(title="",
       x ="Months", 
       y ="USD ($)") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.line = element_line(size = 0.8),
        panel.grid.major.y = element_line(size = 0.5, color = "gray90"),
        legend.text = element_text(size = 8),
        legend.position = "right") +
  coord_cartesian (ylim = c(0, 3000)) +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 3), expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
print(p)
ggsave(p, file="TOEff.svg", width=6.6, height=2.7, units = "in", dpi=300)

## Rxcharge trend ####
Rxcharge_long <- Allcosts %>% dplyr::select (c(2:7,19:23,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("rxcharge"),
               names_to = "time",
               names_prefix = "rxcharge",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs+0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(time == 0|time == 6|time == 12)


Rxcharge_long_6 <- Rxcharge_long %>% 
  filter(time == 6)
Rclm_6 <- glm (costs_adjust ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
               data = Rxcharge_long_6,
               family = gaussian (link = "log"))
Rclm_6_r <- cbind (coefficients(Rclm_6),
                   confint.default(Rclm_6),
                   (summary(Rclm_6)$coefficients[,4]))
write.csv(Rclm_6_r, "Rclm_6_r.csv")


Rxcharge_long_12 <- Rxcharge_long %>% 
  filter(time == 12)
Rclm_12 <- glm (costs_adjust ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
                data = Rxcharge_long_12,
                family = gaussian (link = "log"))
Rclm_12_r <- cbind (coefficients(Rclm_12),
                    confint.default(Rclm_12),
                    (summary(Rclm_12)$coefficients[,4]))
write.csv(Rclm_12_r, "Rclm_12_r.csv")

p <- ggplot(Rxcharge_long, aes(x=time, y=costs_adjust, group = factor(DMTEff1), color = factor (DMTEff1)))+
  geom_point(stat = "summary", fun = "mean", size = 3) +
  scale_color_manual(name = "", 
                     labels = c("No DMT (16487)", "Standard-efficacy DMT (12419)", "High-efficacy DMT (109)"), 
                     values = c("#1b1e23", "#080c68", "#68080e")) +
  geom_line(stat = "summary", fun = "mean", size = 1) +
  geom_ribbon(aes(group = factor(DMTEff1), fill = factor(DMTEff1)), 
              stat = "summary", fun.data = "mean_cl_normal",  show.legend = FALSE, alpha = 0.2) +
  scale_fill_manual("", values = c("#1b1e23", "#080c68", "#68080e")) +
  theme_classic()+
  labs(title="",
       x ="Months", 
       y ="USD ($)") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.line = element_line(size = 0.8),
        panel.grid.major.y = element_line(size = 0.5, color = "gray90"),
        legend.text = element_text(size = 8),
        legend.position = "right") +
  coord_cartesian (ylim = c(0, 80000)) +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 3), expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
print(p)
ggsave(p, file="RCEff.svg", width=6.6, height=2.7, units = "in", dpi=300)




## Rxoop trend ####
Rxoop_long <- Allcosts %>% dplyr::select (c(2:7,24:28,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("rxoop"),
               names_to = "time",
               names_prefix = "rxoop",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs+0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(time == 0|time == 6|time == 12)


Rxoop_long_6 <- Rxoop_long %>% filter(time == 6)
Rolm_6 <- glm (costs_adjust ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
               data = Rxoop_long_6,
               family = gaussian (link = "log"))
Rolm_6_r <- cbind (coefficients(Rolm_6),
                   confint.default(Rolm_6),
                   (summary(Rolm_6)$coefficients[,4]))
write.csv(Rolm_6_r, "Rolm_6_r.csv")


Rxoop_long_12 <- Rxoop_long %>% filter(time == 12)
Rolm_12 <- glm (costs_adjust ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
                data = Rxoop_long_12,
                family = gaussian (link = "log"))
Rolm_12_r <- cbind (coefficients(Rolm_12),
                    confint.default(Rolm_12),
                    (summary(Rolm_12)$coefficients[,4]))
write.csv(Rolm_12_r, "Rolm_12_r.csv")

p <- ggplot(Rxoop_long, aes(x=time, y=costs_adjust, group = factor(DMTEff1), color = factor (DMTEff1)))+
  geom_point(stat = "summary", fun = "mean", size = 3) +
  scale_color_manual(name = "", 
                     labels = c("No DMT (16487)", "Standard-efficacy DMT (12419)", "High-efficacy DMT (109)"), 
                     values = c("#1b1e23", "#080c68", "#68080e")) +
  geom_line(stat = "summary", fun = "mean", size = 1) +
  geom_ribbon(aes(group = factor(DMTEff1), fill = factor(DMTEff1)), 
              stat = "summary", fun.data = "mean_cl_normal",  show.legend = FALSE, alpha = 0.2) +
  scale_fill_manual("", values = c("#1b1e23", "#080c68", "#68080e")) +
  theme_classic()+
  labs(title="",
       x ="Months", 
       y ="USD ($)") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.line = element_line(size = 0.8),
        panel.grid.major.y = element_line(size = 0.5, color = "gray90"),
        legend.text = element_text(size = 8),
        legend.position = "right") +
  coord_cartesian (ylim = c(0, 2500)) +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 3), expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
print(p)
ggsave(p, file="ROEff.svg", width=6.6, height=2.7, units = "in", dpi=300)




## Dmtcharge trend ####
Dmtcharge_long <- Allcosts %>% dplyr::select (c(2:7,29:33,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("dmtcharge"),
               names_to = "time",
               names_prefix = "dmtcharge",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs+0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(time == 0|time == 6|time == 12)

Dmtcharge_long_6 <- Dmtcharge_long %>% 
  filter(time == 6) %>% 
  filter(DMTCat != 0)
Dclm_6 <- glm (costs_adjust ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
               data = Dmtcharge_long_6,
               family = gaussian (link = "log"))
Dclm_6_r <- cbind (coefficients(Dclm_6),
                   confint.default(Dclm_6),
                   (summary(Dclm_6)$coefficients[,4]))
write.csv(Dclm_6_r, "Dclm_6_r.csv")


Dmtcharge_long_12 <- Dmtcharge_long %>% 
  filter(time == 12) %>% 
  filter(DMTCat != 0) 
Dclm_12 <- glm (costs_adjust ~ DMTEff1 + RACE + INDAGE + REGION + DURATION + ELIWS,
                data = Dmtcharge_long_12,
                family = Gamma (link = "log"))
Dclm_12_r <- cbind (coefficients(Dclm_12),
                    confint.default(Dclm_12),
                    (summary(Dclm_12)$coefficients[,4]))
write.csv(Dclm_12_r, "Dclm_12_r.csv")

p <- ggplot(Dmtcharge_long, aes(x=time, y=costs_adjust, group = factor(DMTEff1), color = factor (DMTEff1)))+
  geom_point(stat = "summary", fun = "mean", size = 3) +
  scale_color_manual(name = "", 
                     labels = c("No DMT (16487)", "Standard-efficacy DMT (12419)", "High-efficacy DMT (109)"), 
                     values = c("#1b1e23", "#080c68", "#68080e")) +
  geom_line(stat = "summary", fun = "mean", size = 1) +
  geom_ribbon(aes(group = factor(DMTEff1), fill = factor(DMTEff1)), 
              stat = "summary", fun.data = "mean_cl_normal",  show.legend = FALSE, alpha = 0.2) +
  scale_fill_manual("", values = c("#1b1e23", "#080c68", "#68080e")) +
  theme_classic()+
  labs(title="",
       x ="Months", 
       y ="USD ($)") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.line = element_line(size = 0.8),
        panel.grid.major.y = element_line(size = 0.5, color = "gray90"),
        legend.text = element_text(size = 8),
        legend.position = "right") +
  coord_cartesian (ylim = c(0, 60000)) +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 3), expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
print(p)
ggsave(p, file="DCEff.svg", width=6.6, height=2.7, units = "in", dpi=300)

## DMToop trend ####
Dmtoop_long <- Allcosts %>% dplyr::select (c(2:7,34:38,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("dmtoop"),
               names_to = "time",
               names_prefix = "dmtoop",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs+0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(time == 0|time == 6|time == 12)

Dmtoop_long_6 <- Dmtoop_long %>% 
  filter(time == 6) %>% 
  filter(DMTCat != 0)
Dolm_6 <- glm (costs_adjust ~ DMTEff1 + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
               data = Dmtoop_long_6,
               family = gaussian (link = "log"))
Dolm_6_r <- cbind (coefficients(Dolm_6),
                   confint.default(Dolm_6),
                   (summary(Dolm_6)$coefficients[,4]))
write.csv(Dolm_6_r, "Dolm_6_r.csv")

Dmtoop_long_12 <- Dmtoop_long %>% 
  filter(time == 12) %>% 
  filter(DMTCat != 0) 
Dolm_12 <- glm (costs_adjust ~ DMTEff1 + RACE + INDAGE + REGION + DURATION + ELIWS,
                data = Dmtoop_long_12,
                family = gaussian (link = "log"))
Dolm_12_r <- cbind (coefficients(Dolm_12),
                    confint.default(Dolm_12),
                    (summary(Dolm_12)$coefficients[,4]))
write.csv(Dolm_12_r, "Dolm_12_r.csv")


p <- ggplot(Dmtoop_long, aes(x=time, y=costs_adjust, group = factor(DMTEff1), color = factor (DMTEff1)))+
  geom_point(stat = "summary", fun = "mean", size = 3) +
  scale_color_manual(name = "", 
                     labels = c("No DMT (16487)", "Standard-efficacy DMT (12419)", "High-efficacy DMT (109)"), 
                     values = c("#1b1e23", "#080c68", "#68080e")) +
  geom_line(stat = "summary", fun = "mean", size = 1) +
  geom_ribbon(aes(group = factor(DMTEff1), fill = factor(DMTEff1)), 
              stat = "summary", fun.data = "mean_cl_normal",  show.legend = FALSE, alpha = 0.2) +
  scale_fill_manual("", values = c("#1b1e23", "#080c68", "#68080e")) +
  theme_classic()+
  labs(title="",
       x ="Months", 
       y ="USD ($)") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.line = element_line(size = 0.8),
        panel.grid.major.y = element_line(size = 0.5, color = "gray90"),
        legend.text = element_text(size = 8),
        legend.position = "right") +
  coord_cartesian (ylim = c(0, 2000)) +
  scale_x_continuous(limits = c(0, 12.5), breaks = seq(0, 12, by = 3), expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))
print(p)
ggsave(p, file="DOEff.svg", width=6.6, height=2.7, units = "in", dpi=300)

# Costs - DMT Class (Table 2)####
## Generalized linear model for costs ####
### Charge trend ####
Charge_long <- Allcosts %>% dplyr::select (c(2:7,39:43,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("charge"),
               names_to = "time",
               names_prefix = "charge",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs+0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(DMT != "AMZ") %>% 
  filter(DMT != "CLD") %>% 
  filter(time == 0|time == 6|time == 12)

Charge_long_6 <- Charge_long %>% filter(time == 6)
Clm_6 <- glm (costs_adjust ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
              data = Charge_long_6,
              family = Gamma (link = "log"))

Clm_6_r <- cbind (coefficients(Clm_6),
                  confint.default(Clm_6),
                  (summary(Clm_6)$coefficients[,4]))
write.csv(Clm_6_r, "Clm_6_r.csv")


Charge_long_12 <- Charge_long %>% filter(time == 12)
Clm_12 <- glm (costs_adjust ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
               data = Charge_long_12,
               family = Gamma (link = "log"))
Clm_12_r <- cbind (coefficients(Clm_12),
                   confint.default(Clm_12),
                   (summary(Clm_12)$coefficients[,4]))
write.csv(Clm_12_r, "Clm_12_r.csv")

## Oop trend ####
Oop_long <- Allcosts %>% dplyr::select (c(2:7,44:48,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("oop"),
               names_to = "time",
               names_prefix = "oop",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs+0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(DMT != "AMZ") %>% 
  filter(DMT != "CLD") %>% 
  filter(time == 0|time == 6|time == 12)

Oop_long_6 <- Oop_long %>% filter(time == 6)
Olm_6 <- glm (costs_adjust ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
              data = Oop_long_6,
              family = Gamma (link = "log"))
Olm_6_r <- cbind (coefficients(Olm_6),
                  confint.default(Olm_6),
                  (summary(Olm_6)$coefficients[,4]))
write.csv(Olm_6_r, "Olm_6_r.csv")


Oop_long_12 <- Oop_long %>% filter(time == 12)
Olm_12 <- glm (costs_adjust ~ DMTCat,
               data = Oop_long_12,
               family = Gamma (link = "log"))
Olm_12_r <- cbind (coefficients(Olm_12),
                   confint.default(Olm_12),
                   (summary(Olm_12)$coefficients[,4]))
write.csv(Olm_12_r, "Olm_12_r.csv")

### Medcharge trend ####
Medcharge_long <- Allcosts %>% dplyr::select (c(2:7,9:13,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("medcharge"),
               names_to = "time",
               names_prefix = "medcharge",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs + 0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(DMT != "AMZ") %>% 
  filter(DMT != "CLD") %>%
  filter(time == 0|time == 6|time == 12)


Medcharge_long_6 <- Medcharge_long %>% filter(time == 6)
Mclm_6 <- glm (costs_adjust ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
               data = Medcharge_long_6,
               family = Gamma (link = "log"))
Mclm_6_r <- cbind (coefficients(Mclm_6),
                   confint.default(Mclm_6),
                   (summary(Mclm_6)$coefficients[,4]))
write.csv(Mclm_6_r, "Mclm_6_r.csv")


Medcharge_long_12 <- Medcharge_long %>% filter(time == 12)
Mclm_12 <- glm (costs_adjust ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
                data = Medcharge_long_12,
                family = Gamma (link = "log"))
Mclm_12_r <- cbind (coefficients(Mclm_12),
                    confint.default(Mclm_12),
                    (summary(Mclm_12)$coefficients[,4]))
write.csv(Mclm_12_r, "Mclm_12_r.csv")

## Medopp trend ####
Medoop_long <- Allcosts %>% dplyr::select (c(2:7,14:18,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("medoop"),
               names_to = "time",
               names_prefix = "medoop",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs + 0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(DMT != "AMZ") %>% 
  filter(DMT != "CLD") %>%
  filter(time == 0|time == 6|time == 12)

Medoop_long_6 <- Medoop_long %>% filter(time == 6)
Molm_6 <- glm (costs_adjust ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
               data = Medoop_long_6,
               family = gaussian (link = "log"))
Molm_6_r <- cbind (coefficients(Molm_6),
                   confint.default(Molm_6),
                   (summary(Molm_6)$coefficients[,4]))
write.csv(Molm_6_r, "Molm_6_r.csv")


Medoop_long_12 <- Medoop_long %>% filter(time == 12)
Molm_12 <- glm (costs_adjust ~ DMTCat,
                data = Medoop_long_12,
                family = gaussian (link = "log"))
Molm_12_r <- cbind (coefficients(Molm_12),
                    confint.default(Molm_12),
                    (summary(Molm_12)$coefficients[,4]))
write.csv(Molm_12_r, "Molm_12_r.csv")

## Rxcharge trend ####
Rxcharge_long <- Allcosts %>% dplyr::select (c(2:7,19:23,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("rxcharge"),
               names_to = "time",
               names_prefix = "rxcharge",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs+0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(DMT != "AMZ") %>% 
  filter(DMT != "CLD") %>%
  filter(time == 0|time == 6|time == 12)


Rxcharge_long_6 <- Rxcharge_long %>% 
  filter(time == 6)
Rclm_6 <- glm (costs_adjust ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
               data = Rxcharge_long_6,
               family = gaussian (link = "log"))
Rclm_6_r <- cbind (coefficients(Rclm_6),
                   confint.default(Rclm_6),
                   (summary(Rclm_6)$coefficients[,4]))
write.csv(Rclm_6_r, "Rclm_6_r.csv")


Rxcharge_long_12 <- Rxcharge_long %>% 
  filter(time == 12)
Rclm_12 <- glm (costs_adjust ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
                data = Rxcharge_long_12,
                family = gaussian (link = "log"))
Rclm_12_r <- cbind (coefficients(Rclm_12),
                    confint.default(Rclm_12),
                    (summary(Rclm_12)$coefficients[,4]))
write.csv(Rclm_12_r, "Rclm_12_r.csv")

## Rxoop trend ####
Rxoop_long <- Allcosts %>% dplyr::select (c(2:7,24:28,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("rxoop"),
               names_to = "time",
               names_prefix = "rxoop",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs+0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(DMT != "AMZ") %>% 
  filter(DMT != "CLD") %>%
  filter(time == 0|time == 6|time == 12)


Rxoop_long_6 <- Rxoop_long %>% filter(time == 6)
Rolm_6 <- glm (costs_adjust ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
               data = Rxoop_long_6,
               family = gaussian (link = "log"))
Rolm_6_r <- cbind (coefficients(Rolm_6),
                   confint.default(Rolm_6),
                   (summary(Rolm_6)$coefficients[,4]))
write.csv(Rolm_6_r, "Rolm_6_r.csv")


Rxoop_long_12 <- Rxoop_long %>% filter(time == 12)
Rolm_12 <- glm (costs_adjust ~ DMTCat,
                data = Rxoop_long_12,
                family = gaussian (link = "log"))
Rolm_12_r <- cbind (coefficients(Rolm_12),
                    confint.default(Rolm_12),
                    (summary(Rolm_12)$coefficients[,4]))
write.csv(Rolm_12_r, "Rolm_12_r.csv")

## Dmtcharge trend ####
Dmtcharge_long <- Allcosts %>% dplyr::select (c(2:7,29:33,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("dmtcharge"),
               names_to = "time",
               names_prefix = "dmtcharge",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs+0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(DMT != "AMZ") %>% 
  filter(DMT != "CLD") %>%
  filter(time == 0|time == 6|time == 12)

Dmtcharge_long_6 <- Dmtcharge_long %>% 
  filter(time == 6) %>% 
  filter(DMTCat != 0)
Dclm_6 <- glm (costs_adjust ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
               data = Dmtcharge_long_6,
               family = gaussian (link = "log"))
Dclm_6_r <- cbind (coefficients(Dclm_6),
                   confint.default(Dclm_6),
                   (summary(Dclm_6)$coefficients[,4]))
write.csv(Dclm_6_r, "Dclm_6_r.csv")


Dmtcharge_long_12 <- Dmtcharge_long %>% 
  filter(time == 12) %>% 
  filter(DMTCat != 0) 
Dclm_12 <- glm (costs_adjust ~ DMTCat + RACE + INDAGE + REGION + DURATION + ELIWS,
                data = Dmtcharge_long_12,
                family = Gamma (link = "log"))
Dclm_12_r <- cbind (coefficients(Dclm_12),
                    confint.default(Dclm_12),
                    (summary(Dclm_12)$coefficients[,4]))
write.csv(Dclm_12_r, "Dclm_12_r.csv")

## DMToop trend ####
Dmtoop_long <- Allcosts %>% dplyr::select (c(2:7,34:38,50:54,59,62:63,65)) %>% 
  pivot_longer(cols = starts_with("dmtoop"),
               names_to = "time",
               names_prefix = "dmtoop",
               values_to = "costs",
               values_drop_na = TRUE) %>% 
  mutate (DMTCat = as.character(DMTCat)) %>% 
  mutate (DMTEff1 = as.factor(DMTEff1)) %>% 
  mutate (time = as.numeric(time)) %>% 
  mutate(costs_adjust = costs+0.001) %>% 
  filter(DMT != "DAC") %>% 
  filter(DMT != "AMZ") %>% 
  filter(DMT != "CLD") %>%
  filter(time == 0|time == 6|time == 12)

Dmtoop_long_6 <- Dmtoop_long %>% 
  filter(time == 6) %>% 
  filter(DMTCat != 0)
Dolm_6 <- glm (costs_adjust ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
               data = Dmtoop_long_6,
               family = gaussian (link = "log"))
Dolm_6_r <- cbind (coefficients(Dolm_6),
                   confint.default(Dolm_6),
                   (summary(Dolm_6)$coefficients[,4]))
write.csv(Dolm_6_r, "Dolm_6_r.csv")

Dmtoop_long_12 <- Dmtoop_long %>% 
  filter(time == 12) %>% 
  filter(DMTCat != 0) 
Dolm_12 <- glm (costs_adjust ~ DMTCat + GDR_CD + RACE + INDAGE + REGION + ENR_DRT + ELIWS,
                data = Dmtoop_long_12,
                family = gaussian (link = "log"))
Dolm_12_r <- cbind (coefficients(Dolm_12),
                    confint.default(Dolm_12),
                    (summary(Dolm_12)$coefficients[,4]))
write.csv(Dolm_12_r, "Dolm_12_r.csv")
# ####
# DMT trend (Supplementary Figure 1) ####
Trend <- read.csv("DMTTrend.csv")
Trend <- Trend %>% 
  mutate(year = as.character(year))

legend_ord <- levels(with(Trend, reorder(DMTLab, DMTEff1)))

p <- ggplot(Trend, aes(x = year, y = percent, 
                       group = factor(DMTLab), color = factor(DMTLab), fill = factor(DMTLab))) +
  geom_point(size = 2.5, alpha = 0.7) +
  scale_color_manual(name = "",
                     values = c("#68080e",
                                "#1b1e23",
                                "#008347",
                                "#ff8900",
                                "#001887",
                                "#ecd121",
                                "#cf3637",
                                "#644a8c",
                                "#00a3d7",
                                "#bea991",
                                "#66903c"),
                     breaks=legend_ord) +
  scale_fill_manual(name = "",
                    values = c("#68080e",
                               "#1b1e23",
                               "#008347",
                               "#ff8900",
                               "#001887",
                               "#ecd121",
                               "#cf3637",
                               "#644a8c",
                               "#00a3d7",
                               "#bea991",
                               "#66903c"),
                    breaks=legend_ord) +
  geom_line(size = 1) +
  theme_minimal()+
  labs(title="",
       x ="Year", 
       y ="Percentage of Total Patients")+
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.position = "top",
        legend.title = element_blank())

print(p)
ggsave(p, file="DMTtrend.svg", width=5.3, height=4, units = "in", dpi=300)


# Individual DMT trend ####
TrendIND <- Trend %>% 
  mutate (time = as.numeric(year)) %>% 
  filter (DMTLab == "Natalizumab") %>% 
  filter (percent != 0)

# AD = DMTLab == "Any DMT"
# InjD = DMTLab == "Interferon-beta"|DMTLab == "Glatiramer acetate"
# DMF = DMTLab == "Dimethyl fumarate"
# TER = DMTLab == "Teriflunomide"
# S1PR = DMTLab == "S1P-R modulator"
# NAT = DMTLab == "Natalizumab"

AD <- lm(percent ~ time, data = TrendIND)
summary (AD)

InjD <- lm(percent ~ time, data = TrendIND)
summary (InjD)

DMF <- lm(percent ~ time, data = TrendIND)
summary (DMF)

TER <- lm(percent ~ time, data = TrendIND)
summary (TER)

S1PR <- lm(percent ~ time, data = TrendIND)
summary (S1PR)

NAT <- lm(percent ~ time, data = TrendIND)
summary (NAT)


# ####
# DMT duration (Supplementary Figure 1) ####

MainDuration <- read.csv("MainDuration.csv")

AMZD <- MainDuration %>% 
  filter(AMZ == 1) %>% 
  mutate(days = AMZ_Days) %>% 
  mutate(n = 63) %>% 
  mutate(DMTLab = "Alemtuzumab (63)") %>% 
  mutate(DMTEff1 = 2) %>% 
  select(c(77:81))

CLDD <- MainDuration %>% 
  filter(CLD == 1) %>% 
  mutate(days = CLD_Days) %>% 
  mutate(n = 57) %>% 
  mutate(DMTLab = "Cladribine (57)") %>% 
  mutate(DMTEff1 = 2) %>% 
  select(c(77:81))

DACD <- MainDuration %>% 
  filter(DAC == 1) %>% 
  mutate(days = DAC_Days) %>% 
  mutate(n = 111) %>% 
  mutate(DMTLab = "Daclizumab (111)") %>% 
  mutate(DMTEff1 = 1) %>% 
  select(c(77:81))

DMFD <- MainDuration %>% 
  filter(DMF == 1) %>% 
  mutate(days = DMF_Days) %>% 
  mutate(n = 13239) %>% 
  mutate(DMTLab = "Dimethyl fumarate (13239)") %>% 
  mutate(DMTEff1 = 1) %>% 
  select(c(77:81))

FGLD <- MainDuration %>% 
  filter(FGL == 1) %>% 
  mutate(days = FGL_Days) %>% 
  mutate(n = 6719) %>% 
  mutate(DMTLab = "S1P-R modulator (6766)") %>% 
  mutate(DMTEff1 = 1) %>% 
  select(c(77:81))

SIPD <- MainDuration %>% 
  filter(SIP == 1) %>% 
  mutate(days = SIP_Days) %>% 
  mutate(n = 47) %>% 
  mutate(DMTLab = "S1P-R modulator (6766)") %>% 
  mutate(DMTEff1 = 1) %>% 
  select(c(77:81))

GAD <- MainDuration %>% 
  filter(GA == 1) %>% 
  mutate(days = GA_Days) %>% 
  mutate(n = 19243) %>% 
  mutate(DMTLab = "Glatiramer acetate (19243)") %>% 
  mutate(DMTEff1 = 1) %>% 
  select(c(77:81))

INFB1AD <- MainDuration %>% 
  filter(INFB1A == 1) %>% 
  mutate(days = INFB1A_Days) %>% 
  mutate(n = 15206) %>% 
  mutate(DMTLab = "Interferon-beta (19678)") %>% 
  mutate(DMTEff1 = 1) %>% 
  select(c(77:81))

INFB1BD <- MainDuration %>% 
  filter(INFB1B == 1) %>% 
  mutate(days = INFB1B_Days) %>% 
  mutate(n = 3762) %>% 
  mutate(DMTLab = "Interferon-beta (19678)") %>% 
  mutate(DMTEff1 = 1) %>% 
  select(c(77:81))

PINF1AD <- MainDuration %>% 
  filter(PINF1A == 1) %>% 
  mutate(days = PINF1A_Days) %>% 
  mutate(n = 710) %>% 
  mutate(DMTLab = "Interferon-beta (19678)") %>% 
  mutate(DMTEff1 = 1) %>% 
  select(c(77:81))

MTXD <- MainDuration %>% 
  filter(MTX == 1) %>% 
  mutate(days = MTX_Days) %>% 
  mutate(n = 3) %>% 
  mutate(DMTLab = "Mitoxantrone (3)") %>% 
  mutate(DMTEff1 = 2) %>% 
  select(c(77:81))

NATD <- MainDuration %>% 
  filter(NAT == 1) %>% 
  mutate(days = NAT_Days) %>% 
  mutate(n = 431) %>% 
  mutate(DMTLab = "Natalizumab (431)") %>% 
  mutate(DMTEff1 = 2) %>% 
  select(c(77:81))

OCRD <- MainDuration %>% 
  filter(OCR == 1) %>% 
  mutate(days = OCR_Days) %>% 
  mutate(n = 313) %>% 
  mutate(DMTLab = "B cell depletion (359)") %>% 
  mutate(DMTEff1 = 2) %>% 
  select(c(77:81))

RTXD <- MainDuration %>% 
  filter(RTX == 1) %>% 
  mutate(days = RTX_Days) %>% 
  mutate(n = 46) %>% 
  mutate(DMTLab = "B cell depletion (359)") %>% 
  mutate(DMTEff1 = 2) %>% 
  select(c(77:81))

TERD <- MainDuration %>% 
  filter(TER == 1) %>% 
  mutate(days = TER_Days) %>% 
  mutate(n = 5648) %>% 
  mutate(DMTLab = "Teriflunomide (5648)") %>% 
  mutate(DMTEff1 = 1) %>% 
  select(c(77:81))

NDD <- MainDuration %>% 
  filter(MULDMT == 0) %>% 
  mutate(days = 0) %>% 
  mutate(n = 47755) %>% 
  mutate(DMTLab = "No DMT (47755)") %>% 
  mutate(DMTEff1 = 0) %>% 
  select(c(77:81))

DMTDuration <- bind_rows(AMZD, CLDD) %>% 
  bind_rows(., DACD) %>% 
  bind_rows(., DMFD) %>%
  bind_rows(., FGLD) %>%
  bind_rows(., GAD) %>%
  bind_rows(., INFB1AD) %>%
  bind_rows(., INFB1BD) %>%
  bind_rows(., MTXD) %>%
  bind_rows(., NATD) %>%
  bind_rows(., NDD) %>%
  bind_rows(., OCRD) %>%
  bind_rows(., PINF1AD) %>%
  bind_rows(., RTXD) %>%
  bind_rows(., SIPD) %>%
  bind_rows(., TERD) %>% 
  mutate(month = days/30)
DMTDuration$PATID_N <- seq.int(nrow(DMTDuration))
write.csv(DMTDuration, "DMTDuration.csv")


# DMTDuration<- MainDuration %>%
#   mutate (DMTEff1 = as.character(DMTEff1)) %>%
#   mutate (DMTLab = ifelse(DMT == "NAT", "Natalizumab", DMT)) %>%
#   mutate (DMTLab = ifelse(DMTLab == "AMZ", "Alemtuzumab", DMTLab)) %>%
#   mutate (DMTLab = ifelse(DMTLab == "OCR"|DMTLab == "RTX",
#                           "B cell depletion", DMTLab)) %>%
#   mutate (DMTLab = ifelse(DMTLab == "CLD", "Cladribine", DMTLab)) %>%
#   mutate (DMTLab = ifelse(DMTLab == "INFB1A"|DMTLab == "INFB1B"|DMTLab == "PINF1A",
#                           "Interferon-beta", DMTLab)) %>%
#   mutate (DMTLab = ifelse(DMTLab == "GA", "Glatiramer acetate", DMTLab)) %>%
#   mutate (DMTLab = ifelse(DMTCat == 5, "S1P-R modulator", DMTLab)) %>%
#   mutate (DMTLab = ifelse(DMTCat == 7, "Teriflunomide", DMTLab)) %>%
#   mutate (DMTLab = ifelse(DMTCat == 8, "Dimethyl fumarate", DMTLab)) %>%
#   mutate (DMTLab = ifelse(DMTCat == 0, "No DMT", DMTLab)) %>%
#   filter (DMTLab != "No DMT") %>%
#   select (c(3, 8, 27)) %>%
#   mutate (DURATION = DURATION/30) %>%
#   mutate (DMTEff1 = as.numeric(DMTEff1))

legend_ord <- levels(with(DMTDuration, reorder(DMTLab, DMTEff1)))

DMTDuration <- DMTDuration %>% 
  filter(MULDMT != 0) %>% 
  filter(DMTLab != "Mitoxantrone (3)") %>% 
  filter(DMTLab != "Daclizumab (111)")

p <- ggplot(data = DMTDuration, aes(x = factor(DMTLab, level = legend_ord) , 
                                    y = month, 
                                    group = factor(DMTLab), 
                                    color = factor(DMTLab, level = legend_ord),
                                    fill = factor(DMTLab, level = legend_ord))) +
  # geom_boxplot(show.legend = FALSE,
  #              outlier.shape = NA,
  #              fill = NA,
  #              width = 0.5,
  #              coef = 1.5,
  #              alpha = 0.25) +
  # geom_violin(scale = "width",
  #             color = NA,
  #             alpha = 0.3,
  #             show.legend = FALSE) +
  geom_jitter(position = position_jitter(width = 0.25, seed = 20000623),
              alpha = 0.05, show.legend = FALSE) +
  stat_summary(fun = median, show.legend = FALSE, geom = "crossbar",
               color = "black", width = 0.5, size = 0.3) +
  scale_color_manual(name = "",
                     values = c("#008347",
                                "#ff8900",
                                "#001887",
                                "#ecd121",
                                "#cf3637",
                                "#644a8c",
                                "#00a3d7",
                                "#bea991",
                                "#66903c")) +
  scale_fill_manual(name = "",
                    values = c("#008347",
                               "#ff8900",
                               "#001887",
                               "#ecd121",
                               "#cf3637",
                               "#644a8c",
                               "#00a3d7",
                               "#bea991",
                               "#66903c")) +
  scale_x_discrete(breaks = c("Dimethyl fumarate (13239)",
                              "Glatiramer acetate (19243)",
                              "Interferon-beta (19678)",
                              "S1P-R modulator (6766)",
                              "Teriflunomide (5648)",
                              "Alemtuzumab (63)",
                              "B cell depletion (359)",
                              "Cladribine (57)",
                              "Natalizumab (431)"),
                   labels = c("Dimethyl fumarate\n(13239)",
                              "Glatiramer acetate\n(19243)",
                              "Interferon-beta\n(19678)",
                              "S1P-R modulator\n(6766)",
                              "Teriflunomide\n(5648)",
                              "Alemtuzumab\n(63)",
                              "B cell depletion\n(359)",
                              "Cladribine\n(57)",
                              "Natalizumab\n(431)")) +
  scale_y_continuous(limits = c(0, 150))+
  theme_minimal()+
  labs(title="",
       x ="", 
       y ="Months")+
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 0.85))

print(p)
ggsave(p, file="DMTDuration4.svg", width=5.3, height=3.6, units = "in", dpi=300)









# ####
# Prevalence map (Supplementary Figure 1) ####
library(maps)
library(usmap)
library(ggplot2)
library(mapproj)
library(stringr)
library(tidyr)

usacoord <- map_data("state")


statepre <- read.csv("StatePre.csv") %>% 
  rename(per_insured = X..total.insured) %>%
  mutate(state = str_to_lower(state)) %>% 
  filter(state != "puerto rico") %>%
  arrange(state)

statepre$abbr <- statepop$abbr

MSpremap <- usacoord %>% 
  left_join(., statepre, by = c("region" = "state"))

map <- plot_usmap(data = statepre,
                  values = "per_insured",
                  labels = TRUE,
                  exclude = "DC")


map <- ggplot(MSpremap, aes(x = long, y = lat, map_id = region)) +
  geom_map(map = MSpremap) +
  # coord_map("ortho", orientation = c(39, -98, 0)) +
  geom_polygon(aes(group = group, fill = per_insured), color = "dark gray")
map
+
  # scale_fill_gradientn(colours = c("#ffffff","#dae8f8","#bed8ed", "#92c3de", "#5ca6d6","#1866ab","#144189"),
  #                      values = c(0.3, 0.5, 0.6, 0.7, 0.9, 1.5, 6.0)^0.16666,
  #                      space = "Lab",
  #                      guide = "legend"),
  #                      limits = c(0, 6))
  # scale_fill_gradient2(low = "#ffffff", mid = "#374a75", high = "#06163b",
  #                      trans = "log2",
  #                      limits = c(NA, 6))
  #                      # breaks = c(0, 0.5, 1, 2, 6),
  #                      # labels = c("0", "0.5", "1", "2", "6"))
  
usamap <- map +
  scale_fill_gradientn(colours = c("#ffffff","#dae8f8","#bed8ed", "#92c3de", "#5ca6d6","#1866ab","#144189"),
                       # values = c(0.01, 0.02, 0.03, 0.06, 0.1, 0.5, 0.9),
                       values = scales::rescale(c(0.01, 0.02, 0.06, 0.1, 0.3, 0.5, 2.0)),
                       space = "Lab",
                       guide = "legend",
                       # breaks = c(0.01, 0.02, 0.03, 0.06, 0.1, 0.5, 0.9),
                       breaks = c(0.01, 0.02, 0.06, 0.1, 0.3, 0.5, 2.0),
                       limits = c(0, 2.0),
                       name = NULL) +
  theme(text = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.position = "right",
        legend.title = element_blank())
usamap
ggsave(usamap, file="Premap.svg", width=5.3, height=3.6, units = "in", dpi=300)

c("#ffffff","#dae8f8","#bed8ed", "#92c3de", "#5ca6d6","#1866ab","#144189")

print(usamap)