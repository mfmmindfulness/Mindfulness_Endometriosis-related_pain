#prepare package to be used----

install.packages("WRS2")
tinytex::install_tinytex()

packages.paper= c("openxlsx""dplyr","tidyverse","knitr", "tidyr", 
            "fastDummies", "openxlsx", "rstatix","ggpubr","caret","flextable", "officer", "plotly",
            "gtable","egg", "gridExtra","grid","lavaan","robustbase","robustlmm","arsenal","GGally","mice")


invisible(lapply(packages.paper, library, character.only = TRUE))                          

#load data frame----
names(clinical.trial)

clinical.trial=read.xlsx("RCT.bMBI.endo.pain.xlsx")
View(clinical.trial)


# Data frame structure----

glimpse(clinical.trial)
headTail(clinical.trial)


#Social demographics and baseline outcome variables table----


sociodemographic=read.xlsx("sociodemographic.sub.xlsx")

names(sociodemographic)

#collapse education categories 



sociodemographic = sociodemographic %>% 
  mutate(education=case_when
         (education %in% c("incomplete elementary school","elementary school","incomplete high school") ~ "< high school",
           education %in% c("high school")~"high school", education %in% c("incomplete higher education","higher educationl","graduate")
           ~ "university")) %>% 
  select(id,group,age,race,marital.status,education,BMI,physical.exercise,sleep.hours.per.night,current.endometriosis.medication,n.surgeries,time.chronic.pain, 
         analgesic,anxiety,depression)

#table sociodemographic
#change default statistic in tableby
mycontrols= tableby.control( test=FALSE,
                             numeric.stats=c("mean","sd", "median", "q1q3"),
                             cat.stats=c("countpct"),
                             stats.labels=list(mean="Mean",sd="SD", median="Median", q1q3="Q1,Q3"))

mycontrols2= tableby.control(test=TRUE, total=FALSE,
                             numeric.test="wt", cat.test="chisq",
                             numeric.stats=c("mean","sd", "median", "q1q3"),
                             cat.stats=c("countpct"),
                             stats.labels=list(mean="Mean",sd="SD", median="Median", q1q3="Q1,Q3"))


#table1

tab1=tableby(group~age+race+marital.status+education+BMI+physical.exercise+sleep.hours.per.night+current.endometriosis.medication+n.surgeries+
               time.chronic.pain+analgesic+anxiety+depression , data=sociodemographic, control=mycontrols2)

mylabels = list(age="age (y)", BMI="BMI (kg/m2)")


tab1=summary(tab1,text=TRUE, labelTranslations = mylabels)
tab1=as.data.frame(tab1)

tab1= tab1 %>% 
  rename(variables="") %>% 
  flextable(col_keys = c("variables","control (N=32)","intervention (N=31)","Total (N=63)","p value")) %>% 
  colformat_num(digits=2,j=c("variables","control (N=32)","intervention (N=31)","Total (N=63)","p value")) %>% 
  autofit()



#test difference in baseline per outcome variable

clinical.trial %>%
  filter(time=="t1") %>% 
  select(id,group, sf.36.physical.sum,sf.36.mental.sum,sf_36_physical.functioning,
         sf_36_limitations.physical.functioning,
         sf_36_pain, sf_36_general.health, sf_36_vitality, sf_36_social.function, sf_36_emotional.role,
         sf_36_mental.health) %>%  
  melt(id.vars=c("id","group")) %>% 
  group_by(variable) %>% 
  wilcox_test(value~group, detailed = TRUE) %>%
  adjust_pvalue() %>% 
  add_significance("p") %>% 
  mutate(estimate=round(estimate,3),p=round(p,3),conf.low=round(conf.low,3),
         conf.high=round(conf.high,3),p.adj=round(p.adj,3)) %>% 
  select(variable,estimate,statistic,p,conf.low,conf.high,p.adj) %>% 
  flextable() %>% 
  autofit()


clinical.trial %>%
  filter(time=="t1") %>% 
  select(id,group, pelvic.pain,pain.unpleasantness,dysuria,dyspareunia,dyschezia,dysmenorrhea,PSS_10_total) %>%  
  melt(id.vars=c("id","group")) %>% 
  group_by(variable) %>% 
  wilcox_test(value~group, detailed = TRUE) %>%
  adjust_pvalue() %>% 
  add_significance("p") %>% 
  mutate(estimate=round(estimate,3),p=round(p,3),conf.low=round(conf.low,3),
         conf.high=round(conf.high,3),p.adj=round(p.adj,3)) %>% 
  select(variable,estimate,statistic,p,conf.low,conf.high,p.adj) %>% 
  flextable() %>% 
  autofit()

clinical.trial %>%
  filter(time=="t1") %>% 
  select(id,group, FFMQ_total) %>%  
  melt(id.vars=c("id","group")) %>% 
  group_by(variable) %>% 
  t_test(value~group, detailed = TRUE) %>%
  adjust_pvalue() %>% 
  add_significance("p") %>% 
  mutate(estimate=round(estimate,3),p=round(p,3),conf.low=round(conf.low,3),
         conf.high=round(conf.high,3),p.adj=round(p.adj,3)) %>% 
  select(variable,estimate,statistic,p,conf.low,conf.high,p.adj) %>% 
  flextable() %>% 
  autofit()



#outcome variables between time and group
outcome.table=clinical.trial %>%
  select(id,group,time, pelvic.pain,pain.unpleasantness,dysuria,dyspareunia,dyschezia,dysmenorrhea,PSS_10_total,
         sf.36.physical.sum,sf.36.mental.sum,sf_36_physical.functioning,sf_36_limitations.physical.functioning,
         sf_36_pain, sf_36_general.health, sf_36_vitality, sf_36_social.function, sf_36_emotional.role,
         sf_36_mental.health, FFMQ_total) %>% 
  pivot_wider(names_from = time,values_from=c(pelvic.pain,pain.unpleasantness,dysuria,dyspareunia,dyschezia,dysmenorrhea,PSS_10_total,
                                              sf.36.physical.sum,sf.36.mental.sum,sf_36_physical.functioning,sf_36_limitations.physical.functioning,
                                              sf_36_pain, sf_36_general.health, sf_36_vitality, sf_36_social.function, sf_36_emotional.role,
                                              sf_36_mental.health, FFMQ_total))

#table

tab2=tableby(group~pelvic.pain_t1+pelvic.pain_t2+pelvic.pain_t3+pain.unpleasantness_t1+pain.unpleasantness_t2+
               pain.unpleasantness_t3+dysuria_t1+dysuria_t1+dysuria_t2+dysuria_t3+dyspareunia_t1+dyspareunia_t2+
               dyspareunia_t3+dyschezia_t1+dyschezia_t2+dyschezia_t3+dysmenorrhea_t1+dysmenorrhea_t2+dysmenorrhea_t3+
               PSS_10_total_t1+PSS_10_total_t2+PSS_10_total_t3+sf.36.physical.sum_t1+sf.36.physical.sum_t2+sf.36.physical.sum_t3+
               sf.36.mental.sum_t1+sf.36.mental.sum_t2+sf.36.mental.sum_t3+sf_36_physical.functioning_t1+sf_36_physical.functioning_t2+
               sf_36_physical.functioning_t1+sf_36_physical.functioning_t2+sf_36_physical.functioning_t3+
               sf_36_limitations.physical.functioning_t1+sf_36_limitations.physical.functioning_t2+sf_36_limitations.physical.functioning_t3+
               sf_36_pain_t1+sf_36_pain_t2+sf_36_pain_t3+sf_36_general.health_t1+sf_36_general.health_t2+sf_36_general.health_t3+
               sf_36_general.health_t1+sf_36_general.health_t2+sf_36_general.health_t3+sf_36_vitality_t1+sf_36_vitality_t2+sf_36_vitality_t3+
               sf_36_social.function_t1+sf_36_social.function_t2+sf_36_social.function_t3+sf_36_emotional.role_t1+sf_36_emotional.role_t2+
               sf_36_emotional.role_t3+sf_36_mental.health_t1+sf_36_mental.health_t2+sf_36_mental.health_t3+FFMQ_total_t1+FFMQ_total_t2+
               FFMQ_total_t3, data=outcome.table, control=mycontrols)

tab2=summary(tab2,text=TRUE)

tab2=as.data.frame(tab2)


tab2= tab2 %>% 
  rename(variables="") %>% 
  flextable(col_keys = c("variables","control (N=32)","intervention (N=31)","Total (N=63)","p value")) %>% 
  colformat_num(digits=2,j=c("variables","control (N=32)","intervention (N=31)","Total (N=63)","p value")) %>% 
  autofit()

#Meditation diary, mean of meditation time by week and total----

meditation.diary=read.xlsx("meditation.diary.all.xlsx")
names(meditation.diary)

hist(meditation.diary$mean_total)
meditation.diary %>% 
  pivot_longer(-id) %>% 
  group_by(name) %>% 
  filter(!is.na(value)) %>% 
  summarise(mean=mean(value),sd=sd(value))

# Missing by Time: T1, T2, T3 per variable----


missing.values.t1= clinical.trial %>%
  filter(time=="t1") %>% 
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 

missing.values.t2= clinical.trial %>%
  filter(time=="t2") %>% 
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 

missing.values.t3= clinical.trial %>%
  filter(time=="t1") %>% 
  gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing)) 

missing.values.t1 %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
  labs(x='variable', y="number of missing t1", title='Number of missing values') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

missing.values.t2  %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
  labs(x='variable', y="number of missing t2", title='Number of missing values') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

missing.values.t3 %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
  labs(x='variable', y="number of missing t3", title='Number of missing values') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Percentage of missing per variable, total dataframe----
View(levels)
missing.values.percentage= clinical.trial %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)


levels = (missing.values.percentage  %>% filter(isna == T) %>% arrange(desc(pct)))$key

percentage.plot = missing.values.percentage %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")

# Total missing by id----

row.plot = clinical.trial %>%
  mutate(id = row_number()) %>%
  gather(-id, key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  ggplot(aes(key, id, fill = isna)) +
  geom_raster(alpha=0.8) +
  scale_fill_manual(name = "",
                    values = c('steelblue', 'tomato3'),
                    labels = c("Present", "Missing")) +
  scale_x_discrete(limits = levels) +
  labs(x = "Variable",
       y = "Row Number", title = "Missing values in rows") +
  scale_y_continuous(breaks= seq(0,174,by=20)) +
  coord_flip()


# Sample size by time: t1, t2, t3----

sample.by.time= clinical.trial %>% 
  select (id, time, group) %>% 
  group_by(time, group) %>% 
  summarise(id = n()) %>% 
  rename(participants=id) %>% 
  flextable()

# Participants dropout by time: t1, t2, t3----


dropout= clinical.trial %>% 
  select (id, time, group) %>% 
  group_by(time, group) %>% 
  summarise(id = n()) %>% 
  pivot_wider(names_from = time,values_from = id) %>% 
  mutate(dropout.t1.t2=t1-t2, dropout.t1.t3=t1-t3) %>% 
  group_by(group) %>% 
  mutate(drop.out.t1.t2.percentage=dropout.t1.t2/t1*100,
         drop.out.t1.t3.percentage=dropout.t1.t3/t1*100) %>% 
  select(dropout.t1.t2, dropout.t1.t3,drop.out.t1.t2.percentage ,
         drop.out.t1.t3.percentage) %>% 
  mutate(drop.out.t1.t2.percentage=round(drop.out.t1.t2.percentage,2),
         drop.out.t1.t3.percentage=round(drop.out.t1.t3.percentage,2)) %>% 
  flextable() %>% 
  set_header_labels(drop.out.t1.t2="n t1-t2", drop.out.t1.t3 = "n t1-t3",
                    drop.out.t1.t2.percentage = "% t1-t2",
                    drop.out.t1.t3.percentage = "% t1-t3") %>% 
  set_caption(caption= "Dropout")



# Endometriosis-related pain and perceived stress Distribution between group---- 

# raw scores

#t1

clinical.trial %>% 
  filter(time=="t1") %>% 
  select(group,pelvic.pain,dysuria,dyspareunia,dyschezia,dysmenorrhea,pain.unpleasantness,PSS_10_total) %>%
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())

#t2

clinical.trial %>% 
  filter(time=="t2") %>% 
  select(group,pelvic.pain,dysuria,dyspareunia,dyschezia,dysmenorrhea,pain.unpleasantness,PSS_10_total) %>%
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())

#t3

clinical.trial %>% 
  filter(time=="t3") %>% 
  select(group,pelvic.pain,dysuria,dyspareunia,dyschezia,dysmenorrhea,pain.unpleasantness,PSS_10_total) %>%
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())

# Endometriosis-related pain and perceived stress distribution between group on gain scores----

#compute changes in variables of interest from t1 to t2 and t1 to t3

pain.PSS.gain=clinical.trial %>% 
  select(id, group, time,pelvic.pain,dysuria,dyspareunia,dyschezia,dysmenorrhea,pain.unpleasantness,
         PSS_10_total) %>% 
  pivot_wider(names_from = time,values_from = c(pelvic.pain,dysuria,dyspareunia,
                                                dyschezia,dysmenorrhea,pain.unpleasantness,PSS_10_total)) %>% 
  mutate (pelvic.pain.change.t1_t2 =  pelvic.pain_t2 - pelvic.pain_t1,
          dysuria.change.t1_t2 = dysuria_t2 - dysuria_t1,
          dyspareunia.change.t1_t2 =dyspareunia_t2- dyspareunia_t1,
          dyschezia.change.t1_t2 =dyschezia_t2- dyschezia_t1,
          dysmenorrhea.change.t1_t2 =dysmenorrhea_t2 - dysmenorrhea_t1,
          pain.unpleasantness.change.t1_t2 =pain.unpleasantness_t2 - pain.unpleasantness_t1,
          PSS.change.t1_t2 =PSS_10_total_t2 - PSS_10_total_t1,
          pelvic.pain.change.t1_t3 = pelvic.pain_t3 - pelvic.pain_t1,
          dysuria.change.t1_t3 =dysuria_t3 - dysuria_t1,dyspareunia.change.t1_t3 = dyspareunia_t3 - dyspareunia_t1,
          dyschezia.change.t1_t3 = dyschezia_t3 - dyschezia_t1,
          dysmenorrhea.change.t1_t3 = dysmenorrhea_t3 - dysmenorrhea_t1,
          pain.unpleasantness.change.t1_t3 =pain.unpleasantness_t3 - pain.unpleasantness_t1,
          PSS.change.t1_t3 = PSS_10_total_t3 - PSS_10_total_t1)


#t1-t2

pain.PSS.gain %>%  
  select(group,pelvic.pain.change.t1_t2,dysuria.change.t1_t2,dyspareunia.change.t1_t2,dyschezia.change.t1_t2,
         dysmenorrhea.change.t1_t2,dysmenorrhea.change.t1_t2,pain.unpleasantness.change.t1_t2,
         PSS.change.t1_t2) %>%
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())

#t1-t3

pain.PSS.gain %>%  
  select(group,pelvic.pain.change.t1_t3,
         dyspareunia.change.t1_t3,dyschezia.change.t1_t3,dysmenorrhea.change.t1_t3,
         pain.unpleasantness.change.t1_t3,PSS.change.t1_t3 ) %>%
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())

# Multidimensional quality of life (SF_36) distribution---- 

#distribution between group t1

# Summary sf-36 physical and mental health

dist.sum.sf_36.t1=clinical.trial %>% 
  filter(time=="t1") %>% 
  select(group,sf.36.physical.sum,sf.36.mental.sum) %>% 
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())

#subscales

dist.sub.sf_36.t1=clinical.trial %>%  
  filter(time=="t1") %>% 
  select(group,sf_36_physical.functioning,sf_36_limitations.physical.functioning,
         sf_36_pain,sf_36_general.health,sf_36_vitality, sf_36_social.function,
         sf_36_emotional.role, sf_36_mental.health) %>%
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())# sf_36_emotional.role, sf_36_limitation.physical.functioning
# with extremely low variance in control group 


#distribution between group t2

# Summary sf-36 physical and mental health

dist.sum.sf_36.t2=clinical.trial %>% 
  filter(time=="t2") %>% 
  select(group,sf.36.physical.sum,sf.36.mental.sum) %>% 
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())

#subscales
dist.sub.sf_36.t2=clinical.trial %>%  
  filter(time=="t2") %>% 
  select(group,sf_36_physical.functioning,sf_36_limitations.physical.functioning,
         sf_36_pain,sf_36_general.health,sf_36_vitality, sf_36_social.function,
         sf_36_emotional.role, sf_36_mental.health) %>%
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())

#distribution between group t3

# Summary sf-36 physical and mental health

dist.sum.sf_36.t3=clinical.trial %>% 
  filter(time=="t3") %>% 
  select(group,sf.36.physical.sum,sf.36.mental.sum) %>% 
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())

#subscales

dist.sub.sf_36.t3=clinical.trial %>%  
  filter(time=="t3") %>% 
  select(group,sf_36_physical.functioning,sf_36_limitations.physical.functioning,
         sf_36_pain,sf_36_general.health,sf_36_vitality, sf_36_social.function,
         sf_36_emotional.role, sf_36_mental.health) %>%
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())


#compute changes in variables of interest from t1 to t2 and t1 to t3----

sf.36.gain=clinical.trial %>% 
  select(id, group, time,sf.36.physical.sum, sf.36.mental.sum, sf_36_physical.functioning,sf_36_limitations.physical.functioning,
         sf_36_pain,sf_36_general.health,sf_36_vitality, sf_36_social.function,
         sf_36_emotional.role, sf_36_mental.health) %>% 
  pivot_wider(names_from = time,values_from = c( sf.36.physical.sum, sf.36.mental.sum,sf_36_physical.functioning,
                                                 sf_36_limitations.physical.functioning,
                                                 sf_36_pain,sf_36_general.health,sf_36_vitality, sf_36_social.function,
                                                 sf_36_emotional.role, sf_36_mental.health)) %>% 
  mutate (sf_36.physical.t1.t2=sf.36.physical.sum_t2-sf.36.physical.sum_t1,
          sf.36.mental.sum.t1.t2=sf.36.mental.sum_t2-sf.36.mental.sum_t1,
          sf_36_physical.functioning.t1_t2 =  sf_36_physical.functioning_t2 - sf_36_physical.functioning_t1,
          sf_36_limitations.physical.t1_t2 = sf_36_limitations.physical.functioning_t2 - sf_36_limitations.physical.functioning_t1,
          sf_36_pain.t1_t2 =sf_36_pain_t2- sf_36_pain_t1,
          sf_36_general.health.t1_t2 =sf_36_general.health_t2- sf_36_general.health_t1,
          sf_36_vitality.t1_t2 =sf_36_vitality_t2 - sf_36_vitality_t1,
          sf_36_social.function.t1_t2 =sf_36_social.function_t2 - sf_36_social.function_t1,
          sf_36_emotional.role.t1_t2 =sf_36_emotional.role_t2 - sf_36_emotional.role_t1,
          sf_36_mental.health.t1_t2 =sf_36_mental.health_t2 - sf_36_mental.health_t1,
          sf_36.physical.t1.t3=sf.36.physical.sum_t3-sf.36.physical.sum_t1,
          sf.36.mental.sum.t1.t3=sf.36.mental.sum_t3-sf.36.mental.sum_t1,
          sf_36_physical.functioning.t1_t3 =  sf_36_physical.functioning_t3 - sf_36_physical.functioning_t1,
          sf_36_limitations.physical.t1_t3 = sf_36_limitations.physical.functioning_t3 - sf_36_limitations.physical.functioning_t1,
          sf_36_pain.t1_t3 =sf_36_pain_t3- sf_36_pain_t1,
          sf_36_general.health.t1_t3 =sf_36_general.health_t3- sf_36_general.health_t1,
          sf_36_vitality.t1_t3 =sf_36_vitality_t3 - sf_36_vitality_t1,
          sf_36_social.function.t1_t3 =sf_36_social.function_t3 - sf_36_social.function_t1,
          sf_36_emotional.role.t1_t3 =sf_36_emotional.role_t3 - sf_36_emotional.role_t1,
          sf_36_mental.health.t1_t3 =sf_36_mental.health_t3 - sf_36_mental.health_t1)


# Mindfulness (FFMQ) distribution---- 

#distribution between group and time
dist.FFMQ=clinical.trial %>% 
  select(group,time, FFMQ_total) %>%
  pivot_longer(-group & -time) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ time, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())



#compute changes in variables of interest from t1 to t2 and t1 to t3

FFMQ.gain=clinical.trial %>% 
  select(id, group, time, FFMQ_total) %>% 
  pivot_wider(names_from = time,values_from = c( FFMQ_total)) %>% 
  mutate (FFMQ_total.t1_t2 =t2 - t1,
          FFMQ_total.t1_t3 =t3 - t1)


#distribution between group on gain score t1-t2

gain.FFMQ.t1.t2=FFMQ.gain %>%  
  select(group,FFMQ_total.t1_t2, FFMQ_total.t1_t3 ) %>%
  pivot_longer(-group) %>% 
  ggplot(aes(x = group, y = value, color = group)) + facet_wrap(~ name, scales = "free", nrow = 4) +
  coord_flip ()+
  geom_boxplot() +
  geom_jitter(shape = 10, position = position_jitter(0.1)) +
  stat_summary(fun = median, geom ="pointrange",color = "black") +
  ylab("Value") +
  theme( axis.text.y = element_blank())


# Dropout analysis----

# Dropout analysis t2
View(drop.out.analysis)

drop.out.analysis=clinical.trial %>% 
  filter(time=="t1"| time== "t2") %>% 
  select(id, time,group,pelvic.pain,pain.unpleasantness,dysuria,dyspareunia,dyschezia,dysmenorrhea,
         PSS_10_total,sf_36_physical.functioning,sf_36_limitations.physical.functioning,sf_36_pain,
         sf_36_general.health,sf_36_vitality,sf_36_social.function,sf_36_emotional.role,sf_36_mental.health) %>% 
  pivot_wider(names_from=time, values_from = c(pelvic.pain,pain.unpleasantness,dysuria,dyspareunia,dyschezia,dysmenorrhea,
                                               PSS_10_total,sf_36_physical.functioning,sf_36_limitations.physical.functioning,sf_36_pain,
                                               sf_36_general.health,sf_36_vitality,sf_36_social.function,sf_36_emotional.role,sf_36_mental.health)) %>% 
  mutate(drop.out = is.na(pelvic.pain_t2)) %>% 
  mutate(drop.out = factor(drop.out, levels = c("FALSE", "TRUE"), 
                           labels = c("no", "yes"))) 

# Dropout analysis t3

View(drop.out.analysis.t3)
drop.out.analysis.t3=clinical.trial %>% 
  filter(time=="t1"| time== "t3") %>% 
  select(id, time,group,pelvic.pain,pain.unpleasantness,dysuria,dyspareunia,dyschezia,dysmenorrhea,
         PSS_10_total,sf_36_physical.functioning,sf_36_limitations.physical.functioning,sf_36_pain,
         sf_36_general.health,sf_36_vitality,sf_36_social.function,sf_36_emotional.role,sf_36_mental.health) %>% 
  pivot_wider(names_from=time, values_from = c(pelvic.pain,pain.unpleasantness,dysuria,dyspareunia,dyschezia,dysmenorrhea,
                                               PSS_10_total,sf_36_physical.functioning,sf_36_limitations.physical.functioning,sf_36_pain,
                                               sf_36_general.health,sf_36_vitality,sf_36_social.function,sf_36_emotional.role,sf_36_mental.health)) %>% 
  mutate(drop.out = is.na(pelvic.pain_t3)) %>% 
  mutate(drop.out = factor(drop.out, levels = c("FALSE", "TRUE"), 
                           labels = c("no", "yes"))) 


#Dropout analyses by variable using wilcoxon_t2 


drop.out.test=drop.out.analysis %>% 
  select(id,group,drop.out,pelvic.pain_t1,dysuria_t1,dyspareunia_t1,
         dyschezia_t1,dysmenorrhea_t1,pain.unpleasantness_t1) %>% 
  melt(id.vars=c("id", "drop.out","group")) %>% 
  group_by(variable) %>% 
  wilcox_test(value~drop.out, detailed = TRUE) %>%
  adjust_pvalue() %>% 
  add_significance("p") %>% 
  mutate(estimate=round(estimate,3),p=round(p,3),conf.low=round(conf.low,3),
         conf.high=round(conf.high,3),p.adj=round(p.adj,3)) %>% 
  select(variable,estimate,statistic,p,conf.low,conf.high,p.adj) %>% 
  flextable() %>% 
  autofit()

#Dropout analyses by condition using wilcoxon_t2 
drop.out.test.condition=drop.out.analysis %>% 
  select(id,group,drop.out,pelvic.pain_t1,dysuria_t1,dyspareunia_t1,
         dyschezia_t1,dysmenorrhea_t1,pain.unpleasantness_t1) %>% 
  melt(id.vars=c("id", "drop.out","group")) %>% 
  group_by(variable,group) %>% 
  wilcox_test(value~drop.out, detailed = TRUE) %>%
  adjust_pvalue() %>% 
  add_significance("p") %>% 
  mutate(estimate=round(estimate,3),p=round(p,3),conf.low=round(conf.low,3),
         conf.high=round(conf.high,3),p.adj=round(p.adj,3)) %>% 
  select(variable,estimate,statistic,p,conf.low,conf.high,p.adj) %>% 
  flextable() %>% 
  autofit()


#logistic regression enter outcomes and condition as predictors of dropout----

#t2----
pelvic.pain.drop.out.log=glm(drop.out~pelvic.pain_t1+group, data = drop.out.analysis, family = binomial)
summary(pelvic.pain.drop.out.log)
pelvic.pain.drop.out.log=tbl_regression(pelvic.pain.drop.out.log, exponentiate = TRUE)


unpleasantness.drop.out.log=glm(drop.out~pain.unpleasantness_t1+group, data = drop.out.analysis, family = binomial)
summary(unpleasantness.drop.out.log)
unpleasantness.drop.out.log=tbl_regression(unpleasantness.drop.out.log, exponentiate = TRUE)

dysuria.drop.out.log=glm(drop.out~dysuria_t1+group, data = drop.out.analysis, family = binomial)
summary(dysuria.drop.out.log)
dysuria.drop.out.log=tbl_regression(dysuria.drop.out.log, exponentiate = TRUE)


dyschezia.drop.out.log=glm(drop.out~dyschezia_t1+group, data = drop.out.analysis, family = binomial)
summary(dyschezia.drop.out.log)
dyschezia.drop.out.log=tbl_regression(dyschezia.drop.out.log, exponentiate = TRUE)

dyspareunia.drop.out.log=glm(drop.out~dyspareunia_t1+group, data = drop.out.analysis, family = binomial)
summary(dyspareunia.drop.out.log)
dyspareunia.drop.out.log=tbl_regression(dyspareunia.drop.out.log, exponentiate = TRUE)

dysmenorrhea.drop.out.log=glm(drop.out~dysmenorrhea_t1+group, data = drop.out.analysis, family = binomial)
summary(dysmenorrhea.drop.out.log)
dysmenorrhea.drop.out.log=tbl_regression(dysmenorrhea.drop.out.log, exponentiate = TRUE)

ps.drop.out.log=glm(drop.out~PSS_10_total_t1+group, data = drop.out.analysis, family = binomial)
summary(ps.drop.out.log)
ps.drop.out.log=tbl_regression(ps.drop.out.log, exponentiate = TRUE)

physical.drop.out.log=glm(drop.out~sf_36_physical.functioning_t1+group, data = drop.out.analysis, family = binomial)
summary(physical.drop.out.log)
physical.drop.out.log=tbl_regression(physical.drop.out.log, exponentiate = TRUE)


lim.physical.drop.out.log=glm(drop.out~sf_36_limitations.physical.functioning_t1+group, data = drop.out.analysis, family = binomial)
summary(lim.physical.drop.out.log)
lim.physical.drop.out.log=tbl_regression(lim.physical.drop.out.log, exponentiate = TRUE)


pain.drop.out.log=glm(drop.out~sf_36_pain_t1+group, data = drop.out.analysis, family = binomial)
summary(pain.drop.out.log)
pain.drop.out.log=tbl_regression(pain.drop.out.log, exponentiate = TRUE)


health.drop.out.log=glm(drop.out~sf_36_general.health_t1+group, data = drop.out.analysis, family = binomial)
summary(health.drop.out.log)
health.drop.out.log=tbl_regression(health.drop.out.log, exponentiate = TRUE)


vitality.drop.out.log=glm(drop.out~sf_36_vitality_t1+group, data = drop.out.analysis, family = binomial)
summary(vitality.drop.out.log)
vitality.drop.out.log=tbl_regression(vitality.drop.out.log, exponentiate = TRUE)


social.drop.out.log=glm(drop.out~sf_36_social.function_t1+group, data = drop.out.analysis, family = binomial)
summary(social.drop.out.log)
social.drop.out.log=tbl_regression(social.drop.out.log, exponentiate = TRUE)


emotional.drop.out.log=glm(drop.out~sf_36_emotional.role_t1+group, data = drop.out.analysis, family = binomial)
summary(emotional.drop.out.log)
emotional.drop.out.log=tbl_regression(emotional.drop.out.log, exponentiate = TRUE)


mental.drop.out.log=glm(drop.out~sf_36_mental.health_t1+group, data = drop.out.analysis, family = binomial)
summary(mental.drop.out.log)
mental.drop.out.log=tbl_regression(mental.drop.out.log, exponentiate = TRUE)


# t3----

pelvic.pain.drop.out.log3=glm(drop.out~pelvic.pain_t1+group, data = drop.out.analysis.t3, family = binomial)#pelvic pain predicted drop out
summary(pelvic.pain.drop.out.log3)
pelvic.pain.drop.out.log3=tbl_regression(pelvic.pain.drop.out.log3, exponentiate = TRUE)


unpleasantness.drop.out.log3=glm(drop.out~pain.unpleasantness_t1+group, data = drop.out.analysis.t3, family = binomial)
summary(unpleasantness.drop.out.log3)
unpleasantness.drop.out.log3=tbl_regression(unpleasantness.drop.out.log3, exponentiate = TRUE)


dysuria.drop.out.log3=glm(drop.out~dysuria_t1+group, data = drop.out.analysis.t3, family = binomial)
summary(dysuria.drop.out.log3)
dysuria.drop.out.log3=tbl_regression(dysuria.drop.out.log3, exponentiate = TRUE)



dyschezia.drop.out.log3=glm(drop.out~dyschezia_t1+group, data = drop.out.analysis.t3, family = binomial)
summary(dyschezia.drop.out.log3)
dyschezia.drop.out.log3=tbl_regression(dyschezia.drop.out.log3, exponentiate = TRUE)


dyspareunia.drop.out.log3=glm(drop.out~dyspareunia_t1+group, data = drop.out.analysis.t3, family = binomial)
summary(dyspareunia.drop.out.log3)
dyspareunia.drop.out.log3=tbl_regression(dyspareunia.drop.out.log3, exponentiate = TRUE)

dysmenorrhea.drop.out.log3=glm(drop.out~dysmenorrhea_t1+group, data = drop.out.analysis.t3, family = binomial)#dysmenorrhea predicted drop out
summary(dysmenorrhea.drop.out.log3)
dysmenorrhea.drop.out.log3=tbl_regression(dysmenorrhea.drop.out.log3, exponentiate = TRUE)


ps.drop.out.log3=glm(drop.out~PSS_10_total_t1+group, data = drop.out.analysis.t3, family = binomial)
summary(ps.drop.out.log3)
ps.drop.out.log3=tbl_regression(ps.drop.out.log3, exponentiate = TRUE)


physical.drop.out.log3=glm(drop.out~sf_36_physical.functioning_t1+group, data = drop.out.analysis.t3, family = binomial)
summary(physical.drop.out.log3)
physical.drop.out.log3=tbl_regression(physical.drop.out.log3, exponentiate = TRUE)


lim.physical.drop.out.log3=glm(drop.out~sf_36_limitations.physical.functioning_t1+group, data = drop.out.analysis.t3, family = binomial)
summary(lim.physical.drop.out.log3)
lim.physical.drop.out.log3=tbl_regression(lim.physical.drop.out.log3, exponentiate = TRUE)



pain.drop.out.log3=glm(drop.out~sf_36_pain_t1+group, data = drop.out.analysis.t3, family = binomial)
summary(pain.drop.out.log3)
pain.drop.out.log3=tbl_regression(pain.drop.out.log3, exponentiate = TRUE)



health.drop.out.log3=glm(drop.out~sf_36_general.health_t1+group, data = drop.out.analysis.t3, family = binomial)
summary(health.drop.out.log3)
health.drop.out.log3=tbl_regression(health.drop.out.log3, exponentiate = TRUE)


vitality.drop.out.log3=glm(drop.out~sf_36_vitality_t1+group, data = drop.out.analysis.t3, family = binomial)
summary(vitality.drop.out.log3)
vitality.drop.out.log3=tbl_regression(vitality.drop.out.log3, exponentiate = TRUE)


social.drop.out.log3=glm(drop.out~sf_36_social.function_t1+group, data = drop.out.analysis.t3, family = binomial)
summary(social.drop.out.log3)
social.drop.out.log3=tbl_regression(social.drop.out.log3, exponentiate = TRUE)


emotional.drop.out.log3=glm(drop.out~sf_36_emotional.role_t1+group, data = drop.out.analysis.t3, family = binomial)
summary(emotional.drop.out.log3)
emotional.drop.out.log3=tbl_regression(emotional.drop.out.log3, exponentiate = TRUE)


mental.drop.out.log3=glm(drop.out~sf_36_mental.health_t1+group, data = drop.out.analysis.t3, family = binomial)
summary(mental.drop.out.log3)
mental.drop.out.log3=tbl_regression(mental.drop.out.log3, exponentiate = TRUE)


#table----
library(gtsummary)

tbl_p.pain =
  tbl_merge(tbls = list(pelvic.pain.drop.out.log, pelvic.pain.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 


tbl_unpleasantness =
  tbl_merge(tbls = list(unpleasantness.drop.out.log, unpleasantness.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 

tbl_dysuria =
  tbl_merge(tbls = list(dysuria.drop.out.log, dysuria.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 

tbl_dyschezia =
  tbl_merge(tbls = list(dyschezia.drop.out.log, dyschezia.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 

tbl_dyspareunia =
  tbl_merge(tbls = list(dyspareunia.drop.out.log, dyspareunia.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 

tbl_dysmenorrhea =
  tbl_merge(tbls = list(dysmenorrhea.drop.out.log, dysmenorrhea.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 


tbl_ps =
  tbl_merge(tbls = list(ps.drop.out.log, ps.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 

tbl_physical =
  tbl_merge(tbls = list(physical.drop.out.log, physical.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 

tbl_lim.physical =
  tbl_merge(tbls = list(lim.physical.drop.out.log, lim.physical.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 

tbl_pain =
  tbl_merge(tbls = list(pain.drop.out.log, pain.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 

tbl_health =
  tbl_merge(tbls = list(health.drop.out.log, health.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 

tbl_vitality =
  tbl_merge(tbls = list(vitality.drop.out.log, vitality.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 

tbl_social =
  tbl_merge(tbls = list(social.drop.out.log, social.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 


tbl_emotional =
  tbl_merge(tbls = list(emotional.drop.out.log, emotional.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 


tbl_mental =
  tbl_merge(tbls = list(mental.drop.out.log, mental.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 

#Join tables----

tbl_stack(list(tbl_p.pain,tbl_unpleasantness,tbl_dysuria,tbl_dyschezia,tbl_dyspareunia,tbl_dysmenorrhea,tbl_ps,tbl_physical,
               tbl_lim.physical,tbl_pain,tbl_health,tbl_vitality,tbl_social,
               tbl_emotional,tbl_mental))

#Sociodemographic dropout----

#t2
drop.out.analysis.socio=drop.out.analysis %>% 
  select(drop.out)

sociodemographic.drop.out=sociodemographic %>% 
  bind_cols(drop.out.analysis.socio)

#t3
drop.out.analysis.t3.socio=drop.out.analysis.t3 %>% 
  select(drop.out)

sociodemographic.drop.out3=sociodemographic %>% 
  bind_cols(drop.out.analysis.t3.socio)

#logistic regression

#t2
age.drop.out.log=glm(drop.out~age+group, data = sociodemographic.drop.out, family = binomial)
summary(age.drop.out.log)
age.drop.out.log=tbl_regression(age.drop.out.log, exponentiate = TRUE)


education.drop.out.log=glm(drop.out~education+group, data = sociodemographic.drop.out, family = binomial)
summary(education.drop.out.log)
education.drop.out.log=tbl_regression(education.drop.out.log, exponentiate = TRUE)

marital.status.drop.out.log=glm(drop.out~marital.status+group, data = sociodemographic.drop.out, family = binomial)
summary(marital.status.drop.out.log)
marital.status.drop.out.log=tbl_regression(marital.status.drop.out.log, exponentiate = TRUE)

BMI.drop.out.log=glm(drop.out~BMI+group, data = sociodemographic.drop.out, family = binomial)
summary(BMI.drop.out.log)
BMI.drop.out.log=tbl_regression(BMI.drop.out.log, exponentiate = TRUE)

physical.exercise.drop.out.log=glm(drop.out~physical.exercise+group, data = sociodemographic.drop.out, family = binomial)
summary(physical.exercise.drop.out.log)
physical.exercise.drop.out.log=tbl_regression(physical.exercise.drop.out.log, exponentiate = TRUE)

current.endometriosis.medication.drop.out.log=glm(drop.out~current.endometriosis.medication+group, data = sociodemographic.drop.out, family = binomial)
summary(current.endometriosis.medication.drop.out.log)
current.endometriosis.medication.drop.out.log=tbl_regression(current.endometriosis.medication.drop.out.log, exponentiate = TRUE)

sleep.hours.per.night.drop.out.log=glm(drop.out~sleep.hours.per.night+group, data = sociodemographic.drop.out, family = binomial)
summary(sleep.hours.per.night.drop.out.log)
sleep.hours.per.night.drop.out.log=tbl_regression(sleep.hours.per.night.drop.out.log, exponentiate = TRUE)

time.chronic.pain.drop.out.log=glm(drop.out~time.chronic.pain+group, data = sociodemographic.drop.out, family = binomial)
summary(time.chronic.pain.drop.out.log)
time.chronic.pain.drop.out.log=tbl_regression(time.chronic.pain.drop.out.log, exponentiate = TRUE)



anxiety.drop.out.log=glm(drop.out~anxiety+group, data = sociodemographic.drop.out, family = binomial)
summary(anxiety.drop.out.log)
anxiety.drop.out.log=tbl_regression(anxiety.drop.out.log, exponentiate = TRUE)


depression.drop.out.log=glm(drop.out~depression+group, data = sociodemographic.drop.out, family = binomial)
summary(depression.drop.out.log)
depression.drop.out.log=tbl_regression(depression.drop.out.log, exponentiate = TRUE)

#t3


age.drop.out.log3=glm(drop.out~age+group, data = sociodemographic.drop.out3, family = binomial)#marginally significant
summary(age.drop.out.log3)
age.drop.out.log3=tbl_regression(age.drop.out.log3, exponentiate = TRUE)


education.drop.out.log3=glm(drop.out~education+group, data = sociodemographic.drop.out3, family = binomial)
summary(education.drop.out.log3)
education.drop.out.log3=tbl_regression(education.drop.out.log3, exponentiate = TRUE)

marital.status.drop.out.log3=glm(drop.out~marital.status+group, data = sociodemographic.drop.out3, family = binomial)
summary(marital.status.drop.out.log3)
marital.status.drop.out.log3=tbl_regression(marital.status.drop.out.log3, exponentiate = TRUE)

BMI.drop.out.log3=glm(drop.out~BMI+group, data = sociodemographic.drop.out3, family = binomial)
summary(BMI.drop.out.log3)
BMI.drop.out.log3=tbl_regression(BMI.drop.out.log3, exponentiate = TRUE)

physical.exercise.drop.out.log3=glm(drop.out~physical.exercise+group, data = sociodemographic.drop.out3, family = binomial)
summary(physical.exercise.drop.out.log3)
physical.exercise.drop.out.log3=tbl_regression(physical.exercise.drop.out.log3, exponentiate = TRUE)

current.endometriosis.medication.drop.out.log3=glm(drop.out~current.endometriosis.medication+group, data = sociodemographic.drop.out3, family = binomial)
summary(current.endometriosis.medication.drop.out.log3)
current.endometriosis.medication.drop.out.log3=tbl_regression(current.endometriosis.medication.drop.out.log3, exponentiate = TRUE)

sleep.hours.per.night.drop.out.log3=glm(drop.out~sleep.hours.per.night+group, data = sociodemographic.drop.out3, family = binomial)
summary(sleep.hours.per.night.drop.out.log3)
sleep.hours.per.night.drop.out.log3=tbl_regression(sleep.hours.per.night.drop.out.log3, exponentiate = TRUE)

time.chronic.pain.drop.out.log3=glm(drop.out~time.chronic.pain+group, data = sociodemographic.drop.out3, family = binomial)
summary(time.chronic.pain.drop.out.log3)
time.chronic.pain.drop.out.log3=tbl_regression(time.chronic.pain.drop.out.log3, exponentiate = TRUE)



anxiety.drop.out.log3=glm(drop.out~anxiety+group, data = sociodemographic.drop.out3, family = binomial)
summary(anxiety.drop.out.log3)
anxiety.drop.out.log3=tbl_regression(anxiety.drop.out.log3, exponentiate = TRUE)


depression.drop.out.log3=glm(drop.out~depression+group, data = sociodemographic.drop.out3, family = binomial)
summary(depression.drop.out.log3)
depression.drop.out.log3=tbl_regression(depression.drop.out.log3, exponentiate = TRUE)

#table----


tbl_age =
  tbl_merge(tbls = list(age.drop.out.log,age.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 

tbl_education =
  tbl_merge(tbls = list(education.drop.out.log,education.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 


tbl_marital.status =
  tbl_merge(tbls = list(marital.status.drop.out.log,marital.status.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 


tbl_BMI =
  tbl_merge(tbls = list(BMI.drop.out.log,BMI.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 


tbl_physical.exercise =
  tbl_merge(tbls = list(physical.exercise.drop.out.log,physical.exercise.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 

tbl_sleep.hours.per.night =
  tbl_merge(tbls = list(sleep.hours.per.night.drop.out.log,sleep.hours.per.night.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 

tbl_time.chronic.pain =
  tbl_merge(tbls = list(time.chronic.pain.drop.out.log,time.chronic.pain.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 


tbl_anxiety =
  tbl_merge(tbls = list(anxiety.drop.out.log,anxiety.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 


tbl_depression =
  tbl_merge(tbls = list(depression.drop.out.log,depression.drop.out.log3), 
            tab_spanner = c("**Time 1**", "**Time 2**")) 


#join tables----
tbl_stack(list(tbl_age ,tbl_education ,tbl_marital.status ,tbl_BMI ,
               tbl_physical.exercise ,tbl_sleep.hours.per.night ,
               tbl_time.chronic.pain ,tbl_anxiety ,tbl_depression))


#multiple imputation endometriosis-related pain variables----


clinical.trial.pain2=clinical.trial %>%
  select(id,group,time,pelvic.pain,dysuria,dyspareunia,dyschezia, dysmenorrhea,
         pain.unpleasantness) %>% 
  pivot_wider(names_from = time,values_from=c(pelvic.pain,dysuria,dyspareunia,dyschezia, dysmenorrhea,
                                              pain.unpleasantness)) %>% 
  dummy_cols(select_columns ="group") %>% 
  select(-group_control,-group)

#correlation between variables 
triangular.cor.pain=clinical.trial.pain2 %>% 
  select(-id,-group) %>% 
  lowerCor(use = "complete.obs", method="spearman")

#plot missing pattern

md.pattern(clinical.trial.pain2)
md.pairs(clinical.trial.pain2)


pain_plot.missing.pain = aggr(clinical.trial.pain2 , col=c('navyblue','yellow'),
                              numbers=TRUE, sortVars=TRUE,
                              labels=names(clinical.trial.pain2 ), cex.axis=.7,
                              gap=3, ylab=c("Missing data","Pattern"))


#construct the predictor matrix setting: -2  to indicate the cluster variable, 1 imputation model with a fixed effect and a random intercept(default)

pain.p.matrix=make.predictorMatrix(clinical.trial.pain2)

pain.p.matrix[,"group_intervention"]=-2

imputed_pain2=mice(clinical.trial.pain2, m=5, predictorMatrix = pain.p.matrix, seed=125)
summary(imputed_pain2)


#multiple imputation perceived stress variable----

clinical.trial.stress=clinical.trial %>%
  select(id,group,time,pelvic.pain,pain.unpleasantness,PSS_10_total) %>% 
  pivot_wider(names_from = time,values_from=c(pelvic.pain,pain.unpleasantness,PSS_10_total)) %>% 
  dummy_cols(select_columns ="group") %>% 
  select(-group_control,-group)

names(clinical.trial.stress)

#correlation between variables 

triangular.cor.pain.stress=clinical.trial.stress %>% 
  select(-id,-group) %>% 
  lowerCor(use = "complete.obs", method="spearman")


#plot missing pattern

md.pattern( clinical.trial.stress)
md.pairs( clinical.trial.stress)


stress_plot.missing.stress = aggr( clinical.trial.stress, col=c('navyblue','yellow'),
                                   numbers=TRUE, sortVars=TRUE,
                                   labels=names( clinical.trial.stress), cex.axis=.7,
                                   gap=3, ylab=c("Missing data","Pattern"))


#construct the predictor matrix setting: -2  to indicate the cluster variable, 1 imputation model with a fixed effect and a random intercept(default)

stress.p.matrix=make.predictorMatrix(clinical.trial.stress)
stress.p.matrix[,"group_intervention"]=-2

imputed_stress=mice(clinical.trial.stress, m=5, predictorMatrix =  stress.p.matrix, seed=124)
summary(imputed_stress)


#multiple imputation sf_36----
clinical.trial.impute.sf.36=clinical.trial %>%
  select(id,group,time, sf_36_physical.functioning,sf_36_limitations.physical.functioning,
         sf_36_pain, sf_36_general.health,sf_36_vitality,sf_36_social.function,
         sf_36_emotional.role,sf_36_mental.health) %>% 
  pivot_wider(names_from = time,values_from=c(sf_36_physical.functioning,sf_36_limitations.physical.functioning,
                                              sf_36_pain, sf_36_general.health,sf_36_vitality,sf_36_social.function,
                                              sf_36_emotional.role,sf_36_mental.health))  %>% 
  dummy_cols(select_columns ="group") %>% 
  select(-group_control,-group)


clinical.trial.impute.sf.36.wcx=clinical.trial %>%
  select(id,group,time, sf_36_physical.functioning,sf_36_limitations.physical.functioning,
         sf_36_pain, sf_36_general.health,sf_36_vitality,sf_36_social.function,
         sf_36_emotional.role,sf_36_mental.health) %>% 
  pivot_wider(names_from = time,values_from=c(sf_36_physical.functioning,sf_36_limitations.physical.functioning,
                                              sf_36_pain, sf_36_general.health,sf_36_vitality,sf_36_social.function,
                                              sf_36_emotional.role,sf_36_mental.health))  

#summary physical and mental health

clinical.trial.impute.sf.36.sum=clinical.trial %>%
  select(id,group,time, sf.36.physical.sum,sf.36.mental.sum) %>% 
  pivot_wider(names_from = time,values_from=c( sf.36.physical.sum,sf.36.mental.sum))  %>% 
  dummy_cols(select_columns ="group") %>% 
  select(-group_control,-group)

#correlation between variables 
triangular.cor.sf.36=clinical.trial.impute.sf.36 %>% 
  select(-id,-group_intervention) %>% 
  lowerCor(use = "complete.obs", method="spearman")

triangular.cor.sf.36.sum=clinical.trial.impute.sf.36.sum %>% 
  select(-id,-group_intervention) %>% 
  lowerCor(use = "complete.obs", method="spearman")

#plot missing pattern

md.pattern( clinical.trial.impute.sf.36)
md.pairs( clinical.trial.impute.sf.36)

md.pattern( clinical.trial.impute.sf.36.sum)
md.pairs( clinical.trial.impute.sf.36.sum)

plot.missing.sf.36 = aggr( clinical.trial.impute.sf.36, col=c('navyblue','yellow'),
                           numbers=TRUE, sortVars=TRUE,
                           labels=names( clinical.trial.impute.sf.36), cex.axis=.7,
                           gap=3, ylab=c("Missing data","Pattern"))


plot.missing.sf.36.sum = aggr( clinical.trial.impute.sf.36.sum, col=c('navyblue','yellow'),
                               numbers=TRUE, sortVars=TRUE,
                               labels=names( clinical.trial.impute.sf.36.sum), cex.axis=.7,
                               gap=3, ylab=c("Missing data","Pattern"))

#construct the predictor matrix setting: -2  to indicate the cluster variable, 1 imputation model with a fixed effect and a random intercept(default)

sf.36.p.matrix=make.predictorMatrix( clinical.trial.impute.sf.36)
sf.36.p.matrix[,"group_intervention"]=-2

imputed_sf.36=mice(clinical.trial.impute.sf.36, m=5, predictorMatrix =   sf.36.p.matrix, seed=123)
summary(imputed_sf.36)

sf.36.p.matrix.sum=make.predictorMatrix( clinical.trial.impute.sf.36.sum)
sf.36.p.matrix.sum[,"group_intervention"]=-2

imputed_sf.36.sum=mice(clinical.trial.impute.sf.36.sum, m=5, predictorMatrix =   sf.36.p.matrix.sum,seed=128)
summary(imputed_sf.36)


#imputed to wilcox test

impute.sf.36.wcx=mice(clinical.trial.impute.sf.36.wcx, seed = 167)
summary(impute.sf.36.wcx)


#Robust linear (MM-type estimators) raw scores, imputed data and Ordinary Least Squares without outliers  models on endometriosis-related pain ----

# Evaluate the presence of near zero variance endometriosis related pain---
#convert data from long to wide by time and group
var.test.pain= clinical.trial %>% 
  select(id, group, time, pelvic.pain, dysuria,dyschezia,dyspareunia,dysmenorrhea,pain.unpleasantness,PSS_10_total) %>% 
  pivot_wider(names_from = time|group,values_from = c( pelvic.pain, dysuria,dyschezia,dyspareunia,
                                                       dysmenorrhea,pain.unpleasantness,PSS_10_total))
nearzero.pain=var.test.pain %>% 
  nearZeroVar(saveMetrics = TRUE)

which(nearzero.pain$zeroVar=='TRUE') # variable with zero var
which(nearzero.pain$nzv=='TRUE') # variable with near zero var

#no near zero variable

#pelvic pain----
cor_pelvic.pain=clinical.trial %>% 
  select(id,group,time,pelvic.pain) %>% 
  pivot_wider(names_from = time, values_from = pelvic.pain) 

plot.pelvic.pain=cor_pelvic.pain %>%
  select(group,t1,t2,t3) 

library("GGally")
install.packages("effectsize")
library(effectsize)
ggpairs(plot.pelvic.pain, ggplot2::aes(colour=group)) #evaluate correlation between group and time

#t2

#robust

p.pain.lm.adj2=lmrob(t2~t1+group, data = cor_pelvic.pain) 
summary(p.pain.lm.adj2)
effectsize(p.pain.lm.adj2)

par(mfrow=c(2,2))
plot(p.pain.lm.adj2)
tidy(p.pain.lm.adj2, conf.int = TRUE)


#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(52-2-1)=0,08

cor_pelvic.pain2.out=cor_pelvic.pain[-c(48,49),]#without outlier
p.pain.lm.adj2.out=lm(t2~t1+group, data = cor_pelvic.pain2.out) 
summary(p.pain.lm.adj2.out)
par(mfrow=c(2,2))
plot(p.pain.lm.adj2.out)
plot(p.pain.lm.adj2.out,4)

#t3

p.pain.lm.adj3=lmrob(t3~t1+group, data = cor_pelvic.pain) 
summary(p.pain.lm.adj3)
par(mfrow=c(2,2))
plot(p.pain.lm.adj3)

#cook's distance 4/(45-2-1)=0,09

cor_pelvic.pain3.out=cor_pelvic.pain[-c(58,54),]#without outlier
p.pain.lm.adj3.out=lm(t3~t1+group, data = cor_pelvic.pain3.out) 
summary(p.pain.lm.adj3.out)
par(mfrow=c(2,2))
plot(p.pain.lm.adj3.out,id.n=5)
plot(p.pain.lm.adj3.out,4,id.n=5)

#pelvic pain imputed dataframe----
names(clinical.trial.imputed.pain)

#t2


p.pain.lm.imputed=with(imputed_pain2,lmrob(pelvic.pain_t2~pelvic.pain_t1+group_intervention))
summary(p.pain.lm.imputed)
p.pain.lm.imputed.pool=summary(pool(p.pain.lm.imputed), conf.int = TRUE)
tibble(p.pain.lm.imputed.pool)


#t3

p.pain.lm.imputed.t3=with(imputed_pain2,lmrob(pelvic.pain_t3~pelvic.pain_t1+group_intervention))
summary(p.pain.lm.imputed.t3)
p.pain.lm.imputed.pool.t3=summary(pool(p.pain.lm.imputed.t3), conf.int = TRUE)
tibble(p.pain.lm.imputed.pool.t3)


#dysuria----

cor_dysuria=clinical.trial %>% 
  select(id,group,time,dysuria) %>% 
  pivot_wider(names_from = time, values_from = dysuria) 

plot.dysuria=cor_dysuria %>%
  select(group,t1,t2,t3) 


GGally::ggpairs(plot.dysuria, ggplot2::aes(colour=group)) #evaluate correlation between group and times

#robust

#t2

dysuria.lm.adj2=lmrob(t2~t1+group, data = cor_dysuria) 
summary(dysuria.lm.adj2)
plot(dysuria.lm.adj2)

#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(53-2-1)=0,08

cor_dysuria2.out=cor_dysuria[-c(6,15,62,40),]#without outlier
dysuria.lm.adj2.out=lm(t2~t1+group, data = cor_dysuria2.out) 
summary(dysuria.lm.adj2.out)
par(mfrow=c(2,2))
plot(dysuria.lm.adj2.out, id.n=5)
plot(dysuria.lm.adj2.out,4, id.n=5)


#t3
#robust

dysuria.lm.adj3=lmrob(t3~t1+group, data = cor_dysuria) 
summary(dysuria.lm.adj3)
plot(dysuria.lm.adj3)

#lm
#cook's distance  4/(46-2-1)=0,09
cor_dysuria3.out=cor_dysuria[-c(15,40, 58,42),]#without outlier
dysuria.lm.adj3.out=lm(t3~t1+group, data = cor_dysuria3.out) 
summary(dysuria.lm.adj3.out)
par(mfrow=c(2,2))
plot(dysuria.lm.adj3.out, id.n=5)
plot(dysuria.lm.adj3.out,4, id.n=6)

#dysuria imputed dataframe----

#t2

dysuria.lm.imputed=with(imputed_pain2,lmrob(dysuria_t2~dysuria_t1+group_intervention))
summary(dysuria.lm.imputed)
dysuria.lm.imputed.pool=summary(pool(dysuria.lm.imputed), conf.int = TRUE)
tibble(dysuria.lm.imputed.pool)

#t3

dysuria.lm.imputed.t3=with(imputed_pain2,lmrob(dysuria_t3~dysuria_t1+group_intervention))
summary(dysuria.lm.imputed.t3)
dysuria.lm.imputed.pool.t3=summary(pool(dysuria.lm.imputed.t3), conf.int = TRUE)# marginally significant

#dyspareunia----

cor_dyspareunia=clinical.trial %>% 
  select(id,group,time,dyspareunia) %>% 
  pivot_wider(names_from = time, values_from = dyspareunia) 

plot.dyspareunia=cor_dyspareunia %>%
  select(group,t1,t2,t3) 


GGally::ggpairs(plot.dyspareunia, ggplot2::aes(colour=group)) #evaluate correlation between group and times

#model adjusted for baseline

#t2

#robust

dyspareunia.lm.adj2=lmrob(t2~t1+group, data = cor_dyspareunia) 
summary(dyspareunia.lm.adj2)
plot(dyspareunia.lm.adj2)


#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(54-2-1)=0,07

cor_dyspareunia2.out=cor_dyspareunia[-c(10,45),]#without outlier
dyspareunia.lm.adj2.out=lm(t2~t1+group, data = cor_dyspareunia2.out) 
summary(dyspareunia.lm.adj2.out)
par(mfrow=c(2,2))
plot(dyspareunia.lm.adj2.out, id.n=5)
plot(dyspareunia.lm.adj2.out, 4)

#t3
#robust

dyspareunia.lm.adj3=lmrob(t3~t1+group, data = cor_dyspareunia) 
summary(dyspareunia.lm.adj3)
plot(dyspareunia.lm.adj3)

#lm without outlier
#4/(46-2-1)=0,09

cor_dyspareunia3=cor_dyspareunia[-c(54,32,4),]#without outlier
dyspareunia.lm.adj3.out=lm(t3~t1+group, data = cor_dyspareunia3) 
summary(dyspareunia.lm.adj3.out)
par(mfrow=c(2,2))
plot(dyspareunia.lm.adj3.out)
plot(dyspareunia.lm.adj3.out,4)

#dyspareunia imputed dataframe----

#t2

dyspareunia.lm.imputed=with(imputed_pain2,lmrob(dyspareunia_t2~dyspareunia_t1+group_intervention))
summary(dyspareunia.lm.imputed)
dyspareunia.lm.imputed.pool=summary(pool(dyspareunia.lm.imputed), conf.int = TRUE)
tibble(dyspareunia.lm.imputed.pool)

#t3

dyspareunia.lm.imputed.t3=with(imputed_pain2,lm(dyspareunia_t3~dyspareunia_t1+group_intervention))
summary(dyspareunia.lm.imputed.t3)
dyspareunia.lm.imputed.pool.t3=summary(pool(dyspareunia.lm.imputed.t3), conf.int = TRUE)

#dyschezia----

cor_dyschezia=clinical.trial %>% 
  select(id,group,time,dyschezia) %>% 
  pivot_wider(names_from = time, values_from = dyschezia) 

plot.dyschezia=cor_dyschezia %>%
  select(group,t1,t2,t3) 


GGally::ggpairs(plot.dyschezia, ggplot2::aes(colour=group)) #evaluate correlation between group and times

#model adjusted for baseline

#t2

#robust

dyschezia.lm.adj2=lmrob(t2~t1+group, data = cor_dyschezia) 
summary(dyschezia.lm.adj2)
par(mfrow=c(2,2))
plot(dyschezia.lm.adj2)


#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(56-2-1)=0,07

cor_dyschezia2=cor_dyschezia[-c(40,32,47),]#without outlier
dyschezia.lm.adj.out=lm(t2~t1+group, data = cor_dyschezia2) 
summary(dyschezia.lm.adj.out)
par(mfrow=c(2,2))
plot(dyschezia.lm.adj.out, id.n=5)
plot(dyschezia.lm.adj.out, 4)

#t3
#robust
dyschezia.lm.adj3=lmrob(t3~t1+group, data = cor_dyschezia) 
summary(dyschezia.lm.adj3)
plot(dyschezia.lm.adj3)

#lm without outlier 4/(56-2-1)=0,07

cor_dyschezia3=cor_dyschezia[-c(8,13,32,40,44),]#without outlier
dyschezia.lm.adj3.out=lm(t3~t1+group, data = cor_dyschezia3) 
summary(dyschezia.lm.adj3.out)
par(mfrow=c(2,2))
plot(dyschezia.lm.adj3.out, id.n=5)
plot(dyschezia.lm.adj3.out,4)

#dyschezia imputed dataframe----

#t2


dyschezia.lm.imputed=with(imputed_pain2,lmrob(dyschezia_t2~dyschezia_t1+group_intervention))
summary(dyschezia.lm.imputed)
dyschezia.lm.imputed.pool=summary(pool(dyschezia.lm.imputed), conf.int = TRUE)
tibble(dyschezia.lm.imputed.pool)

#t3


dyschezia.lm.imputed.t3=with(imputed_pain2,lm(dyschezia_t3~dyschezia_t1+group_intervention))
summary(dyschezia.lm.imputed.t3)
dyschezia.lm.imputed.pool.t3=summary(pool(dyschezia.lm.imputed.t3), conf.int = TRUE)


#dysmenorrhea----

cor_dysmenorrhea=clinical.trial %>% 
  select(id,group,time,dysmenorrhea) %>% 
  pivot_wider(names_from = time, values_from = dysmenorrhea) 

plot.dysmenorrhea=cor_dysmenorrhea %>%
  select(group,t1,t2,t3) 


GGally::ggpairs(plot.dysmenorrhea, ggplot2::aes(colour=group)) #evaluate correlation between group and times

#model adjusted for baseline

#t2

#robust
dysmenorrhea.lm.adj2=lmrob(t2~t1+group, data = cor_dysmenorrhea) 
summary(dysmenorrhea.lm.adj2)
plot(dysmenorrhea.lm.adj2)

#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(56-2-1)=0,07

cor_dysmenorrhea2=cor_dysmenorrhea[-c(52,62,36),]#without outlier
dysmenorrhea.lm.adj.out=lm(t2~t1+group, data = cor_dysmenorrhea2) 
summary(dysmenorrhea.lm.adj.out)
par(mfrow=c(2,2))
plot(dysmenorrhea.lm.adj.out, id.n=5)
plot(dysmenorrhea.lm.adj.out,4)

#t3

#robust
dysmenorrhea.lm.adj3=lmrob(t3~t1+group, data = cor_dysmenorrhea) 
summary(dysmenorrhea.lm.adj3)
plot(dysmenorrhea.lm.adj3)

#lm without outlier 4/(46-2-1)=0,09

cor_dysmenorrhea3=cor_dysmenorrhea[-c(36,50,63,15),]#without outlier
dysmenorrhea.lm.adj3.out=lm(t3~t1+group, data = cor_dysmenorrhea3) 
summary(dysmenorrhea.lm.adj3.out)
par(mfrow=c(2,2))
plot(dysmenorrhea.lm.adj3.out, id.n=5)
plot(dysmenorrhea.lm.adj3.out, 4)

#dysmenorrhea imputed dataframe----

#t2

dysmenorrhea.lm.imputed=with(imputed_pain2,lmrob(dysmenorrhea_t2~dysmenorrhea_t1+group_intervention))
summary(dysmenorrhea.lm.imputed)
dysmenorrhea.lm.imputed.pool=summary(pool(dysmenorrhea.lm.imputed), conf.int = TRUE)
tibble(dysmenorrhea.lm.imputed.pool)

#t3

dysmenorrhea.lm.imputed.t3=with(imputed_pain2,lmrob(dysmenorrhea_t3~dysmenorrhea_t1+group_intervention))
summary(dysmenorrhea.lm.imputed.t3)
dysmenorrhea.lm.imputed.pool.t3=summary(pool(dysmenorrhea.lm.imputed.t3), conf.int = TRUE)


#pain.unpleasantness----

cor_pain.unpleasantness=clinical.trial %>% 
  select(id,group,time,pain.unpleasantness) %>% 
  pivot_wider(names_from = time, values_from = pain.unpleasantness) 

plot.pain.unpleasantness=cor_pain.unpleasantness %>%
  select(group,t1,t2,t3) 


GGally::ggpairs(plot.pain.unpleasantness, ggplot2::aes(colour=group)) #evaluate correlation between group and times

#model adjusted for baseline

#t2

pain.unpleasantness.lm.adj2=lmrob(t2~t1+group, data = cor_pain.unpleasantness) 
summary(pain.unpleasantness.lm.adj2)
par(mfrow=c(2,2))
plot(pain.unpleasantness.lm.adj2)

#lm without outlier
# cook's distance > 0,07

cor_pain.unpleasantness2=cor_pain.unpleasantness[-c(8,39,43,50),]#without outlier
pain.unpleasantness.lm.adj.out=lm(t2~t1+group, data = cor_pain.unpleasantness2) 
summary(pain.unpleasantness.lm.adj.out)
par(mfrow=c(2,2))
plot(pain.unpleasantness.lm.adj.out, id.n=5)
plot(pain.unpleasantness.lm.adj.out,4)


#t3
#robust

pain.unpleasantness.lm.adj3=lmrob(t3~t1+group, data = cor_pain.unpleasantness) 
summary(pain.unpleasantness.lm.adj3)
plot(pain.unpleasantness.lm.adj3)

#lm without outlier
# cook's distance > 0,09

cor_pain.unpleasantness3=cor_pain.unpleasantness[-c(50,32),]#without outlier
pain.unpleasantness.lm.adj3.out=lm(t3~t1+group, data = cor_pain.unpleasantness3) 
summary(pain.unpleasantness.lm.adj3.out)
par(mfrow=c(2,2))
plot(pain.unpleasantness.lm.adj3.out,id.n=5)
plot(pain.unpleasantness.lm.adj3.out,4)

#pain.unpleasantness imputed dataframe----

#t2



pain.unpleasantness.lm.imputed=with(imputed_pain2,lmrob(pain.unpleasantness_t2~pain.unpleasantness_t1+group_intervention))
summary(pain.unpleasantness.lm.imputed)
pain.unpleasantness.lm.imputed.pool=summary(pool(pain.unpleasantness.lm.imputed), conf.int = TRUE)
tibble(pain.unpleasantness.lm.imputed.pool)

#t3


pain.unpleasantness.lm.imputed.t3=with(imputed_pain2,lmrob(pain.unpleasantness_t3~pain.unpleasantness_t1+group_intervention))
summary(pain.unpleasantness.lm.imputed.t3)
pain.unpleasantness.lm.imputed.pool.t3=summary(pool(pain.unpleasantness.lm.imputed.t3), conf.int = TRUE)


#perceived stress----

cor_stress=clinical.trial %>% 
  select(id,group,time,PSS_10_total) %>% 
  pivot_wider(names_from = time, values_from = PSS_10_total) 

plot.stress=cor_stress %>%
  select(group,t1,t2,t3) 


GGally::ggpairs(plot.stress, ggplot2::aes(colour=group)) #evaluate correlation between group and times

#model adjusted for baseline

#t2

stress.lm.adj2=lmrob(t2~t1+group, data = cor_stress) 
summary(stress.lm.adj2)
plot(stress.lm.adj2)

#lm without outlier
# cook's distance > 0,07

cor_stress2=cor_stress[-c(39),]#without outlier
stress.lm.adj.out=lm(t2~t1+group, data = cor_stress2) 
summary(stress.lm.adj.out)
par(mfrow=c(2,2))
plot(stress.lm.adj.out, id.n=5)
plot(stress.lm.adj.out, 4)

#t3
#robust

stress.lm.adj3=lmrob(t3~t1+group, data = cor_stress) 
summary(stress.lm.adj3)
plot(stress.lm.adj3)

#lm without outlier
# cook's distance > 0,09

cor_stress3=cor_stress[-c(6,26),]#without outlier
stress.lm.adj3.out=lm(t3~t1+group, data = cor_stress3) 
summary(stress.lm.adj3.out)
par(mfrow=c(2,2))
plot(stress.lm.adj3.out, id.n=5)
plot(stress.lm.adj3.out,4)

#stress imputed dataframe----

#t2

stress.lm.imputed=with(imputed_stress,lmrob(PSS_10_total_t2~PSS_10_total_t1+group_intervention))
summary(stress.lm.imputed)
stress.lm.imputed.pool=summary(pool(stress.lm.imputed), conf.int = TRUE)
tibble(stress.lm.imputed.pool)



#t3

stress.lm.imputed3=with(imputed_stress,lmrob(PSS_10_total_t3~PSS_10_total_t1+group_intervention))
summary(stress.lm.imputed3)
stress.lm.imputed.pool3=summary(pool(stress.lm.imputed3), conf.int = TRUE)
tibble(stress.lm.imputed.pool3)

#Table endometriosis-related pain models----
#lm robust mm-estimator method----
#t2
p.pain.lm.adj2=p.pain.lm.adj2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.276) 

dysuria.lm.adj2=dysuria.lm.adj2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.707)

dyspareunia.lm.adj2=dyspareunia.lm.adj2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.451)

dyschezia.lm.adj2=dyschezia.lm.adj2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.618)


dysmenorrhea.lm.adj2=dysmenorrhea.lm.adj2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.633)

pain.unpleasantness.lm.adj2=pain.unpleasantness.lm.adj2  %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.474)



#t3

p.pain.lm.adj3=p.pain.lm.adj3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.208)

dysuria.lm.adj3=dysuria.lm.adj3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.493)

dyspareunia.lm.adj3=dyspareunia.lm.adj3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.422)

dyschezia.lm.adj3=dyschezia.lm.adj3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.512)

dysmenorrhea.lm.adj3=dysmenorrhea.lm.adj3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.364)

pain.unpleasantness.lm.adj3=pain.unpleasantness.lm.adj3  %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.278)

#lm OLS method without outliers----
#t2
p.pain.lm.adj2.out=p.pain.lm.adj2.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.341) 

dysuria.lm.adj2.out=dysuria.lm.adj2.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.629) 

dyspareunia.lm.adj2.out=dyspareunia.lm.adj2.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.364) 

dyschezia.lm.adj.out=dyschezia.lm.adj.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.657) 

dysmenorrhea.lm.adj.out=dysmenorrhea.lm.adj.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.328) 

pain.unpleasantness.lm.adj.out=pain.unpleasantness.lm.adj.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.538) 

#t3
p.pain.lm.adj3.out=p.pain.lm.adj3.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.236 ) 


dysuria.lm.adj3.out=dysuria.lm.adj3.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.492 ) 

dyspareunia.lm.adj3.out=dyspareunia.lm.adj3.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.461) 

dyschezia.lm.adj3.out=dyschezia.lm.adj3.out  %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.651) 

dysmenorrhea.lm.adj3.out=dysmenorrhea.lm.adj3.out  %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.460) 

pain.unpleasantness.lm.adj3.out=pain.unpleasantness.lm.adj3.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.403) 

#lm using multiple imputed dataset----


#t2
p.pain.lm.imputed.pool=p.pain.lm.imputed.pool %>% 
  filter(term=="group_intervention") 

dysuria.lm.imputed.pool=dysuria.lm.imputed.pool %>% 
  filter(term=="group_intervention") 

dyspareunia.lm.imputed.pool=dyspareunia.lm.imputed.pool %>% 
  filter(term=="group_intervention") 

dyschezia.lm.imputed.pool=dyschezia.lm.imputed.pool  %>% 
  filter(term=="group_intervention") 

dysmenorrhea.lm.imputed.pool=dysmenorrhea.lm.imputed.pool  %>% 
  filter(term=="group_intervention") 

pain.unpleasantness.lm.imputed.pool=pain.unpleasantness.lm.imputed.pool  %>% 
  filter(term=="group_intervention") 

#t3

p.pain.lm.imputed.pool.t3=p.pain.lm.imputed.pool.t3 %>% 
  filter(term=="group_intervention") 

dysuria.lm.imputed.pool.t3=dysuria.lm.imputed.pool.t3 %>% 
  filter(term=="group_intervention") 

dyspareunia.lm.imputed.pool.t3=dyspareunia.lm.imputed.pool.t3 %>% 
  filter(term=="group_intervention") 

dyschezia.lm.imputed.pool.t3=dyschezia.lm.imputed.pool.t3 %>% 
  filter(term=="group_intervention") 

dysmenorrhea.lm.imputed.pool.t3=dysmenorrhea.lm.imputed.pool.t3 %>% 
  filter(term=="group_intervention") 

pain.unpleasantness.lm.imputed.pool.t3=pain.unpleasantness.lm.imputed.pool.t3  %>% 
  filter(term=="group_intervention") 


#table lm.rob----

hist(clinical.trial$FFMQ_total)

endo.related.pain.lm.rob=p.pain.lm.adj2 %>%   
  bind_rows(dysuria.lm.adj2,dyspareunia.lm.adj2,
            dyschezia.lm.adj2,dysmenorrhea.lm.adj2,
            pain.unpleasantness.lm.adj2,p.pain.lm.adj3,
            dysuria.lm.adj3,dyspareunia.lm.adj3, dyschezia.lm.adj3,
            dysmenorrhea.lm.adj3,pain.unpleasantness.lm.adj3) %>% 
  mutate(estimate=round(estimate,3),std.error=round(std.error,3),
         statistic=round(statistic,3),p.value=round(p.value,3),
         conf.low=round(conf.low,3),conf.high=round(conf.high,3)) %>% 
  relocate(any_of(c("term", "estimate", "std.error","statistic","conf.low","conf.high","R2.adj.","p.value"))) %>% 
  rename(variable=term) %>% 
  flextable() %>%
  compose(i=c(1,2,3,4,5,6,7,8,9,10,11,12),j=1, value = as_paragraph (c("Pelvic pain t2", "Dysuria t2",
                                                                       "Dyspareunia t2","Dyschezia t2",
                                                                       "Dysmenorrhea t2","Pain unpleasantness t2",
                                                                       "Pelvic pain t3", "Dysuria t3",
                                                                       "Dyspareunia t3","Dyschezia t3",
                                                                       "Dysmenorrhea t3","Pain unpleasantness t3"))) %>% 
  autofit()



#table lm.ols----

endo.related.pain.lm.ols=p.pain.lm.adj2.out %>%   
  bind_rows(dysuria.lm.adj2.out,dyspareunia.lm.adj2.out,
            dyschezia.lm.adj.out,dysmenorrhea.lm.adj.out, pain.unpleasantness.lm.adj.out,
            p.pain.lm.adj3.out,dysuria.lm.adj3.out,dyspareunia.lm.adj3.out,
            dyschezia.lm.adj3.out,dysmenorrhea.lm.adj3.out,pain.unpleasantness.lm.adj3.out) %>% 
  mutate(estimate=round(estimate,3),std.error=round(std.error,3),
         statistic=round(statistic,3),p.value=round(p.value,3),
         conf.low=round(conf.low,3),conf.high=round(conf.high,3)) %>% 
  relocate(any_of(c("term", "estimate", "std.error","statistic","conf.low","conf.high","R2.adj.","p.value"))) %>% 
  rename(variable=term) %>% 
  flextable() %>%
  compose(i=c(1,2,3,4,5,6,7,8,9,10,11,12),j=1, value = as_paragraph (c("Pelvic pain t2", "Dysuria t2",
                                                                       "Dyspareunia t2","Dyschezia t2",
                                                                       "Dysmenorrhea t2","Pain unpleasantness t2",
                                                                       "Pelvic pain t3", "Dysuria t3",
                                                                       "Dyspareunia t3","Dyschezia t3",
                                                                       "Dysmenorrhea t3","Pain unpleasantness t3"))) %>% 
  autofit()


#table lm.imputaded.rob----

endo.related.pain.lm.imputed.rob=p.pain.lm.imputed.pool %>%   
  bind_rows(dysuria.lm.imputed.pool,
            dyspareunia.lm.imputed.pool,dyschezia.lm.imputed.pool,
            dysmenorrhea.lm.imputed.pool,pain.unpleasantness.lm.imputed.pool,
            p.pain.lm.imputed.pool.t3,dysuria.lm.imputed.pool.t3,
            dyspareunia.lm.imputed.pool.t3,dyschezia.lm.imputed.pool.t3,
            dysmenorrhea.lm.imputed.pool.t3,pain.unpleasantness.lm.imputed.pool.t3) %>% 
  mutate(estimate=round(estimate,3),std.error=round(std.error,3),
         statistic=round(statistic,3),conf.low=round(`2.5 %`,3),conf.high=round(`97.5 %`,3) ,
         df=round(df,3),p.value=round(p.value,3)) %>% 
  select(-df,-`2.5 %`,-`97.5 %`) %>% 
  relocate(any_of(c("term", "estimate", "std.error","statistic","conf.low","conf.high","p.value","R2.adj."))) %>% 
  rename(variable=term) %>% 
  flextable() %>%
  compose(i=c(1,2,3,4,5,6,7,8,9,10,11,12),j=1, value = as_paragraph (c("Pelvic pain t2", "Dysuria t2",
                                                                       "Dyspareunia t2","Dyschezia t2",
                                                                       "Dysmenorrhea t2","Pain unpleasantness t2",
                                                                       "Pelvic pain t3", "Dysuria t3",
                                                                       "Dyspareunia t3","Dyschezia t3",
                                                                       "Dysmenorrhea t3","Pain unpleasantness t3"))) %>% 
  autofit()


effect.size



#Table perceived stress models----
#lm robust mm-estimator method----

#t2
stress.lm.adj2=stress.lm.adj2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.271) 

#t3
stress.lm.adj3=stress.lm.adj3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj= 0.338) 


#lm OLS method without outliers----

#t2
stress.lm.adj.out= stress.lm.adj.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.243) 

#t3

stress.lm.adj3.out=stress.lm.adj3.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.306) 


#lm using multiple imputed dataset----

#t2
stress.lm.imputed.pool=stress.lm.imputed.pool %>% 
  filter(term=="group_intervention")

#t3
stress.lm.imputed.pool3=stress.lm.imputed.pool3 %>% 
  filter(term=="group_intervention")


#table lm.rob----

stress.lm.rob=stress.lm.adj2 %>%   
  bind_rows(stress.lm.adj3) %>% 
  mutate(estimate=round(estimate,3),std.error=round(std.error,3),
         statistic=round(statistic,3),p.value=round(p.value,3),
         conf.low=round(conf.low,3),conf.high=round(conf.high,3)) %>% 
  relocate(any_of(c("term", "estimate", "std.error","statistic","conf.low","conf.high","R2.adj.","p.value"))) %>% 
  rename(variable=term) %>% 
  flextable() %>%
  compose(i=c(1,2),j=1, value = as_paragraph (c("Perceived strress t2", "Perceived stress t3"))) %>% 
  autofit()


#table lm.OLS----

stress.lm.ols=stress.lm.adj.out %>%   
  bind_rows(stress.lm.adj3.out) %>% 
  mutate(estimate=round(estimate,3),std.error=round(std.error,3),
         statistic=round(statistic,3),p.value=round(p.value,3),
         conf.low=round(conf.low,3),conf.high=round(conf.high,3)) %>% 
  relocate(any_of(c("term", "estimate", "std.error","statistic","conf.low","conf.high","R2.adj.","p.value"))) %>% 
  rename(variable=term) %>% 
  flextable() %>%
  compose(i=c(1,2),j=1, value = as_paragraph (c("Perceived strress t2", "Perceived stress t3"))) %>% 
  autofit()

#table lm.imputaded.rob----



stress.lm.imputed.rob=stress.lm.imputed.pool %>%   
  bind_rows(stress.lm.imputed.pool3) %>% 
  mutate(estimate=round(estimate,3),std.error=round(std.error,3),
         statistic=round(statistic,3),conf.low=round(`2.5 %`,3),conf.high=round(`97.5 %`,3) ,
         df=round(df,3),p.value=round(p.value,3)) %>% 
  select(-df,-`2.5 %`,-`97.5 %`) %>% 
  relocate(any_of(c("term", "estimate", "std.error","statistic","conf.low","conf.high","p.value","R2.adj."))) %>%
  rename(variable=term) %>% 
  flextable() %>%
  compose(i=c(1,2),j=1, value = as_paragraph (c("Perceived strress t2", "Perceived stress t3"))) %>% 
  autofit()

#sf_36 subscales with low variance Wilcox test----

#subscales

sf.36.wcx.t2=sf.36.gain %>% 
  select(id,group, sf_36_limitations.physical.t1_t2 ,
         sf_36_pain.t1_t2 ,sf_36_general.health.t1_t2,
         sf_36_emotional.role.t1_t2) %>% 
  pivot_longer(-c("id","group")) %>% 
  group_by(name) %>% 
  wilcox_test(value~group ,detailed = TRUE) %>%
  adjust_pvalue()

sf.36.wcx.t3=sf.36.gain %>% 
  select(id,group,
         sf_36_limitations.physical.t1_t3,sf_36_pain.t1_t3 ,
         sf_36_general.health.t1_t3 ,
         sf_36_emotional.role.t1_t3) %>% 
  pivot_longer(-c("id","group")) %>% 
  group_by(name) %>% 
  wilcox_test(value~group ,detailed = TRUE) %>%
  adjust_pvalue()

# table wilcox sf-36-----

sf.36.wcx=sf.36.wcx.t2 %>%   
  bind_rows(sf.36.wcx.t3) %>% 
  select(name, estimate,statistic, p, conf.low, conf.high, p.adj) %>% 
  mutate(estimate=round(estimate,3),statistic=round(statistic,3),
         p=round(p,3),p.adj=round(p.adj,3), conf.low=round(conf.low,3),conf.high=round(conf.high,3)) %>% 
  relocate(any_of(c("name", "estimate","statistic","conf.low","conf.high","p","p.adj"))) %>% 
  flextable() %>%
  autofit()

View(sf.36.wcx)

#imputed
sf.36.wcx.t2.imputed= sf.36.gain.imputed %>% 
  select(id,group, sf_36_limitations.physical.t1_t2 ,
         sf_36_pain.t1_t2 ,sf_36_general.health.t1_t2 ,
         sf_36_emotional.role.t1_t2) %>% 
  pivot_longer(-c("id","group")) %>% 
  group_by(name) %>% 
  wilcox_test(value~group ,detailed = TRUE) %>%
  adjust_pvalue()


sf.36.wcx.t3.imputed=sf.36.gain.imputed %>% 
  select(id,group,
         sf_36_limitations.physical.t1_t3,sf_36_pain.t1_t3 ,
         sf_36_general.health.t1_t3,
         sf_36_emotional.role.t1_t3) %>% 
  pivot_longer(-c("id","group")) %>% 
  group_by(name) %>% 
  wilcox_test(value~group ,detailed = TRUE) %>%
  adjust_pvalue()

# table wilcox sf-36 imputed-----

sf.36.wcx.imputed=sf.36.wcx.t2.imputed %>%   
  bind_rows(sf.36.wcx.t3.imputed) %>% 
  select(name, estimate,statistic, p, conf.low, conf.high, p.adj) %>% 
  mutate(estimate=round(estimate,3),statistic=round(statistic,3),
         p=round(p,3),p.adj=round(p.adj,3), conf.low=round(conf.low,3),conf.high=round(conf.high,3))  %>% 
  relocate(any_of(c("name", "estimate","statistic","conf.low","conf.high","p","p.adj"))) %>% 
  flextable() %>%
  autofit()

# compare trimmed means of the sf_36 subscales with low variance using two sample Yuen's test between group pain change score----


library("WRS2")

#sf_36_limitations.physical.t1_t2

robust.lim.phys= yuenbt(sf_36_limitations.physical.t1_t2~group, data = sf.36.gain)
robust.lim.phys=data.frame(t(sapply(robust.lim.phys,c)))
robust.lim.phys=robust.lim.phys %>% 
  select(test,conf.int,p.value,diff)

#sf_36_pain.t1_t2

robust.pain= yuenbt(sf_36_pain.t1_t2~group, data = sf.36.gain)
robust.pain=data.frame(t(sapply(robust.pain,c)))
robust.pain=robust.pain %>% 
  select(test,conf.int,p.value,diff)

#sf_36_general.health.t1_t2

robust.general.health= yuenbt(sf_36_general.health.t1_t2~group, data = sf.36.gain)
robust.general.health=data.frame(t(sapply(robust.general.health,c)))
robust.general.health=robust.general.health %>% 
  select(test,conf.int,p.value,diff)

#sf_36_emotional.role.t1_t2

robust.emotional.role= yuenbt(sf_36_emotional.role.t1_t2~group, data = sf.36.gain)
robust.emotional.role=data.frame(t(sapply(robust.emotional.role,c)))
robust.emotional.role=robust.emotional.role %>% 
  select(test,conf.int,p.value,diff)

#sf_36_limitations.physical.t1_t3

robust.lim.phys3= yuenbt(sf_36_limitations.physical.t1_t3~group, data = sf.36.gain)
robust.lim.phys3=data.frame(t(sapply(robust.lim.phys3,c)))
robust.lim.phys3=robust.lim.phys3 %>% 
  select(test,conf.int,p.value,diff)

#sf_36_pain.t1_t3

robust.pain3= yuenbt(sf_36_pain.t1_t3~group, data = sf.36.gain)
robust.pain3=data.frame(t(sapply(robust.pain3,c)))
robust.pain3=robust.pain3 %>% 
  select(test,conf.int,p.value,diff)

#sf_36_general.health.t1_t3

robust.general.health3= yuenbt(sf_36_general.health.t1_t3~group, data = sf.36.gain)
robust.general.health3=data.frame(t(sapply(robust.general.health3,c)))
robust.general.health3=robust.general.health3 %>% 
  select(test,conf.int,p.value,diff)

#sf_36_emotional.role.t1_t3

robust.emotional.role3= yuenbt(sf_36_emotional.role.t1_t3~group, data = sf.36.gain)
robust.emotional.role3=data.frame(t(sapply(robust.emotional.role3,c)))
robust.emotional.role3=robust.emotional.role3 %>% 
  select(test,conf.int,p.value,diff)

#table Yuen's test----
#t2
sf.36.Yuen.test=robust.lim.phys %>%   
  bind_rows( robust.pain,robust.general.health,robust.emotional.role) %>% 
  add_column(variable=c("SF-36.limitation.physical.functioning","SF-36.pain", "SF-36.general.health", "SF-36.emotional.role")) %>%
  relocate(any_of(c("variable","diff" ,"test", "conf.int", "p.value"))) %>% 
  flextable() %>%
  autofit()

#t3
sf.36.Yuen.test3=robust.lim.phys3 %>%   
  bind_rows( robust.pain3,robust.general.health3,robust.emotional.role3) %>% 
  add_column(variable=c("SF-36.limitation.physical.functioning","SF-36.pain", "SF-36.general.health", "SF-36.emotional.role")) %>%
  relocate(any_of(c("variable","diff" ,"test", "conf.int", "p.value"))) %>% 
  flextable() %>%
  autofit()


# Robust linear (MM-type estimators) raw scores, imputed data and Ordinary Least Squares without outliers  models on sf 36 raw score----

# Evaluate the presence of sf 36 near zero variance----
#convert data from long to wide by time and group
var.test.sf36= clinical.trial %>% 
  select(id, group, time,sf.36.physical.sum,sf.36.mental.sum,sf_36_physical.functioning,sf_36_limitations.physical.functioning,
         sf_36_pain,sf_36_general.health,sf_36_vitality, sf_36_social.function,
         sf_36_emotional.role, sf_36_mental.health) %>% 
  pivot_wider(names_from = time|group,values_from = c( sf_36_physical.functioning,sf_36_limitations.physical.functioning,
                                                       sf_36_pain,sf_36_general.health,sf_36_vitality, sf_36_social.function,
                                                       sf_36_emotional.role, sf_36_mental.health))

nearzero.sf36=var.test.sf36 %>% 
  nearZeroVar(saveMetrics = TRUE)

which(nearzero.sf36$zeroVar=='TRUE') # variable with zero var
which(nearzero.sf36$nzv=='TRUE') # variable with near zero var

# sf.36.physical.sum----
cor_physical.sum=clinical.trial %>% 
  select(id,group,time,sf.36.physical.sum) %>% 
  pivot_wider(names_from = time, values_from = sf.36.physical.sum) 

plot.physical.sum=cor_physical.sum %>%
  select(group,t1,t2,t3) 

ggpairs( plot.physical.sum, ggplot2::aes(colour=group)) #evaluate correlation between group and time

#model adjusted for baseline

#t2

#robust

physical.sum.lm.rob2=lmrob(t2~t1+group, data = cor_physical.sum) 
summary(physical.sum.lm.rob2)
par(mfrow=c(2,2))
plot(physical.sum.lm.rob2)

#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(52-2-1)=0,08

cor_physical.sum.out=cor_physical.sum[-c(55,24),]
physical.sum.lm2=lm(t2~t1+group, data = cor_physical.sum.out) 
summary(physical.sum.lm2)
par(mfrow=c(2,2))
plot(physical.sum.lm2, id.n=5)
plot(physical.sum.lm2, 4) 

#imputed data
physical.sum.lm2.imputed=with(imputed_sf.36.sum,lmrob(sf.36.physical.sum_t2~sf.36.physical.sum_t1+group_intervention))
summary(physical.sum.lm2.imputed)
physical.sum.lm2.imputed.pool=summary(pool(physical.sum.lm2.imputed), conf.int = TRUE)
tibble(physical.sum.lm2.imputed.pool)


#t3

#robust

physical.sum.lm.adj3=lmrob(t3~t1+group, data = cor_physical.sum) 
summary(physical.sum.lm.adj3)
par(mfrow=c(2,2))
plot(physical.sum.lm.adj3)

#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(45-2-1)=0,09

cor_physical.sum.out3=cor_physical.sum[-c(55,24),]
physical.sum.lm3=lm(t3~t1+group, data = cor_physical.sum.out3) 
summary(physical.sum.lm3)
par(mfrow=c(2,2))
plot(physical.sum.lm3, id.n=5)
plot(physical.sum.lm3, 4) 

#imputed data
physical.sum.lm3.imputed=with(imputed_sf.36.sum,lmrob(sf.36.physical.sum_t3~sf.36.physical.sum_t1+group_intervention))
summary(physical.sum.lm3.imputed)
physical.sum.lm3.imputed.pool=summary(pool(physical.sum.lm3.imputed), conf.int = TRUE)
tibble(physical.sum.lm3.imputed.pool)


# sf.36.mental.sum----
cor_mental.sum=clinical.trial %>% 
  select(id,group,time,sf.36.mental.sum) %>% 
  pivot_wider(names_from = time, values_from = sf.36.mental.sum) 

plot.mental.sum=cor_mental.sum %>%
  select(group,t1,t2,t3) 

ggpairs( plot.mental.sum, ggplot2::aes(colour=group)) #evaluate correlation between group and time

#model adjusted for baseline

#t2
#robust
cor_mental.sum.out2=cor_mental.sum[-c(60,33,47,59,26),]
mental.sum.lm.rob2=lmrob(t2~t1+group, data = cor_mental.sum.out2) #poor lm model fit, robust model fit affected by outliers
summary(mental.sum.lm.rob2)
par(mfrow=c(2,2))
plot(mental.sum.lm.rob2, id.n=5)
plot(mental.sum.lm.rob2,3)
#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(52-2-1)=0,08

cor_mental.sum.out2=cor_mental.sum[-c(60,33,47,59,26),]
mental.sum.lm2=lm(t2~t1+group, data = cor_mental.sum.out2) #OLS without outliers better fitted than robust lm
summary(mental.sum.lm2)
par(mfrow=c(2,2))
plot(mental.sum.lm2, id.n=5)
plot(mental.sum.lm2,4)

#imputed 
mental.sum.lm2.imputed=with(imputed_sf.36.sum,lm(sf.36.mental.sum_t2~sf.36.mental.sum_t1+group_intervention))
summary(mental.sum.lm2.imputed)
mental.sum.lm2.imputed.pool=summary(pool(mental.sum.lm2.imputed), conf.int = TRUE)
tibble(mental.sum.lm2.imputed.pool)


#t3

mental.sum.lm.adj3=lmrob(t3~t1+group, data = cor_mental.sum) 
summary(mental.sum.lm.adj3)
par(mfrow=c(2,2))
plot(mental.sum.lm.adj3, id.n=5)
plot(mental.sum.lm.adj3,4) 

#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(45-2-1)=0,09

cor_mental.sum.out3=cor_mental.sum[-c(26),]
mental.sum.lm3=lm(t3~t1+group, data = cor_mental.sum.out3) 
summary(mental.sum.lm3)
par(mfrow=c(2,2))
plot(mental.sum.lm3, id.n=5)
plot(mental.sum.lm3,4)

#imputed 
mental.sum.lm3.imputed=with(imputed_sf.36.sum,lmrob(sf.36.mental.sum_t3~sf.36.mental.sum_t1+group_intervention))
summary(mental.sum.lm3.imputed)
mental.sum.lm3.imputed.pool=summary(pool(mental.sum.lm3.imputed), conf.int = TRUE)
tibble(mental.sum.lm3.imputed.pool)

# sf_36_physical functioning----
cor_phys.functioning=clinical.trial %>% 
  select(id,group,time,sf_36_physical.functioning) %>% 
  pivot_wider(names_from = time, values_from = sf_36_physical.functioning) 

plot.phys.functioning=cor_phys.functioning %>%
  select(group,t1,t2,t3) 

ggpairs( plot.phys.functioning, ggplot2::aes(colour=group)) #evaluate correlation between group and time

#model adjusted for baseline

#t2

#robust

phys.functioning.lm.adj2=lmrob(t2~t1+group, data = cor_phys.functioning) 
summary(phys.functioning.lm.adj2)
par(mfrow=c(2,2))
plot(phys.functioning.lm.adj2)

#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(52-2-1)=0,08

cor_phys.functioning.out=cor_phys.functioning[-c(10),]
phys.functioning.lm.adj2.out=lm(t2~t1+group, data =  cor_phys.functioning.out) 
summary(phys.functioning.lm.adj2.out)
par(mfrow=c(2,2))
plot(phys.functioning.lm.adj2.out, id.n=5)
plot(phys.functioning.lm.adj2.out, 4) 

#imputed data
phys.functioning.lm2.imputed=with(imputed_sf.36,lmrob(sf_36_physical.functioning_t2~sf_36_physical.functioning_t1+group_intervention))
summary(phys.functioning.lm2.imputed)
phys.functioning.lm2.imputed.pool=summary(pool(phys.functioning.lm2.imputed), conf.int = TRUE)
tibble(phys.functioning.lm2.imputed.pool)


#t3
#robust
phys.functioning.lm.adj3=lmrob(t3~t1+group, data = cor_phys.functioning) 
summary(phys.functioning.lm.adj3)
par(mfrow=c(2,2))
plot(phys.functioning.lm.adj3)

#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(45-2-1)=0,09

phys.functioning.out3=cor_phys.functioning[-c(10,31,30),]
phys.functioning.lm.adj3.out=lm(t3~t1+group, data =  phys.functioning.out3) 
summary(phys.functioning.lm.adj3.out)
par(mfrow=c(2,2))
plot(phys.functioning.lm.adj3.out,id.n=5)
plot(phys.functioning.lm.adj3.out, 4) 

#imputed data
phys.functioning.lm3.imputed=with(imputed_sf.36,lmrob(sf_36_physical.functioning_t3~sf_36_physical.functioning_t1+group_intervention))
summary(phys.functioning.lm3.imputed)
phys.functioning.lm3.imputed.pool=summary(pool(phys.functioning.lm3.imputed), conf.int = TRUE)
tibble(phys.functioning.lm.imputed.pool)



# sf_36_limitations physical functioning----
cor_lim.phys.functioning=clinical.trial %>% 
  select(id,group,time,sf_36_limitations.physical.functioning) %>% 
  pivot_wider(names_from = time, values_from = sf_36_limitations.physical.functioning) 

plot.lim.phys.functioning=cor_lim.phys.functioning %>%
  select(group,t1,t2,t3) 

ggpairs(plot.lim.phys.functioning, ggplot2::aes(colour=group)) #evaluate correlation between group and time

#model adjusted for baseline

#t2

#robust
lim.phys.functioning.lm.rob2=lmrob(t2~t1+group, data = cor_lim.phys.functioning)# Very poor fit because of little variance in
#intervention group t1 and no linear relation
summary(lim.phys.functioning.lm.rob2)
par(mfrow=c(2,2))
plot(lim.phys.functioning.lm.rob2)

#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(52-2-1)=0,08


lim.phys.functioning.lm.adj2=lm(t2~t1+group, data = cor_lim.phys.functioning) 
summary(lim.phys.functioning.lm.adj2)
par(mfrow=c(2,2))
plot(phys.functioning.lm.adj2, id.n=5)
plot(phys.functioning.lm.adj2,4)

#t3

lim.phys.functioning.lm.adj3=lmrob(t3~t1+group, data = cor_lim.phys.functioning) # Very poor fit because of little variance in
#intervention group t1
summary(lim.phys.functioning.lm.adj3)
par(mfrow=c(2,2))
plot(lim.phys.functioning.lm.adj3)

# sf_36_pain----
cor_sf36_pain=clinical.trial %>% 
  select(id,group,time,sf_36_pain) %>% 
  pivot_wider(names_from = time, values_from = sf_36_pain) 

plot.sf36_pain=cor_sf36_pain %>%
  select(group,t1,t2,t3) 

ggpairs( plot.sf36_pain, ggplot2::aes(colour=group)) #evaluate correlation between group and time

#model adjusted for baseline

#t2
#robust
sf36_pain.lm.rob2=lm(t2~t1+group, data = cor_sf36_pain) 
summary(sf36_pain.lm.rob2)


#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(52-2-1)=0,08

cor_sf36_pain.out=cor_sf36_pain[-55,]
sf36_pain.lm.adj2=lm(t2~t1+group, data = cor_sf36_pain.out) 
summary(sf36_pain.lm.adj2)
par(mfrow=c(2,2))
plot(sf36_pain.lm.adj2, id.n = 5)
plot(sf36_pain.lm.adj2,4)


#imputed

pain.lm2.imputed=with(imputed_sf.36,lmrob(sf_36_pain_t2~sf_36_pain_t1+group_intervention))
summary(phys.functioning.lm2.imputed)
pain.lm2.imputed.pool=summary(pool(pain.lm2.imputed))
tibble(pain.lm2.imputed.pool)

#t3

#robust
sf36_pain.lm.rob3.out=lm(t3~t1+group, data = cor_sf36_pain) #relation does not fitted by robust 
summary(sf36_pain.lm.rob3.out)

#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(45-2-1)=0,09

cor_sf36_pain.out=cor_sf36_pain[-55,]
sf36_pain.lm.adj3.out=lm(t3~t1+group, data = cor_sf36_pain.out) #relation does not fitted by OLS lm
summary(sf36_pain.lm.adj3.out)
par(mfrow=c(2,2))
plot(sf36_pain.lm.adj3.out, id.n = 5)
plot(sf36_pain.lm.adj3,4)

#imputed

pain.lm3.imputed=with(imputed_sf.36,lm(sf_36_pain_t3~sf_36_pain_t1+group_intervention))
summary(phys.functioning.lm3.imputed)
pain.lm3.imputed.pool=summary(pool(pain.lm3.imputed))
tibble(pain.lm3.imputed.pool)

# sf_36_general.health----
cor_general.health=clinical.trial %>% 
  select(id,group,time,sf_36_general.health) %>% 
  pivot_wider(names_from = time, values_from = sf_36_general.health) 

plot.general.health=cor_general.health %>%
  select(group,t1,t2,t3) 

ggpairs( plot.general.health, ggplot2::aes(colour=group)) #evaluate correlation between group and time

#model adjusted for baseline

#t2

#robust
general.health.lm.rob2=lmrob(t2~t1+group, data = cor_general.health) # very poor lm fit
summary(general.health.lm.rob2)
par(mfrow=c(2,2))
plot(general.health.lm.rob2)

#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(52-2-1)=0,08


cor_general.health.out=cor_general.health[-c(4,40,48,54),]
general.health.lm.adj2=lm(t2~t1+group, data = cor_general.health.out) # very poor lm fit
summary(general.health.lm.adj2)
par(mfrow=c(2,2))
plot(general.health.lm.adj2, id.n=5)
plot(general.health.lm.adj2,4)

#imputed

general.health.lm2.imputed=with(imputed_sf.36,lmrob(sf_36_general.health_t2~sf_36_general.health_t1+group_intervention))
summary(general.health.lm2.imputed)
general.health.lm2.imputed.pool=summary(pool(general.health.lm2.imputed))
tibble(general.health.lm2.imputed.pool)


#t3

#Robust

general.health.lm.adj3=lmrob(t3~t1+group, data = cor_general.health) 
summary(general.health.lm.adj3)
par(mfrow=c(2,2))
plot(general.health.lm.adj3)


#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(45-2-1)=0,09

cor_general.health.out3=cor_general.health[-c(26,22),]
general.health.out3=lm(t3~t1+group, data = cor_general.health.out3) 
summary(general.health.out3)
par(mfrow=c(2,2))
plot(general.health.out3, id.n=5)
plot(general.health.out3, 4)


#imputed

general.health.lm3.imputed=with(imputed_sf.36,lmrob(sf_36_general.health_t3~sf_36_general.health_t1+group_intervention))
summary(general.health.lm3.imputed)
general.health.lm3.imputed.pool=summary(pool(general.health.lm3.imputed))
tibble(general.health.lm3.imputed.pool)

# sf_36_vitality----
cor_vitality=clinical.trial %>% 
  select(id,group,time,sf_36_vitality) %>% 
  pivot_wider(names_from = time, values_from = sf_36_vitality) 

plot.vitality=cor_vitality %>%
  select(group,t1,t2,t3) 

ggpairs( plot.vitality, ggplot2::aes(colour=group)) #evaluate correlation between group and time

#model adjusted for baseline

#t2

#robust

cor_vitality.rob.out=cor_vitality[-c(59,21),]
vitality.lm.rob2=lmrob(t2~t1+group, data = cor_vitality.rob.out) 
summary(vitality.lm.rob2)
par(mfrow=c(2,2))
plot(vitality.lm.rob2)
tidy(vitality.lm.rob2, conf.int = TRUE)

#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(52-2-1)=0,08

vitality.out=cor_vitality[-c(21,59),]
vitality.lm.adj2.out=lm(t2~t1+group, data = vitality.out) 
summary(vitality.lm.adj2.out)
par(mfrow=c(2,2))
plot(vitality.lm.adj2.out, id.n=5)
plot(vitality.lm.adj2.out,4)

#imputed

vitality.lm2.imputed=with(imputed_sf.36,lmrob(sf_36_vitality_t2~sf_36_vitality_t1+group_intervention))
summary(vitality.lm2.imputed)
vitality.lm2.imputed.pool=summary(pool(vitality.lm2.imputed), conf.int = TRUE)
tibble(vitality.lm2.imputed.pool)



#t3

vitality.lm.rob3=lmrob(t3~t1+group, data = cor_vitality) 
summary(vitality.lm.rob3)
par(mfrow=c(2,2))
plot(vitality.lm.rob3)


#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(45-2-1)=0,09

cor_vitality.out=cor_vitality[-c(59),]
vitality.lm.adj3.out=lm(t3~t1+group, data = cor_vitality.out) 
summary(vitality.lm.adj3.out)
par(mfrow=c(2,2))
plot(vitality.lm.adj3.out,id.n=5)
plot(vitality.lm.adj3.out,4)


#imputed

vitality.lm3.imputed=with(imputed_sf.36,lmrob(sf_36_vitality_t3~sf_36_vitality_t1+group_intervention))
summary(vitality.lm3.imputed)
vitality.lm3.imputed.pool=summary(pool(vitality.lm3.imputed), conf.int = TRUE)
tibble(vitality.lm3.imputed.pool)

# sf_36_social.function----
cor_social.function=clinical.trial %>% 
  select(id,group,time,sf_36_social.function) %>% 
  pivot_wider(names_from = time, values_from = sf_36_social.function) 

plot.social.function=cor_social.function %>%
  select(group,t1,t2,t3) 

ggpairs( plot.social.function, ggplot2::aes(colour=group)) #evaluate correlation between group and time

#model adjusted for baseline

#t2

#robust
social.function.lm.rob2=lmrob(t2~t1+group, data = cor_social.function) 
summary(social.function.lm.rob2)
par(mfrow=c(2,2))
plot(social.function.lm.rob2)


#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(52-2-1)=0,08

cor_social.function.out=cor_social.function[-c(26,15,34),]
social.function.lm.adj2.out=lm(t2~t1+group, data = cor_social.function.out) 
summary(social.function.lm.adj2.out)
par(mfrow=c(2,2))
plot(social.function.lm.adj2.out, id.n=5)
plot(social.function.lm.adj2.out,4)

#imputed 

social.function.lm2.imputed=with(imputed_sf.36,lmrob(sf_36_social.function_t2~sf_36_social.function_t1+group_intervention))
summary(social.function.lm2.imputed)
social.function.lm2.imputed.pool=summary(pool(social.function.lm2.imputed), conf.int = TRUE)
tibble(social.function.lm2.imputed.pool)


#t3

#robust
social.function.lm.rob3=lmrob(t3~t1+group, data = cor_social.function) 
summary(social.function.lm.rob3)
par(mfrow=c(2,2))
plot(social.function.lm.rob3)


#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(45-2-1)=0,09

cor_social.function.out3=cor_social.function[-c(26,34,55),]
social.function.lm.rob3.out=lm(t3~t1+group, data = cor_social.function.out3) 
summary(social.function.lm.rob3.out)
par(mfrow=c(2,2))
plot(social.function.lm.rob3.out, id.n=5)
plot(social.function.lm.rob3.out, 4)

#imputed

social.function.lm3.imputed=with(imputed_sf.36,lmrob(sf_36_social.function_t3~sf_36_social.function_t1+group_intervention))
summary(social.function.lm3.imputed)
social.function.lm3.imputed.pool=summary(pool(social.function.lm3.imputed), conf.int = TRUE)
tibble(social.function.lm3.imputed.pool)

#sf_36_emotional.role----
cor_emotional.role=clinical.trial %>% 
  select(id,group,time,sf_36_emotional.role) %>% 
  pivot_wider(names_from = time, values_from = sf_36_emotional.role) 

plot.emotional.role=cor_emotional.role %>%
  select(group,t1,t2,t3) 

ggpairs( plot.emotional.role, ggplot2::aes(colour=group)) #evaluate correlation between group and time
#baseline intervention with very low variance and absence of linear relation. Distribution inappropriated to lm. Use wilcoxon test instead.

#t2

#robust
emotional.role.rob2=lmrob(t2~group, data= cor_emotional.role)
summary(emotional.role.rob2)

#OLS
emotional.role.lm2=lm(t2~group, data= cor_emotional.role)
summary(emotional.role.lm2)

# sf_36_mental.health----
cor_mental.health=clinical.trial %>% 
  select(id,group,time,sf_36_mental.health) %>% 
  pivot_wider(names_from = time, values_from = sf_36_mental.health) 

plot.mental.health=cor_mental.health %>%
  select(group,t1,t2,t3) 

ggpairs( plot.mental.health, ggplot2::aes(colour=group)) #evaluate correlation between group and time

#model adjusted for baseline

#t2

#robust

mental.health.lm.rob2=lmrob(t2~t1+group, data = cor_mental.health) 
summary(mental.health.lm.rob2)
par(mfrow=c(2,2))
plot(mental.health.lm.rob2)

#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(52-2-1)=0,08

cor_mental.health.out=cor_mental.health[-c(60,59,22),]
mental.health.lm.adj2.out=lm(t2~t1+group, data = cor_mental.health.out) 
summary(mental.health.lm.adj2.out)
par(mfrow=c(2,2))
plot(mental.health.lm.adj2.out, id.n=5)
plot(mental.health.lm.adj2.out, 4)

#imputed

mental.health.lm2.imputed=with(imputed_sf.36,lmrob(sf_36_mental.health_t2~sf_36_mental.health_t1+group_intervention))
summary(mental.health.lm2.imputed)
mental.health.lm2.imputed.pool=summary(pool(mental.health.lm2.imputed), conf.int = TRUE)
tibble(mental.health.lm2.imputed.pool)


#t3

mental.health.lm.rob3=lmrob(t3~t1+group, data = cor_mental.health) 
summary(mental.health.lm.adj3)
par(mfrow=c(2,2))
plot(mental.health.lm.adj3)


#lm without outlier
#considerar outlier observation that exceed 3 standard deviation in the residual vs leverage plot 
#High influential points is considered cook's distance  4/(n - p - 1)  (Bruce, Peter, and Andrew Bruce. 2017. Practical Statistics for Data Scientists. O’Reilly Media.)
#p is the number of predictors. 4/(45-2-1)=0,09

mental.health.lm3=lm(t3~t1+group, data = cor_mental.health) 
summary(mental.health.lm3)
par(mfrow=c(2,2))
plot(mental.health.lm3, id.n=5)
plot(mental.health.lm3,4)

#imputed

mental.health.lm3.imputed=with(imputed_sf.36,lmrob(sf_36_mental.health_t3~sf_36_mental.health_t1+group_intervention))
summary(mental.health.lm3.imputed)
mental.health.lm3.imputed.pool=summary(pool(mental.health.lm3.imputed), conf.int = TRUE)
tibble(mental.health.lm3.imputed.pool)


#Table sf_36 models----

#lm robust mm-estimator method----

physical.sum.lm.rob2.tab=physical.sum.lm.rob2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.226) 

mental.sum.lm.rob2.tab=mental.sum.lm.rob2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.24 )

phys.functioning.lm.adj2.tab=phys.functioning.lm.adj2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.590)

vitality.lm.rob2.tab=vitality.lm.rob2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.175)

social.function.lm.rob2.tab=social.function.lm.rob2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.188)  

mental.health.lm.rob2.tab=mental.health.lm.rob2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.256)  

#t3 

physical.sum.lm.adj3.tab= physical.sum.lm.adj3  %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.190) 
mental.sum.lm.adj3.tab=mental.sum.lm.adj3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.310)  

phys.functioning.lm.adj3.tab=phys.functioning.lm.adj3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.676)  

vitality.lm.rob3.tab=vitality.lm.rob3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.304) 

social.function.lm.rob3.tab=social.function.lm.rob3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.185) 

mental.health.lm.rob3.tab=mental.health.lm.rob3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.399) 

#lm OLS method without outliers----

#t2
physical.sum.lm2.tab=physical.sum.lm2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.312) 

mental.sum.lm2.tab=mental.sum.lm2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.215) 


phys.functioning.lm.adj2.out.tab=phys.functioning.lm.adj2.out  %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.644)


vitality.lm.adj2.out.tab=vitality.lm.adj2.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.644)


social.function.lm.adj2.out.tab=social.function.lm.adj2.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.315)

mental.health.lm.adj2.out.tab=mental.health.lm.adj2.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.406)


#t3

physical.sum.lm3.tab= physical.sum.lm3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.253) 

mental.sum.lm3.tab=mental.sum.lm3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.290) 

phys.functioning.lm.adj3.out.tab=phys.functioning.lm.adj3.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj= 0.719)

vitality.lm.adj3.out.tab=vitality.lm.adj3.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.240)


social.function.lm.rob3.out.tab= social.function.lm.rob3.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.221)


mental.health.lm3.tab=mental.health.lm3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") %>% 
  mutate(r2.adj=0.411)

  
#lm.imputaded.rob----

#t2

physical.sum.lm2.imputed.pool.tab=physical.sum.lm2.imputed.pool %>% 
  filter(term=="group_intervention") 

mental.sum.lm2.imputed.pool.tab=mental.sum.lm2.imputed.pool %>% 
  filter(term=="group_intervention") 

phys.functioning.lm2.imputed.pool.tab=phys.functioning.lm2.imputed.pool %>% 
  filter(term=="group_intervention") 

vitality.lm2.imputed.pool.tab=vitality.lm2.imputed.pool %>% 
  filter(term=="group_intervention") 

social.function.lm2.imputed.pool.tab=social.function.lm2.imputed.pool %>% 
  filter(term=="group_intervention") 

mental.health.lm2.imputed.pool.tab=mental.health.lm2.imputed.pool %>% 
  filter(term=="group_intervention") 

#t3

physical.sum.lm3.imputed.pool.tab=physical.sum.lm3.imputed.pool %>% 
  filter(term=="group_intervention") 

mental.sum.lm3.imputed.pool.tab =mental.sum.lm3.imputed.pool %>% 
  filter(term=="group_intervention") 

phys.functioning.lm3.imputed.pool.tab=phys.functioning.lm3.imputed.pool %>% 
  filter(term=="group_intervention") 

vitality.lm3.imputed.pool.tab=vitality.lm3.imputed.pool %>% 
  filter(term=="group_intervention") 

social.function.lm3.imputed.pool.tab=social.function.lm3.imputed.pool %>% 
  filter(term=="group_intervention") 

mental.health.lm3.imputed.pool.tab=mental.health.lm3.imputed.pool %>% 
  filter(term=="group_intervention") 

##table lm.robust----

sf.36.robust=physical.sum.lm.rob2.tab %>%   
  bind_rows( mental.sum.lm.rob2.tab,phys.functioning.lm.adj2.tab,
             vitality.lm.rob2.tab,social.function.lm.rob2.tab,
             mental.health.lm.rob2.tab,physical.sum.lm.adj3.tab,
             mental.sum.lm.adj3.tab,phys.functioning.lm.adj3.tab,
             vitality.lm.rob3.tab, social.function.lm.rob3.tab,
             mental.health.lm.rob3.tab) %>% 
  mutate(estimate=round(estimate,3),std.error=round(std.error,3),
         statistic=round(statistic,3),p.value=round(p.value,3),
         conf.low=round(conf.low,3),conf.high=round(conf.high,3)) %>% 
  relocate(any_of(c("term", "estimate", "std.error","statistic","conf.low","conf.high","R2.adj.","p.value"))) %>% 
  rename(variable=term) %>% 
  flextable() %>%
  compose(i=c(1,2,3,4,5,6,7,8,9,10,11,12),j=1, value = as_paragraph (c("physical summary t2", "mental summary t2",
                                                                       "physical functioning t2", "vitality t2", "social functioning t2", "mental health t2","physical summary t3",
                                                                       "mental summary t3", "physical functioning t3", "vitality t3", "social functioning t3", "mental health t3"))) %>% 
  autofit()

#table lm.OLS----

sf.36.ols=physical.sum.lm2.tab %>%   
  bind_rows(mental.sum.lm2.tab,phys.functioning.lm.adj2.out.tab,
            vitality.lm.adj2.out.tab,
            social.function.lm.adj2.out.tab,mental.health.lm.adj2.out.tab,
            physical.sum.lm3.tab,mental.sum.lm3.tab,phys.functioning.lm.adj3.out.tab,
            vitality.lm.adj3.out.tab,social.function.lm.rob3.out.tab,
            mental.health.lm3.tab) %>% 
  mutate(estimate=round(estimate,3),std.error=round(std.error,3),
         statistic=round(statistic,3),p.value=round(p.value,3),
         conf.low=round(conf.low,3),conf.high=round(conf.high,3)) %>% 
  relocate(any_of(c("term", "estimate", "std.error","statistic","conf.low","conf.high","R2.adj.","p.value"))) %>% 
  rename(variable=term) %>% 
  flextable() %>%
  compose(i=c(1,2,3,4,5,6,7,8,9,10,11,12),j=1, value = as_paragraph (c("physical summary t2", "mental summary t2",
                                                                       "physical functioning t2", "vitality t2", "social functioning t2", "mental health t2","physical summary t3",
                                                                       "mental summary t3", "physical functioning t3", "vitality t3", "social functioning t3", "mental health t3"))) %>% 
  autofit()


#table lm.imputaded.rob----

sf.36.lm.imputed.rob=physical.sum.lm2.imputed.pool.tab %>%   
  bind_rows(mental.sum.lm2.imputed.pool.tab,phys.functioning.lm2.imputed.pool.tab,
            vitality.lm2.imputed.pool.tab,social.function.lm2.imputed.pool.tab,
            mental.health.lm2.imputed.pool.tab, physical.sum.lm3.imputed.pool.tab,
            mental.sum.lm3.imputed.pool.tab,phys.functioning.lm3.imputed.pool.tab,
            vitality.lm3.imputed.pool.tab,social.function.lm3.imputed.pool.tab,
            mental.health.lm3.imputed.pool.tab) %>% 
  mutate(estimate=round(estimate,3),std.error=round(std.error,3),
         statistic=round(statistic,3),conf.low=round(`2.5 %`,3),conf.high=round(`97.5 %`,3) ,
         df=round(df,3),p.value=round(p.value,3)) %>% 
  select(-df,-`2.5 %`,-`97.5 %`) %>% 
  relocate(any_of(c("term", "estimate", "std.error","statistic","conf.low","conf.high","p.value","R2.adj."))) %>% 
  rename(variable=term) %>% 
  flextable() %>%
  compose(i=c(1,2,3,4,5,6,7,8,9,10,11,12),j=1, value = as_paragraph (c("physical summary t2", "mental summary t2",
                                                                       "physical functioning t2", "vitality t2", "social functioning t2", "mental health t2","physical summary t3",
                                                                       "mental summary t3", "physical functioning t3", "vitality t3", "social functioning t3", "mental health t3"))) %>% 
  autofit()


#PLOT MLR SIGNIFICANT----
#T2 ----
#pelvic pain
p.pain.lm.adj2=lmrob(t2~t1+group, data = cor_pelvic.pain) 
p.pain.lm.adj2.out=lm(t2~t1+group, data = cor_pelvic.pain2.out) 
p.pain.lm.imputed.pool=summary(pool(p.pain.lm.imputed), conf.int = TRUE)
#dyschezia
dyschezia.lm.adj2=lmrob(t2~t1+group, data = cor_dyschezia) 
dyschezia.lm.adj.out=lm(t2~t1+group, data = cor_dyschezia2)
dyschezia.lm.imputed.pool=summary(pool(dyschezia.lm.imputed), conf.int = TRUE)
#pain unpleasantness
pain.unpleasantness.lm.adj2=lmrob(t2~t1+group, data = cor_pain.unpleasantness) 
pain.unpleasantness.lm.adj.out=lm(t2~t1+group, data = cor_pain.unpleasantness2) 
pain.unpleasantness.lm.imputed.pool=summary(pool(pain.unpleasantness.lm.imputed), conf.int = TRUE)

#t3----
p.pain.lm.adj3
p.pain.lm.adj3.out
p.pain.lm.imputed.pool.t3
dysuria.lm.adj3
dysuria.lm.adj3.out
dysuria.lm.imputed.pool.t3
dyspareunia.lm.adj3
dyspareunia.lm.adj3.out
dyspareunia.lm.imputed.pool.t3
dyschezia.lm.adj3
dyschezia.lm.adj3.out
dyschezia.lm.imputed.pool.t3
dysmenorrhea.lm.adj3
dysmenorrhea.lm.adj3.out
dysmenorrhea.lm.imputed.pool.t3
pain.unpleasantness.lm.adj3
pain.unpleasantness.lm.adj3.out
pain.unpleasantness.lm.imputed.pool.t3

#Plot primary outcome t2----

p.pain.rob=p.pain.lm.adj2 %>%
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")


p.pain.lm=p.pain.lm.adj2.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

p.pain.lm.imputed.pool=p.pain.lm.imputed.pool%>% 
  filter(term=="group_intervention") %>% 
  rename(conf.low='2.5 %', conf.high= '97.5 %')

dyschezia.lm=dyschezia.lm.adj2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

dyschezia.lm.out=dyschezia.lm.adj.out %>%
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

dyschezia.lm.imputed.pool=dyschezia.lm.imputed.pool  %>% 
  filter(term=="group_intervention") %>% 
  rename(conf.low='2.5 %', conf.high= '97.5 %')

pain.unpleasantness.lm= pain.unpleasantness.lm.adj2 %>%
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

pain.unpleasantness.lm.out=pain.unpleasantness.lm.adj.out %>%
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

pain.unpleasantness.lm.imputed=pain.unpleasantness.lm.imputed.pool %>% 
  filter(term=="group_intervention")  %>% 
  rename(conf.low='2.5 %', conf.high= '97.5 %')




pains.mlrb=p.pain.rob %>% 
  bind_rows(p.pain.lm,p.pain.lm.imputed.pool,dyschezia.lm,
            dyschezia.lm.out,dyschezia.lm.imputed.pool,
            pain.unpleasantness.lm,pain.unpleasantness.lm.out,
            pain.unpleasantness.lm.imputed) %>% 
  select(estimate,conf.low,conf.high) %>% 
  add_column(Posttreatment=c("Pelvic.pain.primary","Pelvic.pain.OLS","Pelvic.pain.imputed",
                             "Dyschezia.primary","Dyschezia.OLS","Dyschezia.imputed",
                             "Pain.unpleasantness.primary","Pain.unpleasantness.OLS","Pain.unpleasantness.imputed"),
             .before = "estimate", analyses=c("Primary","Sensitivity.OLS.without.outliers", "Sensitivity.imputed.data",
                                              "Primary","Sensitivity.OLS.without.outliers", "Sensitivity.imputed.data",
                                              "Primary","Sensitivity.OLS.without.outliers", "Sensitivity.imputed.data"))

#t3----


p.pain.lm3=p.pain.lm.adj3 %>%
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")


p.pain.lm.out3=p.pain.lm.adj3.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

p.pain.lm.imputed.pool.t3 = p.pain.lm.imputed.pool.t3  %>% 
  filter(term=="group_intervention")%>% 
  rename(conf.low='2.5 %', conf.high= '97.5 %')

dysuria.lm.adj3=dysuria.lm.adj3%>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

dysuria.lm.out3=dysuria.lm.adj3.out %>%
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

dysuria.lm.imputed.pool.t3 =dysuria.lm.imputed.pool.t3 %>% 
  filter(term=="group_intervention")%>% 
  rename(conf.low='2.5 %', conf.high= '97.5 %')

dyspareunia.lm3=dyspareunia.lm.adj3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

dyspareunia.lm.out3=dyspareunia.lm.adj3.out %>%
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

dyspareunia.lm.imputed.pool.t3 = dyspareunia.lm.imputed.pool.t3 %>% 
  filter(term=="group_intervention")%>% 
  rename(conf.low='2.5 %', conf.high= '97.5 %')



dyschezia.lm3=dyschezia.lm.adj3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

dyschezia.lm.out3=dyschezia.lm.adj3.out %>%
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

dyschezia.lm.imputed.pool.t3=dyschezia.lm.imputed.pool.t3 %>% 
  filter(term=="group_intervention")%>% 
  rename(conf.low='2.5 %', conf.high= '97.5 %')




dysmenorrhea.lm3=dysmenorrhea.lm.adj3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")


dysmenorrhea.lm.out3=dysmenorrhea.lm.adj3.out %>%
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")


dysmenorrhea.lm.imputed.pool.t3=dysmenorrhea.lm.imputed.pool.t3%>% 
  filter(term=="group_intervention")%>% 
  rename(conf.low='2.5 %', conf.high= '97.5 %')




pain.unpleasantness.lm3= pain.unpleasantness.lm.adj3 %>%
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

pain.unpleasantness.lm.out=pain.unpleasantness.lm.adj.out %>%
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

pain.unpleasantness.lm.imputed.t3=pain.unpleasantness.lm.imputed.pool.t3 %>% 
  filter(term=="group_intervention")%>% 
  rename(conf.low='2.5 %', conf.high= '97.5 %')




pain.mlr3b=p.pain.lm3 %>% 
  bind_rows(p.pain.lm.out3,p.pain.lm.imputed.pool.t3,
            dysuria.lm.adj3,dysuria.lm.out3, dysuria.lm.imputed.pool.t3,
            dyspareunia.lm3,dyspareunia.lm.out3,dyspareunia.lm.imputed.pool.t3,
            dyschezia.lm3,dyschezia.lm.out3, dyschezia.lm.imputed.pool.t3,
            dysmenorrhea.lm3,dysmenorrhea.lm.out3,dysmenorrhea.lm.imputed.pool.t3,
            pain.unpleasantness.lm3,pain.unpleasantness.lm.out,pain.unpleasantness.lm.imputed.t3) %>% 
  select(estimate,conf.low,conf.high) %>% 
  add_column(Follow.up=c("Pelvic.pain.primary","Pelvic.pain.OLS","Pelvic.pain.imputed",
                         "Dysuria.primary","Dysuria.OLS","Dysuria.imputed",
                         "Dyspareunia.primary","Dyspareunia.OLS","Dyspareunia.imputed",
                         "Dyschezia.primary","Dyschezia.OLS","Dyschezia.imputed",
                         "Dysmenorrhea.primary","Dysmenorrhea.OLS","Dysmenorrhea.imputed",
                         "Pain.unpleasantness.primary","Pain.unpleasantness.OLS","Pain.unpleasantness.imputed"),
             .before = "estimate", analyses=c("Primary","Sensitivity.OLS.without.outliers", "Sensitivity.imputed.data",
                                              "Primary","Sensitivity.OLS.without.outliers", "Sensitivity.imputed.data",
                                              "Primary","Sensitivity.OLS.without.outliers", "Sensitivity.imputed.data",
                                              "Primary","Sensitivity.OLS.without.outliers", "Sensitivity.imputed.data",
                                              "Primary","Sensitivity.OLS.without.outliers", "Sensitivity.imputed.data",
                                              "Primary","Sensitivity.OLS.without.outliers", "Sensitivity.imputed.data")) 



#Plot secondary outcome----
#t2

mental.health.rob2=mental.health.lm.rob2 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")
mental.health.lm.adj2.out.tab=mental.health.lm.adj2.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") 

mental.health.lm2.imputed.pool.tab=mental.health.lm2.imputed.pool %>% 
  filter(term=="group_intervention") %>% 
  rename(conf.low='2.5 %', conf.high= '97.5 %')

vitality.lm2.out.tab=vitality.lm.adj2.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") 

stress.lm.adj.out= stress.lm.adj.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")



Sf.36.MLRb=vitality.lm2.out.tab %>% 
  bind_rows(mental.health.rob2,mental.health.lm.adj2.out.tab,mental.health.lm2.imputed.pool.tab) %>% 
  select(estimate,conf.low,conf.high) %>% 
  add_column(Posttreatment=c("SF-36.Vitality.primary","SF-36.Mental.Health.primary","SF-36.Mental.Health.OLS","SF-36.Mental.Health.imputed"),
             .before = "estimate", analyses=c("Sensitivity.OLS.without.outliers","Primary","Sensitivity.OLS.without.outliers",
                                              "Sensitivity.imputed.data"))



#t3----
vitality.rob3.tab=vitality.lm.rob3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") 

vitality.lm.out3=vitality.lm.adj3.out %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

mental.health.rob3.tab=mental.health.lm.rob3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention")

mental.health.lm3.tab=  mental.health.lm3 %>% 
  tidy(conf.int = TRUE) %>% 
  filter(term=="groupintervention") 

mental.health.lm3.imputed.pool.tab=  mental.health.lm3.imputed.pool %>% 
  filter(term=="group_intervention") %>% 
  rename(conf.low='2.5 %', conf.high= '97.5 %')

Sf.36.MLR3=vitality.rob3.tab %>% 
  bind_rows(vitality.lm.out3,mental.health.rob3.tab,mental.health.lm3.tab,mental.health.lm3.imputed.pool.tab) %>% 
  select(estimate,conf.low,conf.high) %>% 
  add_column(Follow.up=c("SF-36.Vitality.primary","SF-36.Vitality.OLS",
                         "SF-36.Mental.Health.primary","SF-36.Mental.Health.OLS","SF-36.Mental.Health.imputed"),
             .before = "estimate", analyses=c("Primary","Sensitivity.OLS.without.outliers",
                                              "Primary","Sensitivity.OLS.without.outliers","sensitivity.imputed.data"))

#t2----
#pain----


library("scales")



pain.mlr.plot1.2=ggplot(pains.mlrb, mapping = aes(x = Posttreatment, y = estimate,ymin = conf.low, ymax = conf.high, group=analyses)) +
  geom_pointrange(aes(shape=analyses,color=analyses)) + 
  scale_size_manual(values=c(5,5,5))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.text = element_text(size = rel(0.7)))  +
  theme(legend.position="none") +
  scale_y_continuous(breaks=seq(-5.0,0,0.5),labels = label_number(accuracy = 0.1)) +
  scale_x_discrete(labels = c("Pelvic.pain.primary"="Pelvic pain","Pelvic.pain.OLS"="Pelvic pain","Pelvic.pain.imputed"="Pelvic pain",
                              "Dyschezia.primary"="Dyschezia","Dyschezia.OLS"="Dyschezia","Dyschezia.imputed"="Dyschezia",
                              "Pain.unpleasantness.primary"="Pain unpleasantness","Pain.unpleasantness.OLS"="Pain unpleasantness",
                              "Pain.unpleasantness.imputed"="Pain unpleasantness"))+
  labs(y="",x="")+
  annotate("text", x = 9.3, y = -4.5, label= "A", size = 4)+
  coord_flip() 



#SF-36----



Sf.36.MLR2.1b.plot=ggplot(Sf.36.MLRb, mapping = aes(x = Posttreatment, y = estimate,ymin = conf.low, ymax = conf.high, group=analyses)) +
  geom_pointrange(aes(shape=analyses,color=analyses)) + 
  scale_size_manual(values=c(5,5,5))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.text = element_text(size = rel(0.8)))  +
  theme(legend.position="none") +
  scale_y_continuous(breaks=c (0.2, 4.5, 6.5,13,10,16,21,27 ,31)) +
  scale_x_discrete(labels = c("SF-36.Vitality.primary"="SF-36 Vitality",
                              "SF-36.Mental.Health.primary"="SF-36 Mental health",
                              "SF-36.Mental.Health.OLS"="SF-36 Mental health",
                              "SF-36.Mental.Health.imputed"="SF-36 Mental health"))+
  labs(y="",x="")+
  annotate("text", x = 4.47, y = 0.2, label= "A", size = 4)+
  coord_flip() 



#t3----

#Pain----
pain.mlr.plot2=ggplot(pain.mlr3, mapping = aes(x = Follow.up, y = estimate, group=analyses)) +
  geom_point(aes(shape=analyses,color=analyses)) +
  scale_size_manual(values=c(5,5,5))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.text = element_text(size = rel(0.7))) +
  theme(legend.position="none") +
  scale_y_continuous(breaks=c (-3.2, -2.7,-2.2, -1.7, -1.4)) +
  scale_x_discrete(labels = c("Pelvic.pain"="Pelvic pain",
                              "Pain.unpleasantness"="Pain unpleasantness"))+
  labs(y="")+
  annotate("text", x = 9.3, y = -4.5, label= "B", size = 4)+
  coord_flip() 

pain.mlr.plot2.2=ggplot(pain.mlr3b, mapping = aes(x = Follow.up, y = estimate,ymin = conf.low, ymax = conf.high, group=analyses)) +
  geom_pointrange(aes(shape=analyses,color=analyses)) + 
  scale_size_manual(values=c(5,5,5))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.text = element_text(size = rel(0.7))) +
  theme(legend.position="none") +
  scale_y_continuous(breaks=seq(-6.0,0,0.5)) +
  scale_x_discrete(labels = c("Pelvic.pain.primary"="Pelvic pain","Pelvic.pain.OLS"="Pelvic pain","Pelvic.pain.imputed"="Pelvic pain",
                              "Dysuria.primary"="Dysuria","Dysuria.OLS"="Dysuria","Dysuria.imputed"="Dysuria",
                              "Dyspareunia.primary"="Dyspareunia","Dyspareunia.OLS"="Dyspareunia","Dyspareunia.imputed"="Dyspareunia",
                              "Dyschezia.primary"="Dyschezia","Dyschezia.OLS"="Dyschezia","Dyschezia.imputed"="Dyschezia",
                              "Dysmenorrhea.primary"="Dysmenorrhea","Dysmenorrhea.OLS"="Dysmenorrhea","Dysmenorrhea.imputed"="Dysmenorrhea",
                              "Pain.unpleasantness.primary"="Pain unpleasantness","Pain.unpleasantness.OLS"="Pain unpleasantness",
                              "Pain.unpleasantness.imputed"="Pain unpleasantness"))+
  labs(y="",x="")+
  annotate("text", x = 18.3, y = -6, label= "B", size = 4)+
  coord_flip() 

#SF-36---

Sf.36.MLR3.plot=ggplot(Sf.36.MLR3, mapping = aes(x = Follow.up,ymin = conf.low, ymax = conf.high, y = estimate, group=analyses)) +
  geom_pointrange(aes(shape=analyses,color=analyses)) + 
  scale_size_manual(values=c(5,5,5))+
  theme(axis.text.x = element_text(face="bold"),
        axis.text.y = element_text(face="bold"),
        axis.text = element_text(size = rel(0.8)))  +
  scale_x_discrete(labels = c("SF-36.Vitality.primary"="SF-36 Vitality",
                              "SF-36.Vitality.OLS"="SF-36 Vitality",
                              "SF-36.Mental.Health.primary"="SF-36 Mental health",
                              "SF-36.Mental.Health.OLS"="SF-36 Mental health",
                              "SF-36.Mental.Health.imputed"="SF-36 Mental health"))+
  labs(y="",x="")+
  theme(legend.position="none") +
  scale_y_continuous(breaks=c (2.5,7, 9, 16, 19,28,31)) +
  annotate("text", x = 5.5, y = 2, label= "B", size = 4)+
  coord_flip() 


#grade plots----
grid.arrange(pain.mlr.plot1.2, pain.mlr.plot2.2 , ncol=1)

grid.arrange(Sf.36.MLR2.plot, Sf.36.MLR3.plot , ncol=1)


