# Load libraries
library(tidyverse)
library(lme4)

# Loading Qualtrics data
df = droplevels(read.csv(file.path(dirname(rstudioapi::getSourceEditorContext()$path),'qualtrics_data.csv')
                         , stringsAsFactors=FALSE)[-c(1, 2),])

# Basic exclusions
df <- df[as.Date(df$StartDate) >= as.Date("2019-03-26"),]
df <- df[df$Finished %in% c('True', 'TRUE'),]
df <- df[df$Status == 'IP Address',]
df <- df[df$debrief_agree != 'I WITHDRAW my participation for this study.',]

# Convert to numeric
num_cols <- c('Duration..in.seconds.', sprintf("X%s_Q53_Page.Submit",seq(1:16)))
df[num_cols] <- sapply(df[,num_cols], function(x) {as.double(as.character(x))})
bin_cols <- sprintf("X%s_answer",seq(1:16))
df[bin_cols] <- sapply(df[,bin_cols], function(x) {as.logical(x)})

# Defining composite variables
df$dem_age <- 2019-strtoi(df$dem_birth)
df$subject_politics <- apply(df,1,function(x) substr(x["dem_pol_3"], 0, 4)) 
df$ResponseId <- as.numeric(rownames(df))

# Checking distributions
summary(df$Duration..in.seconds.)
median(df$Duration..in.seconds.)/60
60/(median(df[df$Duration..in.seconds. < 500,]$Duration..in.seconds.)/60)*0.8
table(df$check_1)
table(df$check_2)

table(df$follow_random)
table(df$follow_search)

table(df$consent)
table(df$debrief_agree)

table(df$group)

summary(df$dem_age)
table(df$dem_education)
table(df$dem_gender)
table(df$dem_pol_1)
table(df$dem_pol_2)
table(df$dem_pol_3)

df$true_count <- rowSums(df[,sprintf("X%s_answer",seq(1:16))], na.rm = TRUE)
hist(df$true_count)
summary(df$true_count)

# User-based exclusions
df %>% filter(df$follow_search != 'Yes',
              df$debrief_agree != 'I WITHDRAW my participation for this study.',
              df$check_1 == 'John is easy to see.',
              df$true_count >= 1,
              df$true_count <= 15
) -> df

# Convert to long format
dfl <- reshape(df,
               varying = list(sprintf("X%s_answer",seq(1:16)), sprintf("X%s_Q53_Page.Submit",seq(1:16)),
                              sprintf("true_votes_%s",seq(1:16)), sprintf("true_votes_num_%s",seq(1:16)), 
                              sprintf("headline_id_%s",seq(1:16)), sprintf("headline_text_%s",seq(1:16))),
               timevar = c("question"),
               times = seq(1,16),
               v.names = c('answer', 'answer_time', 'true_votes', 'true_votes_num', 'headline', 'headline_text'),
               direction = "long")
dfl$headline_politics = apply(dfl,1,function(x) substr(x["headline"], 0, 3)) 
dfl[dfl$group=='control',]$true_votes = 'none'
dfl$answer_perc <- dfl$answer*100

# Dop non-political headlines
dfl = dfl[dfl$headline_politics %in% c('Dem', 'Rep') & dfl$answer %in% c('FALSE', 'TRUE'),]

# Define relative variables
dfl$headline_alignment <- ifelse((dfl$subject_politics=='Left' & dfl$headline_politics=='Dem') |
                                   (dfl$subject_politics=='Righ' & dfl$headline_politics=='Rep'), "aligned headline", "opposed headline")

dfl$crowd_alignment <- ifelse((dfl$subject_politics=='Left' & dfl$group=='maj-dem') |
                                (dfl$subject_politics=='Righ' & dfl$group=='maj-rep'), "aligned crowd", "opposed crowd")  
dfl[dfl$group=='control',]$crowd_alignment <- 'none'

dfl$informative_signal <- ifelse((dfl$group=='maj-dem' & dfl$headline_politics=='Dem' & dfl$true_votes=='low') |
                                   (dfl$group=='maj-dem' & dfl$headline_politics=='Rep' & dfl$true_votes=='high') |
                                   (dfl$group=='maj-rep' & dfl$headline_politics=='Dem' & dfl$true_votes=='high') |
                                   (dfl$group=='maj-rep' & dfl$headline_politics=='Rep' & dfl$true_votes=='low'), 
                                 "strong signal", "weak signal")
dfl[dfl$group=='control',]$informative_signal <- 'none'

dfl$opportunity <- ifelse((dfl$headline_alignment=='aligned headline' & dfl$true_votes=='high') |
                            (dfl$headline_alignment=='opposed headline' & dfl$true_votes=='low'), "opportune vote", "unsuitable vote")
dfl[dfl$group=='control',]$opportunity <- 'none'

# Convert to numeric
dfl$true_votes_num <- ifelse(dfl$true_votes=='high', 1, ifelse(dfl$true_votes=='low', -1, 0))
dfl$crowd_alignment_num <- ifelse(dfl$crowd_alignment=="aligned crowd", 1, ifelse(dfl$crowd_alignment=="opposed crowd", -1, 0))
dfl$informative_signal_num <- ifelse(dfl$informative_signal=="strong signal", 1, ifelse(dfl$informative_signal=="weak signal", -1, 0))
dfl$opportunity_num <- ifelse(dfl$opportunity=="opportune vote", 1, ifelse(dfl$opportunity=="unsuitable vote", -1, 0))
dfl$dem_education_num <- as.numeric(factor(dfl$dem_education, levels=c('Less than a high school diploma','High school degree or equivalent (e.g. GED)',
                                                                       'Associate degree (e.g. AA, AS)','Bachelor’s degree (e.g. BA, BS)','Master’s degree (e.g. MA, MS)',
                                                                       'Professional degree (e.g. MD, JD)','Doctorate (e.g. PhD, EdD)')))

# Define factor levels
dfl$crowd_alignment <- factor(dfl$crowd_alignment, levels = c("opposed crowd", "aligned crowd", "none"))
dfl$informative_signal <- factor(dfl$informative_signal, levels = c("weak signal", "strong signal", "none"))
dfl$opportunity <- factor(dfl$opportunity, levels = c("opposed headline","unsuitable vote", "none"))

# Analysis
## Summarizes responses across specified subgroups
response_summary <- function(d, groupvars=NULL, valuevar='answer', filterval=TRUE){
  d %>% dplyr::group_by_at(vars(one_of(groupvars, valuevar))) %>%
    dplyr::summarise(count = n()) %>% 
    dplyr::group_by_at(vars(one_of(groupvars))) %>% 
    dplyr::mutate(
      total = sum(count), 
      estimate = count/total
    ) %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(
      lower = Hmisc::binconf(count,total)[2],
      upper = Hmisc::binconf(count,total)[3]
    ) %>% dplyr::ungroup() %>% dplyr::filter(!!sym(valuevar)==filterval)
}

# Absolute model: Do the different treatments have an effect compared to the control group?
dfl$effect <- paste("::", dfl$true_votes, " signal from ", dfl$group)
dfl[dfl$group=='control',]$effect = '__'
ols_model_abs<-lm(answer~
                    subject_politics*headline_politics+
                    subject_politics:headline_politics:effect, data=dfl)
summary(ols_model_abs) 
confint(ols_model_abs)

log_model_abs<-glmer(answer~subject_politics*headline_politics+
                       subject_politics:headline_politics:effect+
                       (1 | ResponseId) + (1 | headline), 
                     data=dfl, family=binomial)
summary(log_model_abs)

# Relative model: Is the difference between majority-rep vs majority-dem significant / does crowd composition have an effect?
log_model_rel <- glmer(answer ~ headline_politics*subject_politics*true_votes + 
                         headline_politics:subject_politics:group:true_votes +
                         + (1 | ResponseId) + (1 | headline),
                     data=dfl[dfl$group!='control',], family=binomial)
summary(log_model_rel) 

## Direct tests of the two interesting cases
dfl %>% filter(subject_politics=='Left', headline_politics=='Dem', true_votes=='low') %>%
  glm(answer ~ group, data = ., family = "binomial") %>% summary()
dfl %>% filter(subject_politics=='Left', headline_politics=='Rep', true_votes=='high') %>%
  glm(answer ~ group, data = ., family = "binomial") %>% summary()

# Functional model: How much of people's reaction is the unpolitical bandwagon, the political bandwagon or rational learning?
ols_model_fun <-lm(answer_perc ~ 
                     headline_politics + 
                     headline_alignment + 
                     subject_politics + 
                     subject_politics:true_votes_num +
                     subject_politics:true_votes_num:crowd_alignment_num +
                     subject_politics:true_votes_num:opportunity_num +
                     subject_politics:true_votes_num:informative_signal_num
                   , data=dfl)
summary(ols_model_fun) 


log_model_fun<-glmer(answer ~ 
                     headline_politics + 
                     headline_alignment + 
                     subject_politics + 
                     subject_politics:true_votes_num +
                     subject_politics:true_votes_num:crowd_alignment_num +
                     subject_politics:true_votes_num:opportunity_num +
                     subject_politics:true_votes_num:informative_signal_num +
                     (1 | ResponseId) + (1 | headline),
                   data=dfl, family=binomial(link = "logit"))
summary(log_model_fun) 
exp(cbind(OR = coef(log_model_fun), confint(log_model_fun)))

# Post-hoc power analysis based on actual data
install.packages('simr')
library(simr)
powerSim(log_model_fun, test=fixed('subject_politicsRigh:true_votes_num:informative_signal_num'))
powerSim(ols_model_rel, test=fixed('headline_politicsRep:subject_politicsLeft:true_voteshigh:groupmaj-rep'))
pwr.f2.test(u = 11, v = 7631, f2 = 0.005, sig.level = 0.05, power = NULL)

# Results graph
position_dodge3 <- function() {
  ggproto("PositionDodge3", Position,
          compute_panel = function(data, params, scales) {
            width = 0.07
            offset = ifelse(data$shape=='Majority-Democrat group', -width, 
                            ifelse(data$shape=='Majority-Republican group', width, 0))
            if (all(c("xmin", "xmax") %in% names(data))) {
              data$xmax = data$xmax + offset
              data$xmin = data$xmin + offset
            }
            data$x = data$x + offset
            data
          }
  )
}
pos = position_dodge3()
response_summary(dfl, valuevar="answer", groupvars=c("group", 'subject_politics', 
                                                     'headline_politics','true_votes')) %>%
  # filter(headline_politics == 'Dem')
  # mutate(
  #   estimate = ifelse(true_votes %in% c('high'), ifelse(group == 'maj-rep', estimate+1, estimate), estimate),
  #   lower = ifelse(true_votes %in% c('high'), ifelse(group == 'maj-rep', lower+1, lower), lower),
  #   upper = ifelse(true_votes %in% c('high'), ifelse(group == 'maj-rep', upper+1, upper), upper),
  # ) %>%
  mutate(
    true_votes = recode(true_votes, high='Social rating\nconfirms news', low='Social rating\nrefutes news', 
                        none='No social\nrating shown', .default = 'NA'),
    true_votes = factor(true_votes, levels = c('Social rating\nrefutes news', 'No social\nrating shown', 
                                               'Social rating\nconfirms news')),
    group_text = recode(group, control='No social signal (control)', 'maj-dem'='Majority-Democrat group', 
                        'maj-rep'='Majority-Republican group',  .default = 'NA'),
    group_text = factor(group_text, levels = c('No social signal (control)', 'Majority-Democrat group', 
                                               'Majority-Republican group')),
    subject_politics = recode(subject_politics, Left='Liberal participants', Righ='Conservative participants'),
    headline_politics = recode(headline_politics, Dem='News favorable to Democrats', 
                               Rep='News favorable to Republicans', .default = 'NA')
  ) %>%
  ggplot(aes(y=estimate, x=true_votes, color=subject_politics, 
             group=subject_politics, shape=group_text)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), color='black', width=0.1, pos=pos, alpha=0.4) +
  geom_point(size=3, position=pos) + 
  geom_line(pos=pos, data = . %>% filter(group %in% c("control" , "maj-dem"))) +
  geom_line(pos=pos, data = . %>% filter(group %in% c("control" , "maj-rep"))) +
  facet_grid(~headline_politics) +
  scale_color_manual(values=c('#c12b1e', '#020a7b')) + 
  # scale_shape_manual(values = c(15,17,16)) +
  scale_shape_manual(values = c(16,68,82)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) + #limits=c(0.27,.79), 
  labs(x='', y='Reported news credibility', color='Participant ideology', shape='Group partisanship') +
  theme_bw()

ggsave(
  'results.png',
  plot = last_plot(),
  scale = 1,
  width = 6.10,
  height = 3.35,
  units = 'in',
  dpi = 1200,
  limitsize = FALSE
)
  