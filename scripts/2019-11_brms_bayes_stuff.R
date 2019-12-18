# BRMS Bayes Stuff  
# https://timmastny.rbind.io/blog/multilevel-mrp-tidybayes-brms-stan/#model-2-maximum-likelihood-multilevel-model


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lme4)
library(brms)
library(rstan)
library(albersusa)
library(cowplot)

rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())


# Get Data ----------------------------------------------------------------

marriage.data <- foreign::read.dta("data/gay_marriage_megapoll.dta",
                                   convert.underscore=TRUE)
Statelevel <- foreign::read.dta("data/state_level_update.dta",
                                convert.underscore = TRUE)
Census <- foreign::read.dta("data/poststratification 2000.dta",
                            convert.underscore = TRUE)



# Tidy --------------------------------------------------------------------

# add state level predictors to marriage.data
Statelevel <- Statelevel %>% rename(state = sstate)

marriage.data <- Statelevel %>%
  select(state, p.evang, p.mormon, kerry.04) %>%
  right_join(marriage.data)

# Combine demographic groups
marriage.data <- marriage.data %>%
  mutate(race.female = (female * 3) + race.wbh) %>%
  mutate(age.edu.cat = 4 * (age.cat - 1) + edu.cat) %>%
  mutate(p.relig = p.evang + p.mormon) %>%
  filter(state != "")


# Poststratification ------------------------------------------------------

# change column names for natural join with marriage.data
Census <- Census %>% 
  rename(state = cstate,
         age.cat = cage.cat,
         edu.cat = cedu.cat,
         region = cregion)

Census <- Statelevel %>%
  select(state, p.evang, p.mormon, kerry.04) %>%
  right_join(Census)

Census <- Census %>%
  mutate(race.female = (cfemale * 3 ) + crace.WBH) %>%
  mutate(age.edu.cat = 4 * (age.cat - 1) + edu.cat) %>%
  mutate(p.relig = p.evang + p.mormon)


# Model 1: Disaggragation -------------------------------------------------

# calc avg in each state by taking mean response within each state
mod.disag <- marriage.data %>%
  group_by(state) %>%
  summarise(support = mean(yes.of.all)) %>%
  mutate(model = "no_ps")

# average within each group
grp.means <- marriage.data %>%
  group_by(state, region, race.female, age.cat, 
           edu.cat, age.edu.cat, p.relig, kerry.04) %>%
  summarise(support = mean(yes.of.all, na.rm=TRUE))

# then add % within each state
grp.means <- Census %>%
  select(state, age.cat, edu.cat, region, kerry.04,
         race.female, age.edu.cat, p.relig, cpercent.state) %>%
  right_join(grp.means)

# sum scaled group averages and get total state averages
mod.disag.ps <- grp.means %>%
  group_by(state) %>%
  summarise(support = sum(support * cpercent.state, na.rm=TRUE)) %>%
  mutate(model = "ps")

# difference between post strat and empirical means
(disag.point <- bind_rows(mod.disag, mod.disag.ps) %>%
  group_by(model) %>%
  arrange(support, .by_group=TRUE) %>%
  ggplot(aes(x = support, y=forcats::fct_inorder(state), color=model)) + 
  geom_point() + 
  theme_classic() + theme(legend.position = 'none') +  
  directlabels::geom_dl(aes(label=model), method='smart.grid') +
  ylab('state'))

# we'll be using this one a lot, so let's make it a function
compare_scat <- function(d) {
  return(
    ggplot(data=d, aes(x = support, y=support1)) +
      geom_text(aes(label=state), hjust=0.5, vjust=0.25) +
      geom_abline(slope = 1, intercept = 0) +
      xlim(c(0,0.7)) + ylim(c(0,0.7)) + 
      xlab("support") + ylab("poststrat support") +
      coord_fixed()
  )
}

(disag.scat <- bind_cols(mod.disag, mod.disag.ps) %>% compare_scat())
plot_grid(disag.point, disag.scat)

# total % of demographic groups included in the survey by each state
grp.means %>%
  group_by(state) %>%
  summarise(total_percent = sum(cpercent.state, na.rm=TRUE)) %>%
  filter(state != "") %>%
  ggplot(aes(x = state, y = total_percent)) +
  geom_text(aes(label=state), hjust=0.5, vjust=0.25) +
  theme_classic(base_family = "Roboto Condensed")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + 
  coord_fixed(ratio=8) + ylim(c(0,1.1))


# Mod 2: Approximation ----------------------------------------------------

approx.mod <- glmer(formula = yes.of.all ~ (1|race.female)
                    + (1|age.cat) + (1|edu.cat) + (1|age.edu.cat)
                    + (1|state) + (1|region) + 
                      + p.relig + kerry.04,
                    data=marriage.data, family=binomial(link="logit"))


marriage.data %>%
  filter(state == "AK", state == "HI")

# estimate w predict by applying overall state average
ps.approx.mod <- Census %>%
  mutate(support = predict(approx.mod, newdata=., 
                           allow.new.levels=TRUE, type='response')) %>%
  mutate(support = support * cpercent.state) %>%
  group_by(state) %>%
  summarise(support = sum(support))


# Model 3. Full Bayesian brms ---------------------------------------------

# bayes.mod <- brm(yes.of.all ~ (1|race.female) + (1|age.cat) + (1|edu.cat)
#                  + (1|age.edu.cat) + (1|state) + (1|region)
#                  + p.relig + kerry.04,
#                  data=marriage.data, family=bernoulli(),
#                  prior=c(set_prior("normal(0,0.2)", class='b'),
#                          set_prior("normal(0,0.2)", class='sd', group="race.female"),
#                          set_prior("normal(0,0.2)", class='sd', group="age.cat"),
#                          set_prior("normal(0,0.2)", class='sd', group="edu.cat"),
#                          set_prior("normal(0,0.2)", class='sd', group="age.edu.cat"),
#                          set_prior("normal(0,0.2)", class='sd', group="state"),
#                          set_prior("normal(0,0.2)", class='sd', group="region")))
#save(bayes.mod, file = "data/bayes.mod.rda")
load("data/bayes.mod.rda")

summary(bayes.mod)


# visualize
library(tidybayes)

approx_sd <- broom::tidy(approx.mod) %>%
  filter(stringr::str_detect(term, "sd_"))

bayes.mod %>%
  gather_samples(`sd_.*`, regex=TRUE) %>%
  #gather_draws(regex = TRUE, `sd_.*`) %>%
  ungroup() %>%
  mutate(group = stringr::str_replace_all(term, c("sd_" = "","__Intercept"=""))) %>%
  ggplot(aes(y=group, x = estimate)) + 
  ggridges::geom_density_ridges(aes(height=..density..),
                                rel_min_height = 0.01, stat = "density",
                                scale=1.5) + 
  geom_point(data=approx_sd)

ps.bayes.mod <- bayes.mod %>%
  add_predicted_samples(newdata=Census, allow_new_levels=TRUE) %>%
  rename(support = pred) %>%
  mean_qi() %>%
  mutate(support = support * cpercent.state) %>%
  group_by(state) %>%
  summarise(support = sum(support))


# Model Comparisons -------------------------------------------------------

mod.disag[nrow(mod.disag) + 1,] = list("AK", mean(mod.disag$support), "no_ps")
mod.disag[nrow(mod.disag) + 1,] = list("HI", mean(mod.disag$support), "no_ps")

disag.approx <- bind_cols(mod.disag, ps.approx.mod) %>% compare_scat() +
  xlab("disag model") + ylab("approx mod")
disag.bayes <- bind_cols(mod.disag, ps.bayes.mod) %>% compare_scat() + 
  xlab("disag model") + ylab("bayes model")
approx.bayes <- bind_cols(ps.approx.mod, ps.bayes.mod) %>% compare_scat() + 
  xlab("approx model") + ylab("bayes model")

plot_grid(disag.approx, disag.bayes, approx.bayes)


# sample posterior and see distrib
str(predict(bayes.mod, newdata=Census, allow_new_levels=TRUE, 
            nsamples=500, summary=FALSE))

bayes.mod %>%
  add_predicted_samples(newdata=Census, allow_new_levels=TRUE, n=500)


