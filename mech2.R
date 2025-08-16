# --- LOAD NECCESARY PACKAGES AND SETUP ---

# setwd(DELETED FOR MARKING ANONYMITY) 
library(tidyverse)
library(haven)
library(fixest)
library(ggfixest)
library(patchwork)
library(HonestDiD)


# --- VARIABLES TO EXTRAC TFROM DATA

vars = c(
  "prov",     # state
  "q1",       # gender
  "q2",       # age
  "l1b",      # left-right (continuous, 1-10)
  "it1",      # inter-personal trust in others (1 to 4)
  "q10new",   # income (categorical)
  "ed_usa",   # education (ordinal)
  "ing4",     # dem support: democracy best form of gov (1 to 7)
  "pn4",      # dem support: satisfaction with democracy (1 to 4)
  "b1",       # courts give fair trial (trust in judic, 1 to 7)
  "b2",       # respect for pol. institutions (1 to 7)
  "b47a",     # trust in elections (1 to 7)
  "b21a",     # trust in executive (1 to 7)
  "b21",      # trust in pol parties (1 to 7)
  "b13",      # trust in national legislature (1 to 7)
  "pol1",     # interest in politics (1 to 4)
  "vb1",      # registered to vote
  "vb2"       # voted in last election
)

# --- FILTER 2017 WAVE DATA FOR RELEVANT VARIABLES ---
survey.df = read_dta("data/2017.dta")

survey.df = survey.df |>
  select(all_of(vars))

vars = c("b21a", "b21", "b13", "ing4", "pn4", "b47a", "b1", "b2",
                  "q2", "l1b", "q10new", "ed_usa", "it1", "pol1", "vb1", "vb2")
survey.df = survey.df |>
  mutate(across(all_of(vars), as.numeric))

# vb1 and vb2 and q1 alter binary coding
survey.df$vb1 = ifelse(
  survey.df$vb1 == 2,
  0, 
  survey.df$vb1
)

survey.df$vb2 = ifelse(
  survey.df$vb2 == 2,
  0, 
  survey.df$vb2
)

survey.df$q1 = ifelse(
  survey.df$q1 == 2,
  0, 
  survey.df$q1
)


# --- Logistic models ---

m1 = femlm(
  fml     = vb2 ~ b21a + i(q1) + q2 + l1b + ed_usa + q10new | prov,
  data    = survey.df,
  family  = "logit"
)

m2 = femlm(
  fml     = vb2 ~ b2 + i(q1) + q2 + l1b + ed_usa + q10new | prov,
  data    = survey.df,
  family  = "logit"
)

m3 = femlm(
  fml     = vb2 ~ ing4 + i(q1) + q2 + l1b + ed_usa + q10new | prov,
  data    = survey.df,
  family  = "logit"
)

list(m1, m2, m3) |> esttex(
  se.below      = T,
  digits        = 3,
  digits.stats  = 4,
  keep = c("%vb2", "%b21a", "%b2", "%ing4"),
  file          = "figs/surveytbl2.tex",
  replace       = T,
  title         = "Logistic Regression Estimates for Perception of Political Institutions on Individual Turnout. Estimates for control variables are not included.",
  dict          = c(
    q1      = "Male",
    q2      = "Age",
    l1b     = "Political Beliefs",
    ed_usa  = "Education Level",
    q10new  = "Monthly Income",
    it1     = "Interpersonal Trust",
    vb2     = "Voted in the Last Election",
    CA      = "State",
    b2      = "Respect for Institutions",
    ing4    = "Trust in Democracy",
    b21a    = "Trust in Executive",
    pn4     = "Satisfaction with Democracy")
)



# --- Linear models ---

m1l = feols(
  fml   = vb2 ~ b21a + i(q1) + q2 + l1b + ed_usa + q10new | prov,
  data  = survey.df
)

m2l = feols(
  fml   = vb2 ~ b2 + i(q1) + q2 + l1b + ed_usa + q10new | prov,
  data  = survey.df
)

m3l = feols(
  fml   = vb2 ~ ing4 + i(q1) + q2 + l1b + ed_usa + q10new | prov,
  data  = survey.df
)

list(m1l, m2l, m3l) |> esttex(
  se.below      = T,
  digits        = 3,
  digits.stats  = 4,
  keep = c("%vb2", "%b21a", "%b2", "%ing4"),
  file          = "figs/surveyrob1.tex",
  replace       = T,
  title         = "Linear estimates for perceptions of political institutions on turnout.",
  dict          = c(
    q1      = "Male",
    q2      = "Age",
    l1b     = "Political Beliefs",
    ed_usa  = "Education Level",
    q10new  = "Monthly Income",
    it1     = "Interpersonal Trust",
    vb2     = "Voted in the Last Election",
    CA      = "State",
    b2      = "Respect for Institutions",
    ing4    = "Trust in Democracy",
    b21a    = "Trust in Executive",
    pn4     = "Satisfaction with Democracy")
)

# --- Probit models ---

m1p = feglm(
  fml     = vb2 ~ b21a + i(q1) + q2 + l1b + ed_usa + q10new | prov,
  data    = survey.df,
  family  = binomial(link = "probit")
)

m2p = feglm(
  fml     = vb2 ~ b2 + i(q1) + q2 + l1b + ed_usa + q10new | prov,
  data    = survey.df,
  family  = binomial(link = "probit")
)

m3p = feglm(
  fml     = vb2 ~ ing4 + i(q1) + q2 + l1b + ed_usa + q10new | prov,
  data    = survey.df,
  family  = binomial(link = "probit")
)

list(m1p, m2p, m3p) |> esttex(
  se.below      = T,
  digits        = 3,
  digits.stats  = 4,
  keep = c("%vb2", "%b21a", "%b2", "%ing4"),
  file          = "figs/surveyrob2.tex",
  replace       = T,
  title         = "Probit estimates for perceptions of political institutions on turnout.",
  dict          = c(
    q1      = "Male",
    q2      = "Age",
    l1b     = "Political Beliefs",
    ed_usa  = "Education Level",
    q10new  = "Monthly Income",
    it1     = "Interpersonal Trust",
    vb2     = "Voted in the Last Election",
    CA      = "State",
    b2      = "Respect for Institutions",
    ing4    = "Trust in Democracy",
    b21a    = "Trust in Executive",
    pn4     = "Satisfaction with Democracy")
)
