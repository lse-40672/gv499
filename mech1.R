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


# --- FILTER 2014 WAVE DATA FOR RELEVANT VARIABLES ---

df2014 = read_dta("data/2014.dta")
colnames(df2014)[147] = "ed_usa"     # change used to ed_usa

df2014 = df2014 |>
  select(all_of(vars)) |>
  filter(prov == 4006 | prov == 4053) |>
  mutate(year = 2014)


# --- FILTER 2017 WAVE DATA FOR RELEVANT VARIABLES ---
df2017 = read_dta("data/2017.dta")

df2017 = df2017 |>
  select(all_of(vars)) |>
  filter(prov == 4006 | prov == 4053) |>
  mutate(year = 2017)


# --- FILTER 2019 WAVE DATA FOR RELEVANT VARIABLES ---
df2019 = read_dta("data/2019.dta")

df2019 = df2019 |>
  select(all_of(vars)) |>
  filter(prov == 4006 | prov == 4053) |>
  mutate(year = 2019)


# --- COMBINE INTO DATASET, CLEAN DATA ---
survey.df = full_join(df2014, df2017)
survey.df = full_join(survey.df, df2019)

# create california variable
survey.df = survey.df |>
  mutate(CA = ifelse(prov == 4006, 1, 0))

# period, first.treat, rel.time variable
survey.df = survey.df |>
  mutate(period = ifelse(year == 2014, 1, ifelse(year == 2017, 2, 3))) |>
  mutate(first.treat = ifelse(CA == 1, 2, 0)) |>
  mutate(rel.time = period - 2)

# clean data
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

survey.df$pn4 = ifelse(
  survey.df$pn4 == 1 | survey.df$pn4 == 1,
  0,
  1
)


# --- ADD TREATMENT DATA ---
# CAL-EITC started in 2015. Thus, 2017 values should be set to 1 for CA
survey.df = survey.df |>
  mutate(treat = ifelse(
    CA == 1 & year >= 2017, 
    1, 
    NA
  ))

survey.df$treat = ifelse(
  survey.df$CA == 1 & survey.df$year == 2014, 
  0, 
  survey.df$treat
)

# untreated group
# https://itep.org/when-did-your-state-enact-an-earned-income-tax-credit-eitc/
survey.df = survey.df |>
  mutate(treat = ifelse(
    prov == 4053, # washington
    0,
    treat
  ))



# --- quick tests --- 
# control for q1 (gender, binary), q2 (age), l1b (left-right, cont), q10new (income, ord), ed_usa (ed, ord), it1 (trust interpresonal)

# standardise all variables.
vars = c("b13", "b21a", "b1", "b2", "ing4", "pol1")
survey.df = survey.df |>
  mutate(across(all_of(vars), scale))

# trust in legislature
m1 = feols(
  fml   = b13 ~ i(treat) + i(q1) + q2 + l1b + ed_usa + q10new + it1 | CA + year,
  data  = survey.df
)

m1d = feols(
  fml   = b13 ~ i(rel.time, CA, ref = -1) + i(q1) + q2 + l1b + ed_usa + q10new + it1 | CA + year,
  data  = survey.df,
  vcov  = ~CA
)


# trust in executive (*)
m2 = feols(
  fml   = b21a ~ i(treat) + i(q1) + q2 + l1b + ed_usa + q10new + it1 | CA + year,
  data  = survey.df
)

m2d = feols(
  fml   = b21a ~ i(rel.time, CA, ref = -1) + i(q1) + q2 + l1b + ed_usa + q10new + it1 | CA + year,
  data  = survey.df,
  vcov  = ~CA
)



# trust in courts
m3 = feols(
  fml   = b1 ~ i(treat) + i(q1) + q2 + l1b + ed_usa + q10new + it1 | CA + year,
  data  = survey.df
)

m3d = feols(
  fml   = b1 ~ i(rel.time, CA, ref = -1) + i(q1) + q2 + l1b + ed_usa + q10new + it1 | CA + year,
  data  = survey.df,
  vcov  = ~CA
)



# respect for pol institutions (*)
m4 = feols(
  fml   = b2 ~ i(treat) + i(q1) + q2 + l1b + q10new + ed_usa + it1 | CA + year,
  data  = survey.df
)


m4d <- feols(
  fml   = b2 ~ i(rel.time, CA, ref = -1) +i(q1) + q2 + l1b + ed_usa + q10new + it1 | CA + year,
  data  = survey.df,
  vcov  = ~CA
)



# trust in democracy (**)
m5 = feols(
  fml   = ing4 ~ i(treat) + i(q1) + q2 + l1b + q10new + ed_usa + it1 | CA + year,
  data  = survey.df
)


m5d = feols(
  fml   = ing4 ~ i(rel.time, CA, ref = -1) + i(q1) + q2 + l1b + ed_usa + q10new + it1 | CA + year,
  data  = survey.df,
  vcov  = ~CA
)





# --- TABLE ---
list(m1, m2, m3) |> esttex(
  se.below      = T,
  digits        = 3,
  digits.stats  = 4,
  file          = "figs/surveytbl.tex",
  replace       = T,
  title         = "TWFE Estimates for EITC on Political Trust",
  dict          = c(
    treat   = "EITC",
    q1      = "Male",
    q2      = "Age",
    l1b     = "Political Beliefs",
    ed_usa  = "Education Level",
    q10new  = "Monthly Income",
    it1     = "Interpersonal Trust",
    year    = "Year",
    CA      = "State",
    b13     = "Trust in Legislature",
    b21a    = "Trust in Executive",
    b1      = "Trust in Judiciary")
)

list(m5, m4) |> esttex(
  se.below      = T,
  digits        = 3,
  digits.stats  = 4,
  file          = "figs/surveytbl1.tex",
  replace       = T,
  title         = "TWFE Estimates for EITC on Political Trust",
  dict          = c(
    treat   = "EITC",
    q1      = "Male",
    q2      = "Age",
    l1b     = "Political Beliefs",
    ed_usa  = "Education Level",
    q10new  = "Monthly Income",
    it1     = "Interpersonal Trust",
    year    = "Year",
    CA      = "State",
    b2      = "Respect for Institutions",
    ing4    = "Trust in Democracy",
    pn4     = "Satisfaction with Democracy")
)


# --- EVENT STUDY PLOTS --- 
m1plot = ggiplot(m1d) + theme_bw() + labs(title = "Trust in Legislature", x = "Time to Treatment", y = "")
m2plot = ggiplot(m2d) + theme_bw() + labs(title = "Trust in Executive", x = "Time to Treatment", y = "")
m3plot = ggiplot(m3d) + theme_bw() + labs(title = "Trust in Judiciary", x = "Time to Treatment", y = "")
m4plot = ggiplot(m4d) + theme_bw() + labs(title = "Respect for Institutions", x = "Time to Treatment", y = "")
m5plot = ggiplot(m5d) + theme_bw() + labs(title = "Support for Democracy", x = "Time to Treatment", y = "")

plot = (m5plot / m4plot / m2plot)
ggsave("figs/surveyplot.png", plot, width = 8, height = 8)


