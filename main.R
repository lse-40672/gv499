# --- LOAD NECCESARY PACKAGES AND SETUP ---

# setwd(DELETED FOR MARKING ANONYMITY) 
library(tidyverse)
library(fixest)
library(fect)
library(ggfixest)
library(patchwork)

# --- LOAD DATA --
df = read_csv("data/panel.csv")

m1 = feols(
  fml   = turnout ~ i(eitc) + unemploy + jail + minwage | countyID + period,
  data  = df
)
m1 |> print()

m2 <- feols(
  fml = turnout ~ i(rel.time, state, ref = -1) + jail + unemploy | countyID + period,
  data = df
)
m2 |> iplot()

m3 = fect(
  formula = turnout ~ eitc + jail + unemploy + minwage,
  data = df,
  index = c("countyID", "period"),
  method = "mc",
  seed = 2003,
  se = T
)
m3 |> print()

m4 = fect(
  formula = turnout ~ eitc + jail + unemploy + minwage,
  data = df,
  index = c("countyID", "period"),
  method = "ife",
  seed = 2003,
  se = T
)
m4 |> print()

# --- PLOTS and FIGURES --- 
plot1 = m3 |>
  plot(start0 = T) + theme_bw() + labs(
    title = "Aggregated ATT: 1.885** (0.6837)", x = "General Elections Since the Treatment Began"
  )
ggsave("figs/panel.png", plot1, width = 8, height = 5)


plot2 = m3 |>
  plot(
     start0 = T,
     type = "equiv",
     ylim = c(-2, 2)
  ) + theme_bw() + labs(
    title = "", x = "General Elections Since the Treatment Began"
  ) + theme(legend.position = "bottom")

ggsave("figs/equiv.png", plot2, width = 8, height = 5)


plot3 = ggiplot(m2) + theme_bw() + labs(title = "", x = "General Elections Since the Treatment Began")
ggsave("figs/unpar.png", plot3, width = 8, height = 3)

plot3a = ggiplot(m2) + theme_bw() + labs(title = "Two Way Fixed Effects", x = "General Elections Since the Treatment Began")

plot4 = m4 |>
  plot(start0 = T) + theme_bw() + labs(
    title = "Interactive Fixed Effects Counterfactual",
  )

combplot = (plot3a / plot4)
ggsave("figs/robustmain.png", combplot, width = 8, height = 6)
