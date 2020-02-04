library(readr)
library(dplyr)
library(splines) # part of R 3.6.0
library(texreg)

d <- read_tsv("reconnect-temp-dataset.tsv")

# built in A2, see comments there
d$cv3 <- d$iso3c %in% c("BEL", "CYP", "LUX") # like cv but removes GRC, ITA
d$semipres <- d$iso3c %in% c("AUT", "BGR", "CYP", "CZE", "FIN", "FRA", "IRL", "POL", "PRT", "ROU", "SVN", "SVK")
d$semipres[ d$iso3c == "CZE" & d$year <= 2010 ] <- FALSE

# use FIN as baseline
d$iso3c <- relevel(factor(d$iso3c), ref = "FIN")

# MS-'fixed effects' and election-level 'temporal effects' only

f <- formula(
  voter_turnout ~
    # ------------------------------------------------------ MS-level dummies --
    #ms_small + # sig
    #ms_small1 + # n.s.
    #ms_small2 + # n.s.
    #ms_large + # n.s.
    #ms_large1 + # n.s.
    #ms_large2 + # n.s.
    #ms_recent + # works only if turnout_last_ne not included
    #ms_eastern + # works only if turnout_last_ne not included
    #semipres + # n.s.
    # also, semipres
    # - kills ms_small here
    # - also kills ms_eastern in vot.OLS in A2
    # ---------------------------------------------------- turnout at last NE --
    lne_voter_turnout + # works
    # --------------------------------------------------- EP election dummies --
    cv + # works only if country dummies are removed
    #cv3 + # works whether or not country dummies are included
    # (cv3 is also discussed in A2 turnout models)
    first_ep_election +
    ###early +
    # ------------------------------------------ EP election temporal effects --
    #includes_weekdays2 +
    #multiple_days2 +
    ne_concomitant + # works
    ne_before +
    tt_last_ne +
    # ------------------------------------ country (dummies) and year effects --
    factor(iso3c) +
    year
)

# f <- update.formula(f, ~ . - factor(iso3c))
# M1 <- broom::tidy(estimatr::lm_robust(f, data = d, clusters = iso3c, se_type = "stata")) %>% 
#   mutate(model = "M1")
# f <- update.formula(f, ~ . - year + bs(year, degree = 3))
# M2 <- broom::tidy(estimatr::lm_robust(f, data = d, clusters = iso3c, se_type = "stata")) %>% 
#   mutate(model = "M2")
# library(ggplot2)
# bind_rows(M1, M2) %>% 
#   filter(term != "(Intercept)") %>% 
#   ggplot() +
#   geom_pointrange(aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high, color = model), position = position_dodge(width = .5)) +
#   geom_hline(yintercept = 0, lty = "dashed") +
#   coord_flip() +
#   theme_bw(base_size = 14) +
#   theme(panel.grid = element_blank())

T1 <- lm(f, data = d)
T0 <- lm(update(f, ~ . - factor(iso3c)), data = d)

# adding splines (df = 3 and 5), as Schwemmer and Wieczorek (2019)
T2 <- lm(update(f, ~ . - year + bs(year, degree = 3)), data = d)
T3 <- lm(update(f, ~ . - year + bs(year, degree = 5)), data = d)

screenreg(list(T0, T1, T2, T3), single.row = TRUE) %>% 
  print()

htmlreg(list(T0, T1, T2, T3), single.row = TRUE) %>% 
  writeLines("models-2019-11-14.html")

# -- results -------------------------------------------------------------------
#
# - significant temporal effects:
#   - turnout_last_ne
#   - early
#
# - sig. country dummies:
#
#   - BEL (far greater turnout) {cv}
#   - LUX (far greater turnout) {cv}
#   - ITA (far greater turnout) {cv, for a while}
#
#   - MLT (far greater turnout) {small country}
#   - LTU (far greater turnout) {small country}
#
#   - GBR (far weaker turnout)
#   - NLD (far weaker turnout)
#   - SWE (far weaker turnout)
#
