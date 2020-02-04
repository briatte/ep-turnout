# library(dotwhisker)
library(dplyr)
library(lme4)
library(plm)
# library(prais)
library(readr)

d <- read_tsv("reconnect-temp-dataset.tsv")# %>% 
  #group_by(iso3c)# %>% 
  #mutate(voter_turnout_L1 = dplyr::lag(voter_turnout, 1))

# d <- group_by(d, iso3c) %>%
#   arrange(iso3c, year) %>%
#   mutate(
#     voter_turnout_L1 = dplyr::lag(voter_turnout),
#     voter_turnout_D1 = voter_turnout - voter_turnout_L1,
#     vap_turnout_L1 = dplyr::lag(vap_turnout),
#     vap_turnout_D1 = vap_turnout - vap_turnout_L1,
#     stf_dem_m_L1 = dplyr::lag(stf_dem_m),
#     stf_dem_m_D1 = stf_dem_m - stf_dem_m_L1
#   )
# select(d, starts_with("voter_turnout"), starts_with("stf_dem_m"))
# d$voter_turnout <- d$voter_turnout_D1
# d$vap_turnout <- d$vap_turnout_D1
# d$stf_dem_m <- d$stf_dem_m_D1

# d <- filter(d, year != 2019)
# small1 and small2 work only on vap.OLS

# # Kostelka coding; CZE ambiguous, different in 2010
# d$semipres <- d$iso3c %in% c("AUT", "BGR", "CYP", "CZE", "FIN", "FRA", "IRL", "POL", "PRT", "ROU", "SVN", "SVK")
# d$semipres[ d$iso3c == "CZE" & d$year <= 2010 ] <- FALSE

# d <- d[ d$year <= 2009, ]
## d <- d[ !d$cv, ]

# [TODO] add satisf. with democracy

# sensitivity: remove pre-CV Italy
d$cv2 <- d$cv
# years where Italy did not have CV
d$cv2[ d$iso3c == "ITA" & d$year %in% c(1979, 1984, 1989) ] <- NA_integer_
# ... doing this
# - removes 3 obs from each model
# - removes cv from FE models (only case where CV changes within a country)
# - does not affect vot.OLS or vap.OLS much
# - increases the effect of CV in vot.ML from 11.34 to 18
# - increases the effect of CV in vap.ML from  9.37 to 11.49
# - does not affect gdpc and ne_concomitant much in any model
# - kills tt_last_ne in vol.ML

# sensitivity: CV_enforced, Kostelka coding
# (contains Austria: see paper appendix, footnote 1 page 3)
# (we remove AUT but add LUX, which Kostelka does not cover)
d$cv3 <- d$iso3c %in% c("BEL", "CYP", "LUX") # like cv but removes GRC, ITA
# ... doing this
# - removes cv from FE models (because ITA is coded 0 consistently)
# - makes ms_eastern sig. in vot.OLS
# - increases the effect of CV in vot.ML from 11.78 to 19.61
# - **DE**creases the effect of CV in vap.OLS from 17.71 to 10.10
# - does not affect ne_concomitant, tt_last_ne and gdpc much in any model
# ... so it's a trade-off:
# - using cv3 makes ms_eastern sig. on vot.OLS (3 models out of 4 in total)
# - ... but ms_eastern still n.s. on vot.ML
# - ... and cv3 loses CV effect on vap.ML, which cv keeps
# --> [????] keep cv, although A1 models suggest that it is a better coding?
# --> rather, use cv3 and observe that:
# - ms_eastern has consistent negative effect at VAP-level, uncertain otherwise
# - CV has consistent positive effect, even though w/ large error on VAP.ML

# still ambiguous abpout whether to use ms_recent or _recent
# interaction with ms_small not obvious

vot_OLS <- formula(
  #
  voter_turnout ~
    #
    #mu_D_w +
    # ------------------------------------------------------ MS-level dummies --
    ms_eastern + # interacting ms_recent * ms_small gives complex results... !
    #ms_small +
    #ms_small1 +
    #ms_small2 +
    #ms_large +
    #ms_large1 +
    #ms_large2 +
    #semipres + # (not relevant, EP not national elections)
    # ---------------------------------------------------- turnout at last NE --
    lne_voter_turnout +
    stf_dem_m +
    # --------------------------------------------------- EP election dummies --
    early +
    cv +
    # ------------------------------------------- EP election competitiveness --
    # (not using at all, like semipres -- irrelevant, not known on voting day!)
    n_lists +
    enp_votes +
    comp12 +
    # ------------------------------------------ EP election temporal effects --
    ne_concomitant + 
    tt_last_ne + 
    ne_before + 
    includes_weekdays2 + 
    multiple_days2 +
    # ----------------------------------------------------- economic controls --
    gdpc +
    unempl +
    gini_disp
)

vap_OLS <- update.formula(vot_OLS, vap_turnout ~ .)

vot_ML <- update.formula(vot_OLS, ~ . + (1 | iso3c))
vap_ML <- update.formula(vap_OLS, ~ . + (1 | iso3c))

# # D-W tests
# lmtest::dwtest(vot_OLS, data = d)
# pdwtest(M5, data = d)
# pbnftest(M5, data = d)
# pdwtest(M6, data = d)
# pbnftest(M6, data = d)

# VOT models
# ----------

M1 <- lm(vot_OLS, data = d)
print(car::vif(M1))
# plot(density(resid(M1)))

# M1 <- prais::prais_winsten(vot_OLS, data = d)
# print(summary(M1))
# class(M1) <- c("lm", "prais")

M2 <- lmer(vot_ML, data = d, REML = FALSE) # optimize logLik
print(car::vif(M2))
# plot(density(resid(M2)))

M3 <- plm(vot_OLS, index = c("iso3c", "year"), model = "fd", data = d)

# print(car::vif(M3))
# plot(density(resid(M3)))

# VAP models
# ----------

M4 <- lm(vap_OLS, data = d)
print(car::vif(M4))
# plot(density(resid(M4)))

# M4 <- prais::prais_winsten(vap_OLS, data = d)
# print(summary(M4))
# class(M4) <- c("lm", "prais")

M5 <- lmer(vap_ML, data = d, REML = FALSE) # optimize logLik
print(car::vif(M5))
# plot(density(resid(M5)))

M6 <- plm(vap_OLS, index = c("iso3c", "year"), model = "fd", data = d)
# print(car::vif(M6))
# plot(density(resid(M6)))

# texreg::screenreg(
#   list(M1, M2, M4, M5),
#   custom.model.names = c("vot.OLS", "vot.ML", "vap.OLS", "vap.ML")
# ) %>% 
#   print

texreg::screenreg(
  list(M1, M2, M3, M4, M5, M6),
  custom.model.names = c("vot.OLS", "vot.ML", "vot.FE", "vap.OLS", "vap.ML", "vap.FE")
) %>%
  print

texreg::htmlreg(
  list(M1, M2, M3, M4, M5, M6),
  custom.model.names = c("vot.OLS", "vot.ML", "vot.FE", "vap.OLS", "vap.ML", "vap.FE"),
  file = "example-models.html",
  caption = paste0(
    "ML = varying-intercept (random-effects) models.<br />",
    "FE = fixed-effects clustered by country-year.<br />",
    "Turnout in %, tt_last_ne in months."
  )
)

# dwplot(
#   bind_rows(
#     broom::tidy(M1) %>%
#       mutate(model = "vot.OLS"),
#     broom::tidy(M2) %>%
#       mutate(model = "vot.ML"),
#     broom::tidy(M3) %>%
#       mutate(model = "vap.OLS"),
#     broom::tidy(M4) %>%
#       mutate(model = "vap.ML"),
#     broom::tidy(M5) %>%
#       mutate(model = "vot.FE"),
#     broom::tidy(M6) %>%
#       mutate(model = "vap.FE"),
#   )
# ) +
#   geom_vline(xintercept = 0, lty = "dotted")# +
  #facet_wrap(~ dv)

# ggsave("example-models.pdf", width = 10, height = 10)
