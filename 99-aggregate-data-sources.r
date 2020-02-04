library(ggcorrplot)
library(dplyr)
library(readr)

d <- read_tsv("data/country-sample.tsv", col_types = "cccccdddc") %>% 
  filter(sample == 1) %>% 
  select(iso3c, ms_recent, ms_eastern) %>% 
  inner_join(read_tsv("data/elections-ep-only.tsv"), by = "iso3c") %>% 
  select(iso3c, year, everything(), -date, -election_id)

# ------------------------------------------------------------------------------
# country-level fixed parameters
# ------------------------------------------------------------------------------

stopifnot(!is.na(d$ms_recent)) # new Member-State (2004+)
stopifnot(!is.na(d$ms_eastern)) # Eastern Member-State

# ------------------------------------------------------------------------------
# EP election-level parameters
# ------------------------------------------------------------------------------

# # remove 2019 EP elections (for now)
# stopifnot(d$year[ is.na(d$voter_turnout) ] == 2019)
# d <- filter(d, year != 2019)

stopifnot(!is.na(d$voter_turnout))
# plot(density(d$voter_turnout))
stopifnot(!is.na(d$cv)) # compulsory voting
stopifnot(!is.na(d$early)) # early EP election
stopifnot(!is.na(d$ne_concomitant)) # concomitant national election
stopifnot(!is.na(d$tt_last_ne)) # time to last national election
stopifnot(!is.na(d$ne_before)) # number of intermediary national elections
stopifnot(!is.na(d$includes_weekdays2)) # voting day of EP election
stopifnot(!is.na(d$multiple_days2)) # number of days of EP election
stopifnot(!is.na(d$comp12)) # competitiveness (difference between first two parties in vote share)
stopifnot(!is.na(d$lne_voter_turnout)) # turnout at last national parliamentary election
stopifnot(!is.na(d$enp_votes)) # ENP based on vote share

# check turnout rate sources for heterogeneity
#stopifnot(d$voter_turnout_source == "parlgov")
#stopifnot(d$lne_voter_turnout_source == "parlgov")
d <- select(d, -previous_parliament_election_id, -voter_turnout_source, -lne_voter_turnout_source, -lne_election_date, -lne_election_type)

# ------------------------------------------------------------------------------
# recodes of CV dummy
# ------------------------------------------------------------------------------

####### add recodings of CV here?

# ------------------------------------------------------------------------------
# number of EP lists
# ------------------------------------------------------------------------------

e <- select(read_tsv("data-raw/ep-lists.tsv"), -ep_date)
d <- left_join(d, e, by = c("iso3c", "year"))
stopifnot(!is.na(d$n_lists)) # number of EP lists

# ------------------------------------------------------------------------------
# state size dummies
# ------------------------------------------------------------------------------

e <- read_tsv("data-raw/covariate-population.tsv")
d <- left_join(d, select(e, -pop, -log_pop), by = c("iso3c", "year"))
stopifnot(!is.na(d$ns_small))
stopifnot(!is.na(d$ns_small1))
stopifnot(!is.na(d$ns_small2))

# ------------------------------------------------------------------------------
# satisfaction with democracy
# ------------------------------------------------------------------------------

e <- read_tsv("data-raw/eb-satisfaction-aggregates.tsv", col_types = "ciid")
d <- left_join(d, e, by = c("iso3c", "year")) %>%
  select(-stf_dem_n)

# [IMPORTANT] some missing values (n = 13)

# ------------------------------------------------------------------------------
# econ/welfare covariates
# ------------------------------------------------------------------------------

# GDP/c
e <- read_tsv("data-raw/covariate-gdpc.tsv", col_types = "cdd")
d <- left_join(d, e, by = c("iso3c", "year"))
stopifnot(!is.na(d$gdpc))
# plot(density(log(d$gdpc)))

# unemployment
e <- read_tsv("data-raw/covariate-unemployment.tsv", col_types = "cdd")
d <- left_join(d, e, by = c("iso3c", "year"))
stopifnot(!is.na(d$unempl))
# plot(density(log(d$unempl)))

# Gini
e <- read_tsv("data-raw/covariate-gini.tsv")
d <- left_join(d, select(e, -gini_source), by = c("iso3c", "year"))
stopifnot(!is.na(d$gini_disp))
# plot(density(d$gini_disp))
# table(cut_interval(d$gini_disp, 3))

# ------------------------------------------------------------------------------
# Recodes
# ------------------------------------------------------------------------------

# measure tt_last_ne in years
d$tt_last_ne  <- d$tt_last_ne  / 365
d$tt_last_ne2 <- d$tt_last_ne2 / 365

with(d, cor(voter_turnout, vap_turnout, use = "complete")) # 0.92
with(d, cor(voter_turnout, lne_voter_turnout )) # 0.71

# log GDP/capita
d$gdpc <- log(d$gdpc)

# # standardize Gini
stdize <- function(x) (x - mean(x)) / (2 * sd(x))
d$gini_disp <- stdize(d$gini_disp)

# 'static' CV dummy for fixed-effects panel models
# ------------------------------------------------

# select(d, year, first_ep_election)
e <- tapply(d$cv, d$iso3c, mean)
e[ !e %in% 0:1 ]

# remove small number of data points to make a static CV dummy
d$cv_fixed <- d$cv
d$cv_fixed[ d$iso3c == "ITA" & d$year %in% c(1979, 1984, 1989) ] <- NA_integer_
d$cv_fixed[ d$iso3c == "BGR" & d$year == 2019 ] <- NA_integer_
tapply(d$cv_fixed, d$iso3c, mean, na.rm = TRUE)

# ------------------------------------------------------------------------------
# Correlation matrix
# ------------------------------------------------------------------------------

ggcorrplot(
  round(cor(select(d, -iso3c), use = "complete"), 1),
  hc.order = TRUE,
  type = "upper",
  lab = TRUE,
  lab_size = 3,
  legend.title = NULL
)

ggsave("plots/correlation-matrix.pdf", width = 10, height = 10)

# ------------------------------------------------------------------------------
# lagged DV and differential in DV
# ------------------------------------------------------------------------------

d <- group_by(d, iso3c) %>% 
  arrange(year) %>% 
  mutate(
    first_ep_election = as.integer(year == min(year)),
    voter_turnout_L1 = dplyr::lag(voter_turnout),
    voter_turnout_D1 = voter_turnout - voter_turnout_L1
  ) %>% 
  arrange(iso3c, year) %>% 
  ungroup

glimpse(d)
write_tsv(d, "reconnect-temp-dataset.tsv")

sink("descriptives.md")
skimr::skim(d) %>% skimr::kable()
sink()

# have a nice day
