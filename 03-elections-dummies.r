# ------------------------------------------------------------------------------
# Recode CV (compulsory voting) and code early EP elections
# -> Output: data/elections-parliament-and-ep.tsv
#
# Package dependencies
# ------------------------------------------------------------------------------

library(dplyr)
library(readr)

# [TODO] v1 or v2?
f <- "data/elections-parliament-and-ep.tsv"
t <- "data/elections-parliament-and-ep-spec1.r" # data v1
d <- read_tsv(f, col_types = source(t)$value)

# ------------------------------------------------------------------------------
# Compulsory voting (CV)
# ------------------------------------------------------------------------------

# binarize compulsory voting rule
d$cv <- recode(d$cv, "No" = 0L, "Yes" = 1L, .missing = -1L)

# missing values in compulsory voting
# -----------------------------------

# filter(d, cv < 0) %>% 
#   print(n = 1000)

# - GRC 1990, ambiguous (not present in IDEA data)
# - SPA 2019, recent parliamentary election
# - all 2019 EP elections (not yet in IDEA + not in Kantar data source)

# impute missing compulsory voting values
# ---------------------------------------

# (values imputed by François Briatte and Camille Kelbel in early May 2019;
#  see `comment` column of TSV file for details)
f <- "data/compulsory-voting-additions.tsv"
t <- "data/compulsory-voting-additions-spec.r"
e <- read_tsv(f, col_types = source(t)$value) %>% 
  mutate(cv_imputed = as.integer(cv_imputed))

d <- left_join(d, e, by = c("iso3c", "election_type", "election_date")) %>% 
  mutate(cv = if_else(is.na(cv) | cv < 0, cv_imputed, cv)) %>% 
  select(-cv_imputed)

stopifnot(!is.na(d$cv))
# filter(d, !cv %in% 0:1| is.na(cv))

# ------------------------------------------------------------------------------
# Asynchronous (early) EP elections
# ------------------------------------------------------------------------------

# synchronized EP election years
e <- c(1979, 1984, 1989, 1994, 1999, 2004, 2009, 2014, 2019)

# sanity check: EP elections never coded as early in ParlGov
stopifnot(d$early[ d$election_type == "ep" ] == 0L)

# code asynchronous EP elections
# (using election years, even though year-months would be safer)
d$early[ d$election_type == "ep" & !d$year %in% e ] <- 1L

# ... `early` now codes 1 for *both* national and EP early elections

# Austria 1996, Bulgaria 2007, Spain 1987, ... n = 9
filter(d, election_type == "ep", early == 1L)

# ------------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------------

write_tsv(d, "data/elections-parliament-and-ep.tsv")

# have a nice day
