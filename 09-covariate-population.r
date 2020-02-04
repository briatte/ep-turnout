# ------------------------------------------------------------------------------
# Get population estimates
# -> Output: data-raw/wdi-sp.pop.totl.tsv
#
# Package dependencies
# ------------------------------------------------------------------------------

library(countrycode)
library(dplyr)
library(readr)
library(tidyr)
library(WDI)

# country-year dyads
d <- read_tsv("data/elections-parliament-and-ep.tsv") %>% 
  # filter(election_type == "ep") %>% 
  select(iso3c, year) %>% 
  distinct # remove any duplicated years

# ------------------------------------------------------------------------------
# Population
#
# Source: WDI (World Bank)
# https://data.worldbank.org/indicator/sp.pop.totl
# ------------------------------------------------------------------------------

f <- "data-raw/wdi-sp.pop.totl.tsv"
if (!file.exists(f)) {
  
  x <- WDI(
    indicator = "sp.pop.totl",
    country = unique(d$iso3c),
    start = min(d$year),
    end = max(d$year),
    extra = TRUE
  )
  
  x <- select(x, iso3c, year, pop = sp.pop.totl) %>% 
    mutate(iso3c = as.character(iso3c))
  
  write_tsv(x, f)
  
}

x <- read_tsv(f)

# ------------------------------------------------------------------------------
# Missing values
# ------------------------------------------------------------------------------

d <- left_join(d, x, by = c("iso3c", "year")) %>% 
  arrange(iso3c, year)

# stopifnot(!is.na(d$pop))

filter(d, is.na(pop)) %>%
  select(iso3c, year) %>% 
  arrange(year) %>%
  print # missing a few 2018 and 2019 values

# ------------------------------------------------------------------------------
# Code state size from population
# ------------------------------------------------------------------------------

# use only years with nonmissing data
x <- filter(d, year <= 2017) %>% 
  mutate(pop = pop / 10^6, log_pop = log(pop)) %>% 
  group_by(iso3c)

stopifnot(!is.na(x$pop))
stopifnot(!is.na(x$log_pop))

# plot(density(x$pop))
# plot(density(x$log_pop))

# ------------------------------------------------------------------------------
# Code small states
# ------------------------------------------------------------------------------

# [NOTE] Stockemer 2012 uses 2 million to denote small states, with reference to
#        the 2008 revision of the UN World Population Prospects
x$ms_small <- as.integer(x$iso3c %in% c("CYP", "LUX", "MLT"))

# ... yet let's try to see which states really have < 2 million ppl,

#        ... but that benchmark is not stable within countries:
#            there are 2 problematic cases, recent LVA (1->0) and SVN (0->1)
#
# ggplot(x, aes(year, log_pop)) +
#   geom_hline(yintercept = log(c(2, 3, 4)), lty = "dotted") +
#   geom_point(aes(color = pop < 2)) +
#   facet_wrap(~ iso3c)

summarise(x, small_state = sum(log_pop < log(2), na.rm = TRUE) / n()) %>% 
  filter(small_state > 0, small_state < 1)

# ... so we code ms_small1 with LVA = always 0 and SVN = always 1
x$ms_small1 <- as.integer(x$log_pop < log(2))
x$ms_small1[ x$iso3c == "LVA" ] <- 0L
x$ms_small1[ x$iso3c == "SVN" ] <- 1L

# 2.5 is also problematic, but only for a small segment of LVA years (0->1):
#
# ggplot(x, aes(year, log_pop)) +
#   geom_hline(yintercept = log(c(2, 3, 4)), lty = "dotted") +
#   geom_point(aes(color = pop < 2.5)) +
#   facet_wrap(~ iso3c)

summarise(x, small_state = sum(log_pop < log(2.5), na.rm = TRUE) / n()) %>% 
  filter(small_state > 0, small_state < 1)

# ... so we use 2.75 million, which is close enough and classifies both
#     LVA (1) and SVN (1) as small states all throughout the period:
#
# ggplot(x, aes(year, log_pop)) +
#   geom_hline(yintercept = log(c(2, 3, 4)), lty = "dotted") +
#   geom_point(aes(color = pop < 2.75)) +
#   facet_wrap(~ iso3c)

summarise(x, small_state = sum(log_pop < log(2.75), na.rm = TRUE) / n()) %>% 
  filter(small_state > 0, small_state < 1)

# ... so we code ms_small2, where LVA = always 1
x$ms_small2 <- as.integer(x$log_pop < log(2.75))

# at that threshold, all small states except LUX are Eastern ones

# summary of differences between dummies
filter(x, ms_small1 != ms_small2) %>% 
  select(iso3c, starts_with("ms_")) %>% 
  distinct

d <- left_join(d, select(x, -pop), by = c("iso3c", "year")) %>% 
  arrange(iso3c, year) %>% 
  group_by(iso3c) %>% 
  # impute state size from nonmissing years
  tidyr::fill(starts_with("ms_"), .direction = "down")

# ------------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------------

write_tsv(d, "data-raw/covariate-population.tsv")

# have a nice day
