# ------------------------------------------------------------------------------
# Compute time to last national election and other temporal measurements
# -> Output: data/elections-ep-only.tsv
#
# [TOFIX] - ugly min()/if_else() interaction because of type-unsafety
# [TOFIX] - whether to use tt_last_ne or tt_last_ne2
#
# Package dependencies
# ------------------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(readr)

f <- "data/elections-all.tsv"
t <- "data/elections-all-spec.r"
d <- read_tsv(f, col_types = source(t)$value)

# ------------------------------------------------------------------------------
# recode election types
# ------------------------------------------------------------------------------

d$election_of <- tolower(d$election_of)
d$other_vote1 <- tolower(d$other_vote1)
d$other_vote2 <- tolower(d$other_vote2)

x <- "assembly|chamber|congress|council|diet|house|parliament|sejm|senate"
d$election_of[ grepl(x, d$election_of) ] <- "parliament"
d$other_vote1[ grepl(x, d$other_vote1) ] <- "parliament"
d$other_vote2[ grepl(x, d$other_vote2) ] <- "parliament"
# table(d$election_of) %>% print
# table(d$other_vote1) %>% print
# table(d$other_vote2) %>% print

# [IMPORTANT] EP should always be last, i.e. highest ordered level
x <- c("parliament", "president", "referendum", "ep")

# sanity check: all elections of known type
stopifnot(d$election_of %in% x)

# [IMPORTANT] EP ordered last to always be last in sorted row series
d$election_of <- factor(d$election_of, levels = x, ordered = TRUE)

# ------------------------------------------------------------------------------
# measure temporal predictors
#
# - time elapsed since last national election
# - EP election concomitant to national election
# - number of national elections before EP election
# ------------------------------------------------------------------------------

# [IMPORTANT] sort to move EP elections last in their series
d <- arrange(d, iso3c, date, election_of) %>% 
  group_by(iso3c) %>% 
  mutate(
    ep_date = if_else(election_of == "ep", date, as.Date(NA))
  ) %>% 
  tidyr::fill(ep_date, .direction = "up") %>% # na.locf
  group_by(iso3c, ep_date) %>% # 'country + EP election' dyads
  mutate(
    #
    # days to next EP election
    #
    tt_ep = as.integer(ep_date - date),
    #
    # exclusions rules: national elections that are
    #
    #   - too old (5+ years before first EP election)
    #   - too recent (no EP election afterwards)
    #
    exclude = tt_ep >= 5 * 365 | is.na(ep_date),
    #
    # time elapsed since last national election (in days)
    #
    # if elections before EP, find the closest one, and coerce to integer
    # because of a weird type conflict when min() and if_else() interact
    tt_last_ne2 = if_else(
      all(!exclude) & any(tt_ep > 0) & any(election_of != "ep"),
      as.integer(min(tt_ep[ tt_ep > 0 & election_of != "ep" ])),
      NA_integer_
    ) %>%
      if_else(election_of == "ep", ., NA_integer_),
    #
    # number of national elections before EP election (count)
    #
    ne_before = as.integer(n_distinct(date[ date < ep_date ])) %>% 
      if_else(election_of == "ep", ., NA_integer_),
    #
    # EP election concomitant to national election (0/1)
    #
    ne_concomitant = as.integer(sum(tt_ep == 0) > 1) %>% 
      if_else(election_of == "ep", ., NA_integer_)
    #
  )

# ------------------------------------------------------------------------------
# CHECK time to last national election
# ------------------------------------------------------------------------------

## [TOFIX] some stray values, see below
# stopifnot(!is.na(d$tt_last_ne2))

e <- left_join(
  filter(d, election_of == "ep"),
  read_tsv("data/ttl-days.tsv", col_types = "cDdccdd"),
  by = c("iso3c", "ep_date")
)

x <- filter(e, is.na(tt_last_ne) | is.na(tt_last_ne2))
if (nrow(x) > 0) {
  cat("\n[TOFIX] missing tt_last_ne or tt_last_ne2\n")
  select(x, iso3c, ep_date, tt_last_ne, tt_last_ne2) %>%
    print # tt_last_ne = manual coding, tt_last_ne2 = computed
  warning("fixes needed")
}

# missing values in computed version are those where last national election was
# organised before the previous EP election

x <- filter(e, tt_last_ne != tt_last_ne2)
if (nrow(x) > 0) {
  cat("\n[TOFIX] different tt_last_ne2\n")
  select(x, iso3c, ep_date, tt_last_ne, tt_last_ne2) %>%
    print
  warning("fixes needed")
}

e <- select(e, iso3c, ep_date, tt_last_ne)
d <- left_join(d, e, by = c("iso3c", "ep_date"))

# ------------------------------------------------------------------------------
# CHECK duration of elections (single-day or multiple-days)
# ------------------------------------------------------------------------------

d$multiple_days2 <- as.integer(!is.na(d$date_start))
stopifnot(!is.na(d$multiple_days2))

e <- left_join(
  filter(d, election_of == "ep"),
  read_tsv("data/ttl-days.tsv", col_types = "cDdccdd"),
  by = c("iso3c", "ep_date")
)

x <- filter(e, is.na(multiple_days) | is.na(multiple_days2))
if (nrow(x) > 0) {
  cat("\n[TOFIX] missing multiple_days or multiple_days2\n")
  select(x, iso3c, ep_date, multiple_days, multiple_days2) %>%
    print # multiple_days = manual coding, multiple_days2 = computed
  warning("fixes needed")
}

x <- filter(e, multiple_days != multiple_days2)
if (nrow(x) > 0) {
  cat("\n[TOFIX] different multiple_days2\n")
  select(x, iso3c, ep_date, date_start, multiple_days, multiple_days2) %>%
    print
  warning("fixes needed")
}

# ------------------------------------------------------------------------------
# [DRAFT] EP election organised during weekdays
# ------------------------------------------------------------------------------

is_weekday <- function(x) {
  if_else(is.na(x), FALSE, !weekdays(x) %in% "Sunday")
}

d$includes_weekdays2 <- as.integer(is_weekday(d$date) | is_weekday(d$date_start))
stopifnot(!is.na(d$includes_weekdays2))

e <- left_join(
  filter(d, election_of == "ep"),
  read_tsv("data/ttl-days.tsv", col_types = "cDdccdd"),
  by = c("iso3c", "ep_date")
)

x <- filter(e, is.na(includes_weekdays) | is.na(includes_weekdays2))
if (nrow(x) > 0) {
  cat("\n[TOFIX] missing includes_weekdays or includes_weekdays2\n")
  select(x, iso3c, ep_date, includes_weekdays, includes_weekdays2) %>%
    print # includes_weekdays = manual coding, includes_weekdays2 = computed
  warning("fixes needed")
}

x <- filter(e, includes_weekdays != includes_weekdays2)
if (nrow(x) > 0) {
  cat("\n[TOFIX] different includes_weekdays2\n")
  select(x, iso3c, ep_date, date_start, includes_weekdays, includes_weekdays2) %>%
    print
  warning("fixes needed")
}

# ------------------------------------------------------------------------------
# merge to ParlGov-derived election data
# ------------------------------------------------------------------------------

# limit to EP elections
d <- filter(d, election_of == "ep") %>% 
  select(
    # static information, manually collected (data/elections-all.tsv)
    iso3c, date, date_start, other_vote1, other_vote2,
    # temporal measurements (manually or automatically computed)
    tt_last_ne, tt_last_ne2,
    ne_before, ne_concomitant,
    multiple_days2, includes_weekdays2
  )

# load ParlGov-derived data
e <- read_tsv("data/elections-parliament-and-ep.tsv") %>% 
  filter(election_type == "ep") %>% 
  rename(date = election_date) # WARNING; see [NOTE] below...

# [NOTE] ParlGov EP election dates are sometimes wrong
# ----------------------------------------------------

cat("\n[TOFIX] different EP dates in ParlGov\n")
anti_join(d, e, by = c("date", "iso3c")) %>% 
  arrange(date) %>% 
  print

# we have to merge on election years
d$year <- year(d$ep_date)

# isolate ParlGov EP dates
e$pg_date <- e$date

# merge WITHOUT ParlGov EP dates
d <- full_join(d, select(e, -date), by = c("iso3c", "year"))

# sanity check: same number of EP elections in both datasets
stopifnot(nrow(d) == nrow(e))

# export differences betwen our data and ParlGov
filter(d, date != pg_date) %>% 
  ungroup %>% 
  arrange(iso3c, date) %>% 
  select(iso3c, date, date_start, pg_date) %>% 
  write_tsv("ep_date_differences.tsv")

# export EP elections dataset

ungroup(d) %>% 
  # country-year (EP election year) identifiers
  select(iso3c, year, everything(), -ep_date) %>% 
  # remove ParlGov dates and other unused ParlGov information
  select(-pg_date, election_id, -election_type) %>% 
  # unused manually-coded information
  select(-date_start, -other_vote1, -other_vote2) %>%
  write_tsv("data/elections-ep-only.tsv")
