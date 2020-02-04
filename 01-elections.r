# ------------------------------------------------------------------------------
# Assemble ParlGov and IDEA data on national (parliamentary) and EP elections.
# -> Output: data/elections-parliament-and-ep.tsv
#
# Package dependencies
# ------------------------------------------------------------------------------

library(countrycode)
library(dplyr)
library(readr)
library(readxl)

# country sample
f <- "data/country-sample.tsv"
t <- "data/country-sample-spec.r"
x <- read_tsv(f, col_types = source(t)$value)

# ------------------------------------------------------------------------------
# [1] Main ParlGov election data
#
# http://www.parlgov.org/data/table/
# http://www.parlgov.org/data/table/view_election/
# http://www.parlgov.org/data/table/viewcalc_election_parameter/
#
# ------------------------------------------------------------------------------

f <- "data/parlgov-view_election.csv"
t <- "data/parlgov-view_election-spec.r"

if (!file.exists(f)) {
  u <- "http://www.parlgov.org/static/data/development-utf-8/view_election.csv"
  download.file(u, f, mode = "wb")
}

d <- read_csv(f, col_types = source(t)$value) %>% 
  rename(iso3c = country_name_short)

# sanity check: ISO-3C codes match
stopifnot(x$iso3c %in% d$iso3c)

# ------------------------------------------------------------------------------
# Effective number of parties
# (based on votes, i.e. on elected parties; Laakso/Taagepera 1979)
# ------------------------------------------------------------------------------

# dividing vote shares in % by 1,000, as Gallagher does, in order to check our
# results against those of ParlGov, which uses the Ghallager formula, / 10,000
d <- group_by(d, iso3c, election_id) %>% 
  mutate(
    enp_votes = (1 / sum(vote_share^2 / 10^4, na.rm = TRUE)) %>% 
      if_else(is.infinite(.), NA_real_, .) %>% # non-elections w/o vote shares
      round(2)
  )

# check against ParlGov
f <- "data/parlgov-view_election_parameter.csv"
t <- "data/parlgov-view_election_parameter-spec.r"

if (!file.exists(f)) {
  u <- "http://www.parlgov.org/static/data/development-utf-8/viewcalc_election_parameter.csv"
  download.file(u, f, mode = "wb")
}

# [NOTE] this table has a `turnout` column that is strictly identical to the
#        one that we compute ourselves later on (100 * votes_cast / electorate)
e <- read_csv(f, col_types = source(t)$value) %>% 
  select(election_id, enp_votes)

# sanity check: election IDs match for elections prior or up to EP 2019
stopifnot(
  filter(d, election_date < "2019-06-01")$election_id %in% e$election_id
)

# some missing values (very recent elections)
filter(d, !election_id %in% e$election_id) %>% 
  select(iso3c, election_id, election_type, election_date) %>% 
  distinct # CAN 2019-10, ESP 2019-11 (both parliamentary elections)

# add ParlGov ENP measures (enp_votes.y)
e <- left_join(d, e, by = "election_id") %>% 
  mutate(
    no_parlgov = is.na(enp_votes.y),
    delta = abs(enp_votes.x - enp_votes.y)
  )


# [NOTE] our ENP measures are slightly different from ParlGov's because ParlGov
#        does not take parties of unidentified party families; see note at [1]
#
# [1] http://www.parlgov.org/data/table/viewcalc_election_parameter/
#
range(round(e$delta, 4), na.rm = TRUE)
filter(e, no_parlgov | delta > 0.01) %>% 
  select(iso3c, starts_with("election_"), starts_with("enp_v"), delta) %>% 
  distinct

# ------------------------------------------------------------------------------
# Competitiveness
# (difference of vote shares between two most successful parties)
# ------------------------------------------------------------------------------

# largest party vote share minus second largest
e <- group_by(d, country_id, election_id) %>% 
  # remove parties with no vote shares, otherwise min_rank might fail
  filter(!is.na(vote_share)) %>% 
  # party rank, 1 = largest vote share
  mutate(rank = n() - min_rank(vote_share) + 1) %>%
  # keep only two largest parties/vote shares
  filter(rank < 3) %>% 
  # competitiveness of election, in vote share percentage points
  summarise(comp12 = round(max(vote_share) - min(vote_share), 2))

d <- left_join(d, e, c("country_id", "election_id"))

# sanity checks: all values in [0, 100) or missing (see [NOTE] ParlGov issues)
stopifnot(round(d$comp12) %in% 0:100 | is.na(d$comp12))

d <- filter(d, iso3c %in% x$iso3c[ x$sample == 1 ]) %>% 
  select(
    iso3c,
    country_id,
    election_id,
    election_type,
    election_date,
    comp12,
    enp_votes,
    previous_parliament_election_id
  ) %>% 
  distinct # drop duplicate rows (parties -> countries)

# ------------------------------------------------------------------------------
# Correct competitiveness for Belgium
# ------------------------------------------------------------------------------

d$comp12[ d$iso3c == "BEL" ] <- NA_real_

# Belgium has two different electoral colleges, so some of the parties assumed
# to be in competition are actually not; we use corrected estimates of comp12,
# computed manually by Camille Kelbel in early May 2019
#
# [TODO] add corrected score for 2019 EP election
#
f <- "data/electoral-competitiveness-corrected.tsv"
t <- "data/electoral-competitiveness-corrected-spec.r"

e <- read_tsv(f, col_types = source(t)$value)

d <- left_join(d, e, by = c("iso3c", "election_date", "election_type")) %>%
  mutate(comp12 = if_else(is.na(comp12_corrected), comp12, comp12_corrected)) %>%
  select(-comp12_corrected)

# ------------------------------------------------------------------------------
# Restrict dataset to elections just before, or after, first EP election
# ------------------------------------------------------------------------------

# select only elections from first national election before first EP election
d <- group_by(d, iso3c) %>% 
  arrange(election_date) %>% 
  mutate(n = row_number(), n_first_ep = min(n[ election_type == "ep" ])) %>% 
  filter(n >= n_first_ep - 1) %>%
  select(-n, -n_first_ep)

# ------------------------------------------------------------------------------
# [2] Additional ParlGov data
#
# http://www.parlgov.org/data/table/
# http://www.parlgov.org/data/table/election/
#
# ------------------------------------------------------------------------------

f <- "data/parlgov-election.csv"
t <- "data/parlgov-election-spec.r"

if (!file.exists(f)) {
  u <- "http://www.parlgov.org/static/data/development-utf-8/election.csv"
  download.file(u, f, mode = "wb")
}

e <- read_csv(f, col_types = source(t)$value) %>% 
  select(
    country_id,
    election_id = id,
    election_date = date,
    early,
    electorate,
    votes_cast,
    votes_valid,
    data_source,
    description,
    comment
  )

# merge selected elections to parliamentary election data
e <- left_join(d, e, by = c("country_id", "election_id", "election_date"))

# sanity check: all countries, elections and election dates matched
stopifnot(nrow(e) == nrow(d))

# compute voter turnout rate
e$voter_turnout <- round(100 * e$votes_cast / e$electorate, 2)

# sanity check: all values in (0, 100) or missing (see [NOTE] ParlGov issues)
stopifnot(round(e$voter_turnout) %in% 0:100 | is.na(e$voter_turnout))

# [NOTE] ParlGov issues:
# ----------------------

# (1) non-elections: pre-EP election composition appear as EP elections
#     (e.g. Austria 1994/1995, Spain 1985/1986)
#
#     ... which means that sometimes, we get "two EP elections" instead of one
#
# mutate(e, year = substr(election_date, 1, 4)) %>%
#   filter(election_type == "ep") %>%
#   group_by(iso3c, year) %>%
#   tally %>%
#   filter(n > 1)
#
# (some national elections also happen twice in the same year, but that's
#  actually normal, e.g. snap elections after government dissolves parliament)
#
# (2) a few turnout rates in recent national elections are missing
#     (e.g. GBR 2015, IRL 2016)
# 
# filter(e, is.na(voter_turnout)) %>%
#   select(iso3c, election_type, election_date, description, comment) %>%
#   View
#
# (3) ParlGov does not include voting-age-population (VAP) turnout
#
# -> we solve those issues below by using IDEA data

# ------------------------------------------------------------------------------
# [3] IDEA turnout rates
#
# https://www.idea.int/data-tools/data/voter-turnout
# https://www.idea.int/data-tools/continent-view/Europe/40
# (manually downloaded, couldn't bother scraping the JSON from the JavaScript)
#
# ------------------------------------------------------------------------------

f <- "data/idea-voter-turnout-all-elections.xls"

d <- read_excel(f) %>% 
  filter(Country %in% x$idea_name) %>% 
  select(
    Country,
    election_type = `Election type`,
    year = Year,
    voter_turnout = `Voter Turnout`,
    vap_turnout = `VAP Turnout`,
    cv = `Compulsory voting`
  )

# convert country names to ISO-3C
d <- mutate(d, iso3c = countrycode(Country, "country.name", "iso3c")) %>% 
  select(iso3c, everything(), -Country)

# sanity check: all countries matched
stopifnot(d$iso3c %in% e$iso3c)

# convert turnout rates to proper fractions
d$voter_turnout <- as.numeric(gsub("\\s%", "", d$voter_turnout))
d$vap_turnout   <- as.numeric(gsub("\\s%", "", d$vap_turnout))

# recode IDEA election types to match ParlGov ones
d$election_type <- substr(d$election_type, 1, 2) %>% 
  recode(
    "EU" = "ep",
    "Pa" = "parliament",
    "Pr" = "president", # [NOTE] not covered by ParlGov
    .default = NA_character_
  )

# ------------------------------------------------------------------------------
# [4] Merge ParlGov and IDEA
# ------------------------------------------------------------------------------

nrow(d) # IDEA
nrow(e) # ParlGov

# IDEA does not provide exact election dates, so we match on election years
# ParlGov election year (as.numeric because IDEA election year is <dbl>)
e$year <- as.numeric(substr(e$election_date, 1, 4))

# [NOTE] some ParlGov elections are not matched by IDEA ones
# ----------------------------------------------------------

# anti_join(e, d, by = c("iso3c", "election_type", "year")) %>%
#   select(iso3c, country_id, election_id, election_type, election_date) %>%
#   print(n = 1000)

# - 4 x 'initial composition' ParlGov "elections" (see [NOTE] ParlGov issue 1)
#   ... all dated January 1st -- so we're good
# - 1 x strange case (Greece 1990, early election, absent from IDEA)
#   ... that case is harder to understand

# left-join on ParlGov (e), excluding all presidential elections and all 
# out-of-time-range elections from IDEA (d)
d <- left_join(e, d, by = c("iso3c", "election_type", "year")) %>% 
  arrange(iso3c, year)

# sanity check: new data (d) has same number of rows as ParlGov alone (e)
stopifnot(nrow(d) == nrow(e))

# compare ParlGov (self-computed) voter turnout (.x) to IDEA rates (.y)
# ---------------------------------------------------------------------

mutate(d, delta = abs(voter_turnout.x - voter_turnout.y)) %>%
  filter(delta >= 2.5) %>% # threshold: 2.5 percentage points
  arrange(-delta) %>%
  select(iso3c, year, election_type, election_id, contains("voter_"), delta) %>%
  print(n = 1000)

# 17 obs. out of 415 (4% of total)

# ... [a] use alternative sources for triangulation
#
# EED: European Election Database (NSD)
#      https://nsd.no/european_election_database
#
# Pse: Psephos Election Archive (Adam Carr)
#      http://psephos.adam-carr.net/
#
# IPU: Inter-Parliamentary Union (PARLINE)
#      https://data.ipu.org/
#
# (closer = closer to alternative data source)
#
# FRA 1978 (pa) : ParlGov 83.2, IDEA 71.6, [note  1] -> closer: ParlGov
# BGR 2017 (pa) : ParlGov 42.7, IDEA 53.8, [note  2] -> closer: IDEA
# HRV 2011 (pa) : ParlGov 61.9, IDEA 54.2, Pse 56.5  -> closer: IDEA
# GRC 2015 (pa) : ParlGov 56.6, IDEA 63.9, [note  3] -> closer: ParlGov
# FRA 2017 (pa) : ParlGov 48.7, IDEA 42.6, [note  4] -> closer: ParlGov
# ESP 19Ap (pa) : ParlGov 66.2, IDEA 71.8, [note  5] -> closer: ?
# ITA 2008 (pa) : ParlGov 75.5, IDEA 80.5, [note  6] -> closer: ?
# DNK 1981 (pa) : ParlGov 83.2, IDEA 87.8, [note  7] -> closer: ParlGov
# GRC 89Ju (pa) : ParlGov 80.3, IDEA 84.5, [note  8] -> closer: ParlGov
# FIN 2019 (pa) : ParlGov 72.8, IDEA 68.7, [note  9] -> closer: ParlGov
# FRA 2002 (pa) : ParlGov 64.4, IDEA 60.3, [note 10] -> closer: ParlGov
# ESP 19No (pa) : ParlGov 75.8, IDEA 71.8, Pse 75.8  -> closer: ParlGov
# GRC 89No (pa) : ParlGov 80.7, IDEA 84.5, [note 11] -> closer: ParlGov
# GRC 1993 (pa) : ParlGov 79.2, IDEA 83.0, EED 79.2  -> closer: ParlGov
# GRC 1985 (pa) : ParlGov 80.2, IDEA 83.8, [note 12] -> closer: ParlGov
# ESP 2019 (EP) : ParlGov 60.7, IDEA 64.3, Pse ########1085
# GRC 2012 (pa) : ParlGov 65.1, IDEA 62.5, [note 13] -> closer: IDEA
#
# Notes:
#
# [1] no data in either EED or Psephos; using (unsourced) WP-FR figures:
#     1st round: reg. 34402883, valid = reg. - abst. 5729750 -> turnout = 83.3
#     (figure unlikely to include overseas territ., which CDSP did not record)
#     https://www.data.gouv.fr/fr/datasets/elections-legislatives-1958-2012/
#
# [2] Psephos (citing specialized + media websites) reports turnout = 53.9
#     IPU reports turnout = 54.07
#
# [3] both Psephos (citing Ministry of Interior) and IPU report turnout = 56.6
#
# [4] both Psephos (citing Ministry of Interior) and IPU report turnout = 48.7
#
# [5] there were two Spanish parliamentary elections in 2019, and IDEA did not
#     record the April one, which makes its figure for that one false
#
# [6] EED erroneously reports 100% turnout on this election, for which
#     the Italian Ministry of Interior did not publish the electorate size;
#     Psephos uses the same source and does not report turnout either
#
# [7] no data in either EED or Psephos; WP-EN cites Nohlen & Stöver 2010, 545:
#     national turnout, taking Greeland and Faroe Islands into account = 82.8
#     (ParlGov figure is apprently metropolitan Denmark only)
#
# [8] 1989 June election; no data in either EED or Psephos
#     WP-EN cites Nohlen & Stöver 2010, 830: turnout = 80.3
#
# [9] no data in either EED or Psephos; WP-EN cites Ministry of Justice:
#     turnout = 72.4 -- IPU reports turnout = 72.14
#
# [10] both EED (citing email from Ministry of Interior official) and Psephos
#      (citing National Assembly website) report turnout = 64.4
#
# [11] 1989 Nov. election; no data in either EED or Psephos
#      WP-EN cites Nohlen & Stöver 2010, 830: turnout = 80.7
#
# [12] no data in either EED or Psephos;
#      WP-EN cites Nohlen & Stöver 2010, 830: turnout = 80.2
#
# [13] Psephos (citing Ministry of Interior) reports turnout = 62.5 (= IDEA);
#      EED listed source (Ministry of Interior) also reports turnout = 62.5; it
#      seemed to report 65.1 (= ParlGov), but I cannot locate figure any more...
#
# [TEMP] extra mistaken figure in ParlGov, in wait for fix
# --------------------------------------------------------
#
# UK/GBR 2004 (EP):    ParlGov 38.5, IDEA 39.2, EED 39.2 -> IDEA closer
# (ParlGov is reporting turnout based on valid votes only)
# (Stockemer 2012 reports 38.5)
# (Psephos and IPU do not report EP elections)
#
# ... [b] use ParlGov with as little input from IDEA as possible
# --------------------------------------------------------------

# [a] discard 'initial EP composition' ParlGov "elections"
filter(d, is.na(voter_turnout.x), grepl("-01-01$", election_date)) %>% 
  nrow # n = 8

# drop 8 non-election cases
d <- filter(d, !is.na(voter_turnout.x) | !grepl("-01-01$", election_date))

# [b] use IDEA turnout rates when ParlGov ones are missing
filter(d, is.na(voter_turnout.x), !is.na(voter_turnout.y)) %>% 
  nrow # n = 3

# use IDEA rates in 3 (all national) elections
e <- is.na(d$voter_turnout.x)
d$voter_turnout.x[ e ] <- d$voter_turnout.y[ e ]

# acknowledge different sources
d$voter_turnout_source <- if_else(e, "idea", "parlgov")

# ------------------------------------------------------------------------------
# [5] Finalize and export
# ------------------------------------------------------------------------------

d <- arrange(d, iso3c, year, election_date) %>% 
  select(
    iso3c,
    year,
    #
    # election attributes
    #
    election_date,
    election_type,
    early,
    cv,
    #
    # turnout rates
    #
    vap_turnout,                     # IDEA exclusively
    voter_turnout = voter_turnout.x, # ParlGov, with a few IDEA values
    voter_turnout_source,
    #
    # party-based (vote share) measures
    #
    comp12,
    enp_votes,
    #
    # ParlGov identifiers
    election_id,
    previous_parliament_election_id
  )

# export
write_tsv(d, "data/elections-parliament-and-ep.tsv")

# have a nice day
