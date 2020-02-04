# ------------------------------------------------------------------------------
# Add LNE measures to `d` [TODO] document
# -> Output: data/elections-parliament-and-ep.tsv
#
# Package dependencies
# ------------------------------------------------------------------------------

library(dplyr)

# ------------------------------------------------------------------------------
# Turnout at last national (parliamentary) election
#
# turnout at last national election before EP election
#
# lne_voter_turnout,
# lne_voter_turnout_source,
# lne_election_type,
# lne_election_date,
#
# ------------------------------------------------------------------------------

d <- arrange(d, iso3c, election_date) %>% 
  mutate(
    lne_voter_turnout = voter_turnout %>% 
      if_else(election_type == "ep", NA_real_, .),
    lne_voter_turnout_source = voter_turnout_source %>% 
      if_else(election_type == "ep", NA_character_, .),
    lne_election_type = election_type %>% 
      if_else(election_type == "ep", NA_character_, .),
    lne_election_date = as.character(election_date) %>% 
      if_else(election_type == "ep", NA_character_, .)
  ) %>% 
  group_by(iso3c) %>% 
  tidyr::fill(starts_with("lne_"), .direction = "down") %>% 
  mutate(
    lne_voter_turnout = lne_voter_turnout %>% 
      if_else(election_type == "ep", ., NA_real_),
    lne_voter_turnout_source = lne_voter_turnout_source %>% 
      if_else(election_type == "ep", ., NA_character_),
    lne_election_type = lne_election_type %>% 
      if_else(election_type == "ep", ., NA_character_),
    lne_election_date = as.character(election_date) %>% 
      if_else(election_type == "ep", ., NA_character_)
  )

# export
write_tsv(d, "data/elections-parliament-and-ep.tsv")

# have a nice day
