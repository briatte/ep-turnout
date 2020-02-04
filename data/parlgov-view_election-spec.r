cols(
  country_name_short = col_character(),
  country_name = col_character(),
  election_type = col_character(),
  election_date = col_date(format = ""),
  vote_share = col_double(),
  seats = col_double(),
  seats_total = col_double(),
  party_name_short = col_character(),
  party_name = col_character(),
  party_name_english = col_character(),
  left_right = col_double(),
  country_id = col_double(),
  election_id = col_double(),
  previous_parliament_election_id = col_double(),
  previous_cabinet_id = col_double(),
  party_id = col_double()
)