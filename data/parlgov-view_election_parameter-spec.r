cols(
  election_id = col_integer(),
  turnout = col_double(),
  enp_votes = col_double(),
  enp_seats = col_double(),
  disproportionality = col_double(),
  advantage_ratio = col_double(),
  polarization_vote = col_double(),
  polarization_seats = col_double(),
  calculation_date = col_datetime(format = "")
)
