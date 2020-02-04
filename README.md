[François Briatte](https://espol-lille.eu/en/speaker/francois-briatte/), 
[Camille Kelbel](https://espol-lille.eu/en/speaker/camille-kelbel/), 
[Julien Navarro](https://espol-lille.eu/en/speaker/julien-navarro/), 
[Giulia Sandri](https://espol-lille.eu/en/speaker/giulia-sandri/) and 
[Felix von Nostitz](https://espol-lille.eu/en/speaker/felix-christopher-von-nostitz/), 
"Timing and Results of European Parliament Elections, 1979-2019", <DOI>, 2019.

This research was carried out as part of the [RECONNECT project](https://reconnect-europe.eu/), which has received funding from the European Union's Horizon 2020 research and innovation programme under grant agreement No 770142.

More details:

- CODEBOOK - variable definitions
- DATA - contents of the `data` folder
- HOWTO - what the (R) code does
- TODO - current issues

Please report issues [via GitHub](./issues) or [to François Briatte](mailto:f.briatte@gmail.com).

# CODEBOOK

- `iso3c` - country code
- `year` - year of EP election
- `ms_recent` - new Member-State (2004+), fixed per country
- `ms_eastern` - Eastern Member-State
- `tt_last_ne` - time to last national election, in days (see note)
- `tt_last_ne2` - time to last national election, in days, version 2 (see note #1)
- `ne_before` - number of intermediary national elections since last EP election
- `ne_concomitant` - concomitant national election
- `multiple_days2` - whether EP election was held over more than one voting day
- `includes_weekdays2` - whether EP election was held on at least one weekday
- `early` - whether EP election was organised early
- `cv` - whether compulsory voting was enforced
- `vap_turnout` - voting-age population turnout (see note #2)
- `voter_turnout` - EP election voter turnout (see note #2)
- `lne_voter_turnout` - voter turnout at last national parliamentary election
- `comp12` - competitiveness (difference between first two parties in vote share)
- `enp_votes` - effective number of parties, based on votes shares
- `n_lists` - number of lists competing in EP election
- `ms_small` - small-population Member State (see note #3)
- `ms_small1` - small-population Member State (alternative coding; see note #3)
- `ms_small2` - small-population Member State (alternative coding; see note #3)
- `stf_dem_n` - number of EB survey samples used to measure the two next variables.
- `stf_dem_m` - average satisfaction with democracy, estimated from Eurobarometer samples from the EP election year and from the 4 years before. Germany is an aggregate of Eastern and Western samples. Britain is Great Britain only, excluding Northern Ireland. The data were _not_ [weighted](https://www.gesis.org/eurobarometer-data-service/survey-series/standard-special-eb/weighting-overview/) prior to pooling.
- `stf_dem_p` - same as above, but as a percentage of 'fairly' or 'very' satisfied respondents.
- `gdpc` - GDP per capita in EP election year (see note #4)
- `unempl` - unemployment rate in EP election year
- `gini_disp` - Gini coefficient in EP election year (see note #4)

Notes:

1. `tt_last_ne` is the manually coded version, `tt_last_ne2` is the automatically-computed version, which still has some missing observations and should thus be ignored for now.

2. Voter turnout figures (`voter_turnout`) for 2019 are provisional, and voting-age population turnout (`vap_turnout`) is entirely missing for 2019.

3. Coded exactly as in Stockemer (2012): Cyprus, Luxembourg and Malta are coded as small. Alternative codings are based on higher population threshold: `ms_small1` also includes Estonia and Slovenia, and `ms_small2` also includes Estonia, Latvia and Slovenia.

4. Values of `gdpc` for 2019 are predicted based on ARIMA models. Values of `gini_disp` for 2019 and a few early years are predicted based on generalized additive models.
# DATA

Note -- some of the datasets listed below are provided with a 'spec' file that contains R code to define their [column specification](https://readr.tidyverse.org/reference/cols.html).

- `country_sample.tsv` lists the countries under examination. Includes the country codes required to match the countries across all data sources used in the project.

- `electoral-competitiveness-corrected.tsv` holds corrected competitiveness scores for EP elections held in Belgium. See `01-elections.r` for details. Calculations by Camille Kelbel.

- `parlgov-election.csv` is the `election` table found in the development version of ParlGov as of January 16, 2020.

- `parlgov-view_election.csv` is the `view_election` table found in the development version of ParlGov as of January 16, 2020.

- `parlgov-view_election_parameter.csv` is the `view_election_parameter` table found in the development version of ParlGov as of January 16, 2020.

# HOWTO

- `01-elections.r` aggregates various electoral measures provided by [ParlGov](http://www.parlgov.org/). It also corrects some of those measures, and contains some comparisons between ParlGov and other sources of electoral measures ([IDEA](https://www.idea.int/), [IPU](https://www.ipu.org/) and [Psephos](http://psephos.adam-carr.net/)).
