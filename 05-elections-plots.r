library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

d <- read_tsv("data/elections-parliament-and-ep.tsv") %>% 
  group_by(iso3c, election_type) %>% 
  mutate(
    cv = recode(cv, `1` = "Y", .default = "N", .missing = "NA") %>% 
      factor(levels = c("Y", "N", "NA")),
    # change in voter turnout
    voter_turnout_L1 = dplyr::lag(voter_turnout, 1),
    voter_turnout_D1 = voter_turnout - voter_turnout_L1,
    # for plots showing only main EP election years
    year = as.integer(substr(election_date, 1, 4))
  ) %>% 
  ungroup

# d$voter_turnout_ratio <- 

# synchronized EP elections
e <- c(1979, 1984, 1989, 1994, 1999, 2004, 2009, 2014, 2019) %>% 
  paste0(., "-01-01") %>% 
  as.Date

# ------------------------------------------------------------------------------
# plot themes
# ------------------------------------------------------------------------------

t1 <- theme(
  panel.background = element_rect(fill = "grey95"),
  panel.grid.minor = element_blank(),
  legend.justification = c(1, 0),
  legend.position = c(1, 0),
  legend.direction = "horizontal"
)

# for histograms
t2 <- theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank(),
  panel.grid = element_blank()
)

t3 <- t1 + theme(legend.justification = "center", legend.position = "bottom")

t4 <- t1 + theme(panel.grid = element_blank())

# ------------------------------------------------------------------------------
# turnout rates
# ------------------------------------------------------------------------------

g <- aes(election_date, voter_turnout, color = election_type, shape = cv)
ggplot(d, g) +
  geom_point() +
  geom_line(aes(group = election_type)) +
  scale_color_brewer("Election type", palette = "Set1") +
  scale_shape_manual("CV", values = c("Y" = 21, "N" = 19, "NA" = 4)) +
  scale_x_date(breaks = e, labels = function(x) substr(x, 3, 4)) +
  # scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ iso3c, scales = "free_x") +
  labs(title = "Voter turnout (%)", y = NULL, x = "Election year") +
  t1

ggsave(
  "plots/voter-turnout.pdf",
  width = 10,
  height = 10
)

# ------------------------------------------------------------------------------
# difference (change) in turnout rates
# ------------------------------------------------------------------------------

g <- aes(election_date, voter_turnout_D1, color = election_type, shape = cv)
ggplot(d, g) +
  geom_point() +
  geom_line(aes(group = election_type)) +
  geom_hline(yintercept = 0, lty = "dotted") +
  scale_color_brewer("Election type", palette = "Set1") +
  scale_shape_manual("CV", values = c("Y" = 21, "N" = 19, "NA" = 4)) +
  scale_x_date(breaks = e, labels = function(x) substr(x, 3, 4)) +
  # scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ iso3c, scales = "free_x") +
  labs(title = "Change in voter turnout", y = NULL, x = "Election year") +
  t1

ggsave(
  "plots/voter-turnout-change.pdf",
  width = 10,
  height = 10
)

# ------------------------------------------------------------------------------
# turnout rates :: histograms
# ------------------------------------------------------------------------------

g <- aes(x = voter_turnout) # , color = election_type

# EP elections
x <- filter(d, election_type == "ep")
ggplot(x, g) +
  # by country
  geom_dotplot(method = "histodot") + # , binwidth = .05
  # 'all countries' facet
  geom_histogram(data = mutate(x, iso3c = "(EU-28)"), bins = 15) +
  facet_wrap(~ iso3c, scales = "free_y") +
  # 'all countries' shown in each facet
  geom_histogram(data = select(x, voter_turnout), alpha = 0.5, bins = 15) +
  labs(
    title = "Voter turnout (histograms): EP elections",
    y = NULL
  ) +
  t2

ggsave(
  "plots/voter-turnout-histograms-by-country-ep.pdf",
  width = 10,
  height = 9
)

# national parliamentary elections
x <- filter(d, election_type == "parliament")
ggplot(x, g) +
  # by country
  geom_dotplot(method = "histodot") + # , binwidth = .05
  # 'all countries' facet
  geom_histogram(data = mutate(x, iso3c = "(EU-28)"), bins = 15) +
  facet_wrap(~ iso3c, scales = "free_y") +
  # 'all countries' shown in each facet
  geom_histogram(data = select(x, voter_turnout), alpha = 0.5, bins = 15) +
  labs(
    title = "Voter turnout (histograms): National parliamentary elections",
    y = NULL
  ) +
  t2

ggsave(
  "plots/voter-turnout-histograms-by-country-parliament.pdf",
  width = 10,
  height = 9
)

# ------------------------------------------------------------------------------
# overall density curves
# ------------------------------------------------------------------------------

x <- ungroup(d) %>% 
  select(election_type, voter_turnout, voter_turnout_D1) %>% 
  tidyr::gather(key, value, -election_type)

ggplot(x, aes(x = value, color = election_type)) +
  geom_density() +
  geom_vline(
    data = group_by(x, election_type, key) %>% 
      summarise(mu = mean(value, na.rm = TRUE)),
    aes(xintercept = mu), lty = "dotted"
  ) +
  scale_color_brewer(palette = "Set1") +
  facet_grid(election_type ~ key, scales = "free") +
  guides(color = FALSE) +
  labs(
    title = "Density curves of voter turnout rates",
    subtitle = "Dotted lines at averages",
    y = NULL
  ) +
  t1

ggsave(
  "plots/voter-turnout-densities.pdf",
  width = 6,
  height = 6
)

# ------------------------------------------------------------------------------
# ranked boxplots
# ------------------------------------------------------------------------------

# - avoid faceting because EP and national turnout produce different rankings
# - avoid coloring by CV because CV is mixed in some countries:
g <- aes(reorder(iso3c, voter_turnout), voter_turnout)

# simplify CV at country-election level
x <- group_by(d, iso3c, election_type) %>% 
  summarise(
    n = sum(!is.na(cv)),
    p_cv = sum(as.integer(cv == "Y") / n, na.rm = TRUE)
  )

# special cases, with % of CV elections:
filter(x, p_cv > 0, p_cv < 1)
# - Bulgaria  5 parliament  % CV = 20
# - Italy     9 EP          % CV = 33
# - Italy     11 parliament % CV = 36

# mean turnout for each election type
group_by(d, election_type) %>% 
  summarise(mu = mean(voter_turnout, na.rm = TRUE))

# [1] ep
x <- filter(d, election_type == "ep")
ggplot(x, g) +
  geom_hline(
    aes(yintercept = mean(x$voter_turnout, na.rm = TRUE)),
    lty = "dotted"
  ) +
  geom_boxplot(outlier.size = 0, color = "grey25") +
  geom_point(aes(shape = cv)) +
  coord_flip() +
  scale_shape_manual(values = c("Y" = 21, "N" = 19, "NA" = 4)) +
  labs(
    x = NULL,
    y = NULL,
    title = paste(
      "Voter turnout in EP elections,",
      paste0(range(x$year), collapse = "-")
    ),
    subtitle = "Dotted line at sample average"
  ) +
  t3

ggsave(
  "plots/voter-turnout-boxplots-ep.pdf",
  width = 9,
  height = 9
)

# [2] parliaments
x <- filter(d, election_type == "parliament")
ggplot(x, g) +
  geom_hline(
    aes(yintercept = mean(x$voter_turnout, na.rm = TRUE)),
    lty = "dotted"
  ) +
  geom_boxplot(outlier.size = 0, color = "grey25") +
  geom_point(aes(shape = cv)) +
  coord_flip() +
  scale_shape_manual(values = c("Y" = 21, "N" = 19, "NA" = 4)) +
  labs(
    x = NULL,
    y = NULL,
    title = paste(
      "Voter turnout in parliamentary elections,",
      paste0(range(x$year), collapse = "-")
    ),
    subtitle = "Dotted line at sample average"
  ) +
  t3

ggsave(
  "plots/voter-turnout-boxplots-parliament.pdf",
  width = 9,
  height = 9
)

# [NOTE] not looking at boxplots for change in voter turnout,
#        makes little sense (all centered round zero, see densities)

# ------------------------------------------------------------------------------
# Effective number of parties
# ------------------------------------------------------------------------------

# EU-28 means
x <- group_by(d, year) %>%
  filter(election_type == "ep") %>%
  summarise(n = n(), enp_votes = mean(enp_votes)) %>% 
  filter(n > 2, !is.na(enp_votes)) %>% 
  mutate(iso3c = "(EU-28)")

ggplot(filter(d, election_type == "ep"), aes(year, enp_votes)) +
  # by country
  geom_line() +
  # 'all countries' facet
  geom_line(data = x, color = "steelblue") +
  facet_wrap(~ iso3c) +
  # 'all countries' shown in each facet
  geom_line(data = select(x, -iso3c), color = "steelblue", lty = "dashed") +
  scale_y_continuous() +
  scale_x_continuous(
    breaks = as.integer(substr(e, 1, 4)),
    labels = function(x) substr(x, 3, 4)
  ) +
  labs(
    title = "Effective number of elected parties in EP elections, 1979-2019",
    x = NULL,
    y = NULL
  ) +
  t4

ggsave(
  "plots/enp_votes.pdf",
  width = 11,
  height = 9
)

# ------------------------------------------------------------------------------
# Effective number of parties
# ------------------------------------------------------------------------------

# EU-28 means
x <- group_by(d, year) %>%
  filter(election_type == "ep") %>%
  summarise(n = n(), comp12 = mean(comp12)) %>% 
  filter(n > 2, !is.na(comp12)) %>% 
  mutate(iso3c = "(EU-28)")

ggplot(filter(d, election_type == "ep"), aes(year, comp12)) +
  # by country
  geom_line() +
  # 'all countries' facet
  geom_line(data = x, color = "steelblue") +
  facet_wrap(~ iso3c) +
  # 'all countries' shown in each facet
  geom_line(data = select(x, -iso3c), color = "steelblue", lty = "dashed") +
  scale_y_continuous() +
  scale_x_continuous(
    breaks = as.integer(substr(e, 1, 4)),
    labels = function(x) substr(x, 3, 4)
  ) +
  labs(
    title = "Electoral competitiveness in EP elections, 1979-2019",
    x = NULL,
    y = NULL
  ) +
  t4

ggsave(
  "plots/comp12.pdf",
  width = 11,
  height = 9
)

# have a nice day
