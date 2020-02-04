# ------------------------------------------------------------------------------
# Process Eurobarometer estimates of 'satisfaction with [domestic] democracy'
# -> Output: data-raw/eb-satisfaction-aggregates.tsv
#
# Package dependencies
# ------------------------------------------------------------------------------

library(countrycode)
library(dplyr)
library(ggplot2)
library(readxl)
library(readr)
library(tidyr)

# ------------------------------------------------------------------------------
# Eurobarometer 'satisfaction with [domestic] democracy' trend data
#
# Source: European Commission, DG Communication website
# http://ec.europa.eu/commfrontoffice/publicopinion/index.cfm/Chart/getChart/chartType/gridChart//themeKy/45/groupKy/226/savFile/10000
#
# Notes:
#
# - We tried reconstituting the series ourselves using GESIS data (N = 1363),
#   but the weighting schemes in old EB waves are such a mess that we
#   eventually went for the lazy option of downloading EC figures (N = 1102)
#
# - Checking the figures against those in the ZA3521 'Mannheim trend file'
#   shows that the figures *are* weighted, although the exact weighting scheme
#   is not perfectly reproducible with Stata `[aw = wsample]`
#
# - FWIW, average unweighted satisfaction over the entire sample was 0.5418 in
#   our reconstituted series, and is 0.5411 in this one; weighting EB data
#   seems to make very little difference, and average satisfaction vs. fraction
#   of very/fairly satisfied correlates at 0.99 -- so we're fine all the way
#
# ------------------------------------------------------------------------------

# used for country subsetting
e <- read_tsv("data/elections-ep-only.tsv") %>% 
  select(iso3c, year, date)

d <- read_excel("data-raw/eb-satisfaction-with-democracy.xlsx") %>% 
  filter(!Country %in% c("European Union", "Turkish Cypriot Community")) %>% 
  mutate(
    iso3c = countrycode(Country, "country.name", "iso3c"),
    date = as.Date(Date),
    year = as.integer(substr(date, 1, 4)),
    total_positive = `Very satisfied` + `Fairly satisfied`,
    total_negative = `Not very satisfied` + `Not at all satisfied`,
    total_nonmissing = total_positive + total_negative,
    nonresponse_rate = 1 - total_nonmissing,
    nonresponse_rate_factored = factor(nonresponse_rate %/% 0.05)
  ) %>% 
  filter(iso3c %in% unique(e$iso3c)) %>% 
  select(iso3c, date, year, starts_with("total"), starts_with("nonresponse"))

range(d$year) # 1973-2018

# subset to earliest EP election (1979) - 4 years
d <- filter(d, year >= 1975)
min(d$year) # 1976

# 5-year windows leading to (major) EP election year
g <- c(min(d$year), 1979, 1984, 1989, 1994, 1999, 2004, 2009, 2014, 2019)
d$year5 <- cut(d$year, g, include.lowest = TRUE)

# ------------------------------------------------------------------------------
# plots
# ------------------------------------------------------------------------------

# full trend, colored by EP election period
# -----------------------------------------

g <- aes(date, total_positive, group = iso3c, color = year5)

ggplot(d, g) +
  geom_hline(yintercept = mean(d$total_positive), lty = "dotted") +
  geom_point() +
  geom_line() +
  scale_color_discrete("") +
  scale_y_continuous(breaks = 2:3) +
  scale_x_date(
    breaks = as.Date(paste0(c(1979, 1989, 1999, 2009, 2019), "-01-01")),
    labels = function(x) substr(x, 3, 4)
  ) +
  facet_wrap(~ iso3c) +
  labs(
    title = paste(
      "Satisfaction with democracy (% very/fairly satisfied),",
      paste0(range(d$year), collapse = "-")
    ),
    subtitle = "Dotted line at sample average",
    x = NULL,
    y = NULL
  ) +
  theme(
    panel.background = element_rect(fill = "grey95"),
    panel.grid.minor = element_blank()
  )

ggsave(
  "plots/stf_dem_p-by-period.pdf",
  width = 13,
  height = 9
)

# boxplots by country
# 

g <- aes(reorder(iso3c, total_positive), total_positive)

ggplot(d, g) +
  geom_hline(
    aes(yintercept = mean(d$total_positive)),
    lty = "dotted"
  ) +
  geom_boxplot(outlier.size = 0, color = "grey25") +
  # geom_point() +
  coord_flip() +
  labs(
    x = NULL,
    y = NULL,
    title = paste(
      "Satisfaction with democracy (% very/fairly satisfied),",
      paste0(range(d$year), collapse = "-")
    ),
    subtitle = "Dotted line at sample average"
  )

ggsave(
  "plots/stf_dem_p-boxplots.pdf",
  width = 9,
  height = 9
)
# ------------------------------------------------------------------------------
# missing values
# ------------------------------------------------------------------------------

summary(x$nonresponse_rate)
table(x$nonresponse_rate_factored)
table(x$nonresponse_rate_factored) / nrow(x)

# ~ 74 % of rows with   < 5 % missing values
# ~ 21 % of rows with  5-10 % missing values
#         5 rows with 10-15 % missing values
#         2 rows with 15-20 % missing values

# # missing values concentrated in early waves
#
# filter(x, nonresponse_rate > 0.1) %>%
#   select(iso3c, date, nonresponse_rate) %>%
#   print(n = 100)

# # some serious cases in Belgium, Luxembourg, Spain, France; rest is fine
#
# ggplot(x, aes(date, nonresponse_rate)) +
#   geom_col(aes(fill = )) +
#   scale_fill_brewer(palette = "YlOrRd") +
#   facet_wrap(~ iso3c, scales = "free_x") +
#   guides(fill = FALSE) +
#   theme(panel.grid.minor = element_blank())

# ------------------------------------------------------------------------------
# aggregate over EP election periods
# ------------------------------------------------------------------------------

# use all measures from EP election year, plus previous years
d <- full_join(select(d, -year), e, by = c("iso3c", "date")) %>% 
  arrange(iso3c, date) %>% 
  # fill in EP election year for rows coming from the EB data
  tidyr::fill(year, .direction = "up") %>% 
  group_by(iso3c, year) %>% 
  summarise(
    stf_dem_n = sum(!is.na(total_positive)),
    stf_dem_p = mean(total_positive, na.rm = TRUE) %>% 
      if_else(is.nan(.), NA_real_, .) # cases where stf_dem_n == 0
  )

# average satisfaction similar to its pre-aggregate value
mean(d$stf_dem_p, na.rm = TRUE)

# each data point is based on ~ 6 EB waves on average
mean(d$stf_dem_n)

# coverage was low in 2004 (see below) and 2009 especially
tapply(d$stf_dem_n, d$year, mean)

# missing values (no EB wave to aggregate in order to compute `stf_dem_p`):
#
# -  2 early Western EP elections (AUT, SWE)
# - 10 Eastern EP elections in 2004 (all new Member States)
# -  1 later Member State (ROU in 2009)
filter(d, is.na(stf_dem_p)) %>% 
  arrange(year)

write_tsv(d, "data-raw/eb-satisfaction-aggregates.tsv")

# have a nice day
