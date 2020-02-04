# ------------------------------------------------------------------------------
# Get Gini coefficients
# -> Output: data-raw/covariate-gini.tsv
#
# Package dependencies
# ------------------------------------------------------------------------------

library(countrycode)
library(dplyr)
library(ggplot2)
library(mgcv)  # gam
library(purrr) # map, map2
library(readr)
library(tidyr) # nest, unnest

# country-year dyads
d <- read_tsv("data/elections-parliament-and-ep.tsv") %>% 
  # filter(election_type == "ep") %>% 
  select(iso3c, year) %>% 
  distinct # remove any duplicated years

# ------------------------------------------------------------------------------
# (Standardized) Gini coefficients
#
# Source: SWIID (Frederick Solt)
# https://fsolt.org/swiid/
# (manually downloaded)
# ------------------------------------------------------------------------------

x <- read_csv("data-raw/swiid8_1_summary.csv")%>%
  select(country, year, gini_disp)

# to compute similar values manually (using the SWIID Stata dataset):
#
# library(dplyr)
# library(haven)
# library(tidyr)
# x <- read_dta("data-raw/swiid8_1/swiid8_1.dta") %>%
#   mutate(year = as.numeric(year)) %>% # lose Stata format attributes
#   group_by(country, year) %>%
#   select(country, year, matches("_(.*)gini_disp")) %>%
#   gather(key, value, -country, -year) %>%
#   summarise(gini_disp = 100 * mean(value, na.rm = TRUE)) %>%
#   ungroup # to allow modifying the country variable

# ISO-3C codes (none for e.g. Czechoslovakia, but we do not need it)
x$country <- countrycode(x$country, "country.name", "iso3c")
x <- rename(x, iso3c = country)

d <- left_join(d, x, by = c("iso3c", "year")) %>% 
  arrange(iso3c, year)

# ------------------------------------------------------------------------------
# Missing values
# ------------------------------------------------------------------------------

# stopifnot(!is.na(d$gini_disp))

filter(d, is.na(gini_disp)) %>%
  select(iso3c, year) %>% 
  arrange(year) %>% # Belgium 1978, Luxembourg 1979 and 1984, plus some 2017-2019
  print

# thin plate regression splines seem to predict Gini pretty well
#
#   (and avoid the knot-placement problem:
#    see Wood 2017 sec. 5.5 on isotropic smoothing, esp. pp. 218-9)
#
# f <- y ~ s(x, bs = "tp")
# 
# ggplot(filter(x, iso3c %in% d$iso3c), aes(year, gini_disp)) +
#   geom_point() +
#   geom_smooth(method = "gam", formula = f) +
#   facet_wrap(~ iso3c, scales = "free_x")
# 
# ggplot(filter(x, iso3c == "LUX"), aes(year, gini_disp)) +
#   geom_point() + 
#   geom_smooth(method = "gam", formula = f)

# prepare estimation sample
x <- filter(x, iso3c %in% d$iso3c) %>% 
  # add rows with missing values
  full_join(select(d, -gini_disp), by = c("iso3c", "year")) %>%
  group_by(iso3c) %>% 
  nest

# fit generalized additive models by country
z <- map(
  x$data,
  ~ gam(formula = gini_disp ~ s(year, bs = "tp"), data = .)
)

# compute residuals
x$data <- map2(
  x$data, z,
  ~ mutate(
    .x,
    yhat = predict(.y, newdata = data.frame(year = .x$year)),
    resid = gini_disp - yhat
  )
)

x <- unnest(x) %>% 
  arrange(iso3c, year)

# almost no residuals >= 1 (3 points out of 1116)
filter(x, abs(resid) >= 1)
# - Denmark 1987
# - Italy 1991
# - Sweden 1975

ggplot(x, aes(x = resid, fill = abs(resid) >= 1)) +
  geom_dotplot(color = NA) +
  geom_vline(xintercept = c(-1, 0, 1), lty = "dotted") +
  scale_x_continuous(breaks = c(-1, 0, 1)) +
  scale_fill_manual(
    "GAM residuals",
    values = c("TRUE" = "tomato", "FALSE" = "black"),
    labels = c("TRUE" = ">= 1", "FALSE" = "< 1"),
  ) +
  facet_wrap(~ iso3c) +
  labs(y = NULL, x = NULL) +
  theme(
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    legend.direction = "horizontal"
  )

ggsave(
  "plots/gini-gam-residuals.pdf",
  width = 11,
  height = 9
)

# add imputed values
d <- left_join(d, select(x, -gini_disp), by = c("iso3c", "year")) %>% 
  mutate(
    gini_source = if_else(is.na(gini_disp), "swiid-imputed", "swiid"),
    gini_disp = if_else(is.na(gini_disp), round(yhat, 1), gini_disp)
  ) %>% 
  select(-yhat, -resid)

# ------------------------------------------------------------------------------
# Density curves
# ------------------------------------------------------------------------------

ggplot(d, aes(x = gini_disp)) +
  # by country
  geom_density(fill = "grey75") +
  # 'all countries' facet
  geom_density(data = mutate(d, iso3c = "(EU-28)")) +
  facet_wrap(~ iso3c, scales = "free_y") +
  # 'all countries' shown in each facet
  geom_density(data = select(d, gini_disp), lty = "dotted") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  "plots/gini-density-curves.pdf",
  width = 11,
  height = 9
)

# save a TSV plain text copy
write_tsv(d, "data-raw/covariate-gini.tsv")

# have a nice day
