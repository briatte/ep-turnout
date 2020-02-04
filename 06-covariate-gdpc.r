# ------------------------------------------------------------------------------
# Get GDP per capita (constant USD)
# -> Output: data/wdi-ny.gdp.pcap.cd.tsv
#
# Package dependencies
# ------------------------------------------------------------------------------

library(countrycode)
library(dplyr)
library(fable)
library(ggplot2)
library(tsibble)
library(purrr)
library(readr)
library(WDI)

# country-year dyads
d <- read_tsv("data/elections-parliament-and-ep.tsv") %>% 
  # filter(election_type == "ep") %>% 
  select(iso3c, year) %>% 
  distinct # remove any duplicated years

# ------------------------------------------------------------------------------
# GDP per capita
#
# Source: WDI (World Bank)
# https://data.worldbank.org/indicator/ny.gdp.pcap.cd
# ------------------------------------------------------------------------------

f <- "data/wdi-ny.gdp.pcap.cd.tsv"
if (!file.exists(f)) {
  
  x <- WDI(
    indicator = "ny.gdp.pcap.cd",
    country = unique(d$iso3c),
    start = min(d$year),
    end = max(d$year),
    extra = TRUE
  )
  
  x <- x %>% 
    select(iso3c, year, gdpc = ny.gdp.pcap.cd) %>% 
    mutate(iso3c = as.character(iso3c))
  
  write_tsv(x, f)
  
}

x <- read_tsv(f)

# ------------------------------------------------------------------------------
# Missing values
# ------------------------------------------------------------------------------

d <- left_join(d, x, by = c("iso3c", "year")) %>% 
  arrange(iso3c, year)

# stopifnot(!is.na(d$gdpc))

filter(d, is.na(gdpc)) %>%
  select(iso3c, year) %>% 
  arrange(year) %>%
  print # missing a few 2018 + all 2019 values

# predict log-GDP/c for 2018-2019 using various ARIMA models
t <- na.omit(x) %>% # remove years with no GDP/c values
  mutate(log_gdpc = log(gdpc)) %>% 
  as_tsibble(key = iso3c, index = year) %>% 
  model(ARIMA(log_gdpc)) %>% 
  forecast(h = 2) %>% 
  # time horizon of 2 years after last year values (2017)
  mutate(yhat_sd = map_dbl(.distribution, ~ .$sd)) %>% 
  rename(yhat = log_gdpc) %>% 
  as_tibble

# add imputed values
x <- full_join(x, select(t, iso3c, year, yhat), by = c("iso3c", "year")) %>% 
  mutate(
    gdpc_source = if_else(is.na(gdpc), "wdi-imputed", "wdi"),
    gdpc = if_else(is.na(gdpc), exp(yhat), gdpc)
  ) %>% 
  # remove older years
  filter(!is.na(gdpc)) %>% 
  select(-yhat)

# sanity check: forecasting only last three years, 2018-2020
stopifnot(x$year[ x$gdpc_source == "wdi-imputed" ] %in% c(2018, 2019, 2020))

ggplot(x, aes(year, gdpc)) +
  geom_line() +
  geom_point(aes(color = gdpc_source)) +
  geom_segment(
    data = t,
    aes(xend = year, y = exp(yhat - yhat_sd), yend = exp(yhat + yhat_sd)),
    color = "tomato"
  ) +
  scale_color_manual(values = c("wdi" = "black", "wdi-imputed" = "tomato")) +
  facet_wrap(~ iso3c, scales = "free") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(1, 0),
    legend.direction = "horizontal"
  )

ggsave(
  "plots/gdpc-arima-forecast.pdf",
  width = 11,
  height = 9
)

d <- left_join(select(d, -gdpc), x, by = c("iso3c", "year"))

# ------------------------------------------------------------------------------
# Density curves
# ------------------------------------------------------------------------------

ggplot(d, aes(x = gdpc)) +
  # by country
  geom_density(fill = "grey75") +
  # 'all countries' facet
  geom_density(data = mutate(d, iso3c = "(EU-28)")) +
  facet_wrap(~ iso3c, scales = "free_y") +
  # 'all countries' shown in each facet
  geom_density(data = select(d, gdpc), lty = "dotted") +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave(
  "plots/gdpc-density-curves.pdf",
  width = 11,
  height = 9
)

# ------------------------------------------------------------------------------
# Export
# ------------------------------------------------------------------------------

write_tsv(d, "data/covariate-gdpc.tsv")

# have a nice day
