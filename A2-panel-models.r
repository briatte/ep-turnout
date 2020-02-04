library(broom)
library(dplyr)
library(ggplot2)
library(panelAR)
library(readr)
library(plm)
library(tidyr)

d <- read_tsv("reconnect-temp-dataset.tsv") %>% 
  as.data.frame

d14 <- filter(d, year != 2019) %>% 
  as.data.frame

f <- formula(
  voter_turnout ~
    ms_eastern + # (OR) ms_recent +
    ne_concomitant +
    tt_last_ne +
    ne_before +
    includes_weekdays2 +
    first_ep_election +
    cv +
    lne_voter_turnout +
    comp12 +
    n_lists +
    enp_votes +
    stf_dem_p +
    gdpc +
    unempl +
    gini_disp
)


L1 <- lm(f, data = d)
print(car::vif(L1))
# resid(L1) %>% density %>% plot

# autocorrelation
pdwtest(f, data = as.data.frame(d))

P1 <- panelAR::panelAR(f, data = as.data.frame(d),
                       panelVar = "iso3c", timeVar = "year",
                       autoCorr = "ar1", panelCorrMethod = "phet")
# P1 <- plm(f, index = c("iso3c", "year"), model = "within", data = d)

L2 <- lm(f, data = d14)
print(car::vif(L2))
# resid(L2) %>% density %>% plot

# autocorrelation
pdwtest(f, data = as.data.frame(d14))

P2 <- panelAR::panelAR(f, data = as.data.frame(d14),
                       panelVar = "iso3c", timeVar = "year",
                       autoCorr = "ar1", panelCorrMethod = "phet",
                       rho.na.rm = TRUE)
# P2 <- plm(f, index = c("iso3c", "year"), model = "within", data = d14)

f <- update.formula(f, ~ . - cv + cv_fixed)
FD1 <- plm(f, index = c("iso3c", "year"), model = "fd", data = d)
FD2 <- plm(f, index = c("iso3c", "year"), model = "fd", data = d14)

tidy.panelAR <- function(x) {
  tibble(
    term = names(coef(x)),
    estimate = coef(x),
    std.error = sqrt(diag(x$vcov)),
    statistic = estimate / std.error,
    p.value = 2 * pt(abs(statistic), x$df.residual, lower.tail = FALSE)
  )
}

g <- bind_rows(
  tidy(L1) %>% tibble::add_column(model = "L19")  ,
  tidy(L2) %>% tibble::add_column(model = "L14")  ,
  tidy.panelAR(P1) %>% tibble::add_column(model = "P19")  ,
  tidy.panelAR(P2) %>% tibble::add_column(model = "P14")  ,
  tidy(FD1) %>% tibble::add_column(model = "FD19") ,
  tidy(FD2) %>% tibble::add_column(model = "FD14")
)

g$model <- factor(g$model, levels = c("L14", "P14", "FD14", "L19", "P19", "FD19"))
g$p.level <- cut(
  g$p.value,
  c(0, .01, .05, .1, 1),
  include.lowest = FALSE,
  right = FALSE,
  labels = c("< .01", "< .05", "< .1", "n.s.")
)
ggplot(
  filter(g, term != "(Intercept)"),
  aes(model, estimate, group = model, color = p.level)
) +
  geom_pointrange(
    aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 * std.error)
  ) +
  geom_hline(yintercept = 0, lty = "dotted") +
  #scale_color_brewer(palette = "RdGy") +
  scale_color_manual(values = c("< .01" = "black", "< .05" = "grey35", "< .1" = "grey65", "n.s." = "grey85")) +
  # scale_color_gradient2(low = "green", mid = "grey50", midpoint = 0.05, high = "blue") +
  facet_wrap(~ term, scales = "free_y") +
  labs(x = NULL, y = NULL) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(1, 0)
  )

ggsave(
  "panel-models.pdf",
  width = 10,
  height = 9
)
levels(g$p.level) <- c("***", "**", "*", "")
mutate(g, estimate = paste0(round(estimate, 2), p.level), se = paste0("(",round(std.error, 2), ")")) %>% 
  select(model, term, estimate, se) %>%
  tidyr::gather(k, v, -model, -term) %>%
  #mutate(v = if_else(is.na(v), "", v)) %>% 
  arrange(model, term, k) %>% 
  tidyr::spread(model, v) %>% 
  mutate(term = if_else(k == "se", "", term)) %>% 
  select(-k) %>% 
  write_tsv("panel-models.tsv", na = "")

# cheers
