# ***********************************************************************************************
# Library   : tidybayes
# Title     : Extracting and visualizing tidy residuals from Bayesian models
# Created by: Owner
# Created on: 2021/04/06
# URL       : https://mjskay.github.io/tidybayes/articles/tidybayes-residuals.html
# ***********************************************************************************************


# ＜ポイント＞
# - tidybayesは生前データとしてサンプリング結果を出してくれるパッケージ
# - MCMC自体に対してサクっと見る関数は入ってない


# ＜対応パッケージ＞
# - rstan
# - brms
# - rstanarm
# - runjags
# - rjags
# - jagsUI
# - coda::mcmc
# - coda::mcmc.list
# - MCMCglmm



# 0 準備 -----------------------------------------------------------------------
library(dplyr)
library(purrr)
library(tidyr)
library(ggdist)
library(tidybayes)
library(ggplot2)
library(cowplot)
library(rstan)
library(brms)
library(gganimate)

rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())




set.seed(4118)
n = 100

cens_df =
  tibble(
    y_star = rnorm(n, 0.5, 1),
    y_lower = floor(y_star),
    y_upper = ceiling(y_star),
    censoring = "interval"
  )


head(cens_df, 10)

uncensored_plot = cens_df %>%
  ggplot(aes(y = "", x = y_star)) +
  stat_slab() +
  geom_jitter(aes(y = 0.75, color = ordered(y_lower)), position = position_jitter(height = 0.2), show.legend = FALSE) +
  ylab(NULL) +
  scale_x_continuous(breaks = -4:4, limits = c(-4, 4)) +
  background_grid("x")

censored_plot = cens_df %>%
  ggplot(aes(y = "", x = (y_lower + y_upper)/2)) +
  geom_dotplot(
    aes(fill = ordered(y_lower)),
    method = "histodot", origin = -4, binwidth = 1, dotsize = 0.5, stackratio = .8, show.legend = FALSE,
    stackgroups = TRUE, binpositions = "all", color = NA
  ) +
  geom_segment(
    aes(x = y + 0.5, xend = y + 0.5, y = 1.75, yend = 1.5, color = ordered(y)),
    data = data.frame(y = unique(cens_df$y_lower)), show.legend = FALSE,
    arrow = arrow(type = "closed", length = unit(7, "points")), size = 1
  ) +
  ylab(NULL) +
  xlab("interval-censored y") +
  scale_x_continuous(breaks = -4:4, limits = c(-4, 4)) +
  background_grid("x")

plot_grid(align = "v", ncol = 1, rel_heights = c(1, 2.5),
  uncensored_plot,
  censored_plot
)

m_ideal = brm(
  y_star ~ 1,
  data = cens_df,
  family = student,

  file = "library/tidybayes/article/models/tidybayes-residuals_m_ideal.rds"  # cache model (can be removed)
)

m_ideal

cens_df %>%
  add_residual_draws(m_ideal) %>%
  ggplot(aes(x = .row, y = .residual)) +
  stat_pointinterval()


cens_df %>%
  add_residual_draws(m_ideal) %>%
  median_qi() %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line()


cens_df %>%
  add_predicted_draws(m_ideal) %>%
  summarise(
    p_residual = mean(.prediction < y_star),
    z_residual = qnorm(p_residual)
  ) %>%
  ggplot(aes(sample = z_residual)) +
  geom_qq() +
  geom_abline()


m = brm(
  y_lower | cens(censoring, y_upper) ~ 1,
  data = cens_df,

  file = "library/tidybayes/article/models/tidybayes-residuals_m.rds"  # cache model (can be removed)
)

m

cens_df %>%
  add_residual_draws(m) %>%
  ggplot(aes(x = .row, y = .residual)) +
  stat_pointinterval()

cens_df %>%
  add_residual_draws(m) %>%
  median_qi(.residual) %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line()


cens_df %>%
  add_predicted_draws(m) %>%
  summarise(
    p_lower = mean(.prediction < y_lower),
    p_upper = mean(.prediction < y_upper),
    p_residual = runif(1, p_lower, p_upper),
    z_residual = qnorm(p_residual)
  ) %>%
  ggplot(aes(x = .row, y = z_residual)) +
  geom_point()


cens_df %>%
  add_predicted_draws(m) %>%
  summarise(
    p_lower = mean(.prediction < y_lower),
    p_upper = mean(.prediction < y_upper),
    p_residual = runif(1, p_lower, p_upper),
    z_residual = qnorm(p_residual)
  ) %>%
  ggplot(aes(sample = z_residual)) +
  geom_qq() +
  geom_abline()


k = 20

p = cens_df %>%
  add_predicted_draws(m) %>%
  summarise(
    p_lower = mean(.prediction < y_lower),
    p_upper = mean(.prediction < y_upper),
    p_residual = list(runif(k, p_lower, p_upper)),
    residual_draw = list(1:k)
  ) %>%
  unnest(c(p_residual, residual_draw)) %>%
  mutate(z_residual = qnorm(p_residual)) %>%
  ggplot(aes(sample = z_residual)) +
  geom_qq() +
  geom_abline() +
  transition_manual(residual_draw)

set.seed(41181)
n = 100

cens_df_t =
  tibble(
    y = rt(n, 3) + 0.5,
    y_lower = floor(y),
    y_upper = ceiling(y),
    censoring = "interval"
  )


uncensored_plot = cens_df_t %>%
  ggplot(aes(y = "", x = y)) +
  stat_slab() +
  geom_jitter(aes(y = 0.75, color = ordered(y_lower)), position = position_jitter(height = 0.2), show.legend = FALSE) +
  ylab(NULL) +
  scale_x_continuous(breaks = -10:10, limits = c(-10, 10)) +
  background_grid("x")

censored_plot = cens_df_t %>%
  ggplot(aes(y = "", x = (y_lower + y_upper)/2)) +
  geom_dotplot(
    aes(fill = ordered(y_lower)),
    method = "histodot", origin = -4, binwidth = 1, dotsize = 0.5, stackratio = .8, show.legend = FALSE,
    stackgroups = TRUE, binpositions = "all", color = NA
  ) +
  geom_segment(
    aes(x = y + 0.5, xend = y + 0.5, y = 1.75, yend = 1.5, color = ordered(y)),
    data = data.frame(y = unique(cens_df_t$y_lower)), show.legend = FALSE,
    arrow = arrow(type = "closed", length = unit(7, "points")), size = 1
  ) +
  ylab(NULL) +
  xlab("interval-censored y") +
  scale_x_continuous(breaks = -10:10, limits = c(-10, 10)) +
  background_grid("x")

plot_grid(align = "v", ncol = 1, rel_heights = c(1, 2.25),
  uncensored_plot,
  censored_plot
)

m_t1 = brm(
  y_lower | cens(censoring, y_upper) ~ 1,
  data = cens_df_t,

  file = "library/tidybayes/article/models/tidybayes-residuals_m_t1"  # cache model (can be removed)
)

cens_df_t %>%
  add_residual_draws(m_t1) %>%
  median_qi(.residual) %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line()


cens_df_t %>%
  add_predicted_draws(m_t1) %>%
  summarise(
    p_lower = mean(.prediction < y_lower),
    p_upper = mean(.prediction < y_upper),
    p_residual = runif(1, p_lower, p_upper),
    z_residual = qnorm(p_residual)
  ) %>%
  ggplot(aes(sample = z_residual)) +
  geom_qq() +
  geom_abline()



m_t2 = brm(
  y_lower | cens(censoring, y_upper) ~ 1,
  data = cens_df_t,
  family = student,

  file = "library/tidybayes/article/models/tidybayes-residuals_m_t2.rds"  # cache model (can be removed)
)


cens_df_t %>%
  add_residual_draws(m_t2) %>%
  median_qi(.residual) %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line()


k = 20

p = cens_df_t %>%
  add_predicted_draws(m_t2) %>%
  summarise(
    p_lower = mean(.prediction < y_lower),
    p_upper = mean(.prediction < y_upper),
    p_residual = list(runif(k, p_lower, p_upper)),
    residual_draw = list(1:k)
  ) %>%
  unnest(c(p_residual, residual_draw)) %>%
  mutate(z_residual = qnorm(p_residual)) %>%
  ggplot(aes(sample = z_residual)) +
  geom_qq() +
  geom_abline() +
  transition_manual(residual_draw)


cens_df_o = cens_df_t %>%
  mutate(y_factor = ordered(y_lower))


m_o = brm(
  y_factor ~ 1,
  data = cens_df_o,
  family = cumulative,
  prior = prior(normal(0, 10), class = Intercept),
  control = list(adapt_delta = 0.99),

  file = "library/tidybayes/article/models/tidybayes-residuals_m_o.rds"  # cache model (can be removed)
)



m_o = brm(
  y_factor ~ 1,
  data = cens_df_o,
  family = cumulative,
  prior = prior(normal(0, 10), class = Intercept),
  control = list(adapt_delta = 0.99),

  file = "library/tidybayes/article/models/tidybayes-residuals_m_o.rds"  # cache model (can be removed)
)


m_o = brm(
  y_factor ~ 1,
  data = cens_df_o,
  family = cumulative,
  prior = prior(normal(0, 10), class = Intercept),
  control = list(adapt_delta = 0.99),

  file = "library/tidybayes/article/models/tidybayes-residuals_m_o.rds"  # cache model (can be removed)
)

cens_df_o %>%
  add_residual_draws(m_o) %>%
  median_qi(.residual) %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line()


cens_df_o %>%
  add_predicted_draws(m_o) %>%
  mutate(.prediction = ordered(levels(y_factor)[.prediction], levels = levels(y_factor))) %>%
  summarise(
    p_lower = mean(.prediction < y_factor),
    p_upper = mean(.prediction <= y_factor),
    p_residual = runif(1, p_lower, p_upper),
    z_residual = qnorm(p_residual)
  ) %>%
  ggplot(aes(x = .row, y = z_residual)) +
  geom_point()


k = 20

p = cens_df_o %>%
  add_predicted_draws(m_o) %>%
  mutate(.prediction = ordered(levels(y_factor)[.prediction], levels = levels(y_factor))) %>%
  summarise(
    p_lower = mean(.prediction < y_factor),
    p_upper = mean(.prediction <= y_factor),
    p_residual = list(runif(k, p_lower, p_upper)),
    residual_draw = list(1:k)
  ) %>%
  unnest(c(p_residual, residual_draw)) %>%
  mutate(z_residual = qnorm(p_residual)) %>%
  ggplot(aes(sample = z_residual)) +
  geom_qq() +
  geom_abline() +
  transition_manual(residual_draw)


library(rlang)
make_probability_residuals = function(data, prediction, y, y_upper = NA, n = 1) {
  .prediction = enquo(prediction)
  .y = enquo(y)
  .y_upper = enquo(y_upper)

  if (eval_tidy(expr(is.factor(!!.prediction) && !is.ordered(!!.prediction)), data)) {
    data = mutate(data, !!.prediction := ordered(!!.prediction, levels = levels(!!.prediction)))
  }

  if (is.na(enquo(y_upper)[[2]])) {
    #no y_upper provided, use y as y_upper
    data = summarise(data,
      .p_lower = mean(!!.prediction < !!.y),
      .p_upper = mean(!!.prediction <= !!.y)
    )
  } else {
    #y_upper should be a vector, and if an entry in it is NA, use the entry from y
    data = summarise(data,
      .p_lower = mean(!!.prediction < !!.y),
      .p_upper = mean(!!.prediction <= ifelse(is.na(!!.y_upper), !!.y, !!.y_upper))
    )
  }

  data %>%
    mutate(
      .p_residual = map2(.p_lower, .p_upper, runif, n = !!n),
      .residual_draw = map(.p_residual, seq_along)
    ) %>%
    unnest(c(.p_residual, .residual_draw)) %>%
    mutate(.z_residual = qnorm(.p_residual))
}


set.seed(51919)

bin_df = tibble(
  y = rbernoulli(100, .7)
)

m_bin = brm(
  y ~ 1,
  data = bin_df,
  family = bernoulli,

  file = "library/tidybayes/article/models/tidybayes-residuals_m_bin.rds"  # cache model (can be removed)
)


bin_df %>%
  add_residual_draws(m_bin) %>%
  median_qi() %>%
  ggplot(aes(sample = .residual)) +
  geom_qq() +
  geom_qq_line()


k = 20

p = bin_df %>%
  add_predicted_draws(m_bin) %>%
  make_probability_residuals(.prediction, y, n = k) %>%
  ggplot(aes(sample = .p_residual)) +
  geom_qq(distribution = qunif) +
  geom_abline() +
  transition_manual(.residual_draw)