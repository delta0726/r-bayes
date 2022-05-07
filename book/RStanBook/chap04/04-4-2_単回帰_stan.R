# ***********************************************************************************************
# Title   : StanとRでベイズ統計モデリング
# Theme   : 2 Stan入門編
# Chapter : 4-4 StanとRStanをはじめよう（単回帰_stan）
# Date    : 2022/05/07
# Page    : P28 - P49
# URL     : https://github.com/MatsuuraKentaro/RStanBook
# ***********************************************************************************************


# ＜概要＞
# - 単回帰を通してStanとRstanの使い方を確認する
#   --- 実際にはRからStanを実行してMCMCサンプルを得てベイズ信頼区間やベイズ予測区間を計算する
#   --- 単回帰は単純なモデルなのでlmとstanの結果にほぼ差がないことを確認する


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 線形回帰モデルの構築
# 3 信頼区間のプロット
# 4 予測区間のプロット
# 5 トレースプロットで収束診断
# 6 MCMCの設定変更
# 7 MCMC散布図と周辺分布
# 8 ベイズ信頼区間とベイズ予想区間の算出


# 0 準備 ---------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(rstan)
library(ggmcmc)
library(patchwork)


# オプション設定
# --- 計算の高速化(P46)
# --- stanファイルのコンパイル結果の保存＆複数chainを並列計算
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# データロード
d <- read_csv(file = 'chap04/csv/data-salary.txt')


# 1 データ確認 -----------------------------------------------------------------------------

# データ確認
# --- 年収(Y)と年齢(X)のデータ
d %>% print()

# 散布図
d %>%
  ggplot(aes(x = X, y = Y)) +
  geom_point(shape = 1, size = 3) +
  theme_bw(base_size = 18)


# 2 線形回帰モデルをstanから実行 ----------------------------------------------------------

# 予測用データ
X_new <- data.frame(X = 23:60)

# Stan用のデータ
data <-
  list(N = nrow(d),
       X = d$X,
       Y = d$Y,
       N_new = nrow(X_new),
       X_new = X_new$X)

# モデル構築
fit <- stan(file = 'chap04/stan/model4-4.stan', data = data, seed = 1234)

# 確認
fit %>% print()

# 結果保存
# save.image(file = "chap04/data/result-model4-5.Rdata")


# 3 信頼区間のプロット ----------------------------------------------------------------

# データ抽出
ms <- fit %>% rstan::extract()

# データ作成
d_est <-
  ms$y_base_new %>%
    apply(2, quantile, probs = c(0.025, 0.25, 0.50, 0.75, 0.975)) %>%
    t() %>%
    as.data.frame() %>%
    bind_cols(X_new)

# プロット
ggplot() +
  theme_bw(base_size = 18) +
  geom_ribbon(data = d_est, aes(x = X, ymin = `2.5%`, ymax = `97.5%`), fill = 'black', alpha = 1 / 6) +
  geom_ribbon(data = d_est, aes(x = X, ymin = `25%`, ymax = `75%`), fill = 'black', alpha = 2 / 6) +
  geom_line(data = d_est, aes(x = X, y = `50%`), size = 1) +
  geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 3) +
  coord_cartesian(xlim = c(22, 61), ylim = c(200, 1400)) +
  scale_y_continuous(breaks = seq(from = 200, to = 1400, by = 400)) +
  labs(y = 'Y')


# 4 予測区間のプロット ----------------------------------------------------------------

# データ抽出
ms <- fit %>% rstan::extract()

# データ作成
d_est <-
  ms$y_new %>%
    apply(2, quantile, probs = c(0.025, 0.25, 0.50, 0.75, 0.975)) %>%
    t() %>%
    as.data.frame() %>%
    bind_cols(X_new)

# プロット
ggplot() +
  theme_bw(base_size = 18) +
  geom_ribbon(data = d_est, aes(x = X, ymin = `2.5%`, ymax = `97.5%`), fill = 'black', alpha = 1 / 6) +
  geom_ribbon(data = d_est, aes(x = X, ymin = `25%`, ymax = `75%`), fill = 'black', alpha = 2 / 6) +
  geom_line(data = d_est, aes(x = X, y = `50%`), size = 1) +
  geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 3) +
  coord_cartesian(xlim = c(22, 61), ylim = c(200, 1400)) +
  scale_y_continuous(breaks = seq(from = 200, to = 1400, by = 400)) +
  labs(y = 'Y')


# 5 トレースプロットで収束診断 --------------------------------------------------------

fit %>%
  ggs(inc_warmup = TRUE, stan_include_auxiliar = TRUE) %>%
  ggs_traceplot() +
  theme_bw(base_size = 18) +
  scale_colour_manual(values = c('#dcdcdc', '#a9a9a9', '#696969', '#000000')) +
  labs(color = 'Chain')


# 6 MCMCの設定変更 ------------------------------------------------------------------

data <-
  list(N = nrow(d),
       X = d$X,
       Y = d$Y)

# コンパイルのみ実行
stanmodel <- stan_model(file = 'chap04/stan/model4-5.stan')


fit <-
  stanmodel %>%
    sampling(data = data,
             pars = c('b', 'sigma'),
             init = function() {
               list(a = runif(1, -10, 10),
                    b = runif(1, 0, 10), sigma = 10) },
             seed = 123,
             chains = 3, iter = 1000, warmup = 200, thin = 2)


# 7 MCMC散布図と周辺分布 ------------------------------------------------


# データ抽出
ms <- fit %>% rstan::extract()

# データフレーム作成
df_mcmc <- tibble(a = ms$a, b = ms$b, sigma = ms$sigma)

# 範囲設定
x_range <- c(-420, 210)
y_range <- c(14.5, 29)
x_breaks <- seq(-400, 200, 200)
y_breaks <- seq(15, 25, 5)

p_xy <-
  df_mcmc %>%
    ggplot(aes(x = a, y = b)) +
    geom_point(alpha = 1 / 4, size = 2, shape = 1) +
    coord_cartesian(xlim = x_range, ylim = y_range) +
    scale_x_continuous(breaks = x_breaks) +
    scale_y_continuous(breaks = y_breaks) +
    theme_bw(base_size = 18)

p_x <-
  df_mcmc %>%
    ggplot(aes(x = a)) +
    geom_histogram(aes(y = ..density..), colour = 'black', fill = 'white') +
    geom_density(alpha = 0.3, fill = 'gray20') +
    coord_cartesian(xlim = x_range) +
    scale_x_continuous(breaks = x_breaks) +
    labs(x = '', y = '') +
    theme_bw(base_size = 18) +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())

p_y <-
  df_mcmc %>%
    ggplot(aes(x = b)) +
    geom_histogram(aes(y = ..density..), colour = 'black', fill = 'white') +
    geom_density(alpha = 0.3, fill = 'gray20') +
    scale_x_continuous(breaks = y_breaks) +
    coord_flip(xlim = y_range) +
    labs(x = '', y = '') +
    theme_bw(base_size = 18) +
    theme(axis.title.y = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.x = element_blank(), axis.ticks.y = element_blank())

# プロット結合
wrap_plots(
  p_x, plot_spacer(),
  p_xy, p_y,
  nrow = 2,
  widths = c(1, 0.3),
  heights = c(0.3, 1)
)


# 8 ベイズ信頼区間とベイズ予想区間の算出 ------------------------------------------------

# データ抽出
ms <- fit %>% rstan::extract()

# サンプル数
N_mcmc <- ms$lp__ %>% length()

# データフレーム作成
# --- 50際の人の基本年収分布(からのMCMCサンプル)を作成
# --- 正規乱数から年収の予測分布を作成
y50_base <- ms$a + ms$b * 50
y50 <- rnorm(n = N_mcmc, mean = y50_base, sd = ms$sigma)
d_mcmc <- data.frame(a = ms$a, b = ms$b, sigma = ms$sigma, y50_base, y50)

# 予測データの設定
X_new <- 23:60
N_X <- X_new %>% length()

# 予測データ作成
set.seed(1234)
y_base_mcmc <- matrix(nrow = N_mcmc, ncol = N_X) %>% as.data.frame()
y_mcmc <- matrix(nrow = N_mcmc, ncol = N_X) %>% as.data.frame()
for (i in 1:N_X) {
  y_base_mcmc[, i] <- ms$a + ms$b * X_new[i]
  y_mcmc[, i] <- rnorm(n = N_mcmc, mean = y_base_mcmc[, i], sd = ms$sigma)
}


# ベイズ信用区間の作成
qua <- y_base_mcmc %>% apply( 2, quantile, probs = c(0.025, 0.25, 0.50, 0.75, 0.975))
d_est <- data.frame(X = X_new, t(qua), check.names = FALSE)

# プロット
ggplot() +
  geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 3) +
  geom_line(data = d_est, aes(x = X, y = `50%`), size = 1) +
  geom_ribbon(data = d_est, aes(x = X, ymin = `2.5%`, ymax = `97.5%`), fill = 'black', alpha = 1 / 6) +
  geom_ribbon(data = d_est, aes(x = X, ymin = `25%`, ymax = `75%`), fill = 'black', alpha = 2 / 6) +
  coord_cartesian(xlim = c(22, 61), ylim = c(200, 1400)) +
  scale_y_continuous(breaks = seq(from = 200, to = 1400, by = 400)) +
  labs(y = 'Y') +
  theme_bw(base_size = 18)


# ベイズ予測区間の作成
qua <- y_mcmc %>% apply( 2, quantile, probs = c(0.025, 0.25, 0.50, 0.75, 0.975))
d_est <- data.frame(X = X_new, t(qua), check.names = FALSE)

# プロット
ggplot() +
  theme_bw(base_size = 18) +
  geom_ribbon(data = d_est, aes(x = X, ymin = `2.5%`, ymax = `97.5%`), fill = 'black', alpha = 1 / 6) +
  geom_ribbon(data = d_est, aes(x = X, ymin = `25%`, ymax = `75%`), fill = 'black', alpha = 2 / 6) +
  geom_line(data = d_est, aes(x = X, y = `50%`), size = 1) +
  geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 3) +
  coord_cartesian(xlim = c(22, 61), ylim = c(200, 1400)) +
  scale_y_continuous(breaks = seq(from = 200, to = 1400, by = 400)) +
  labs(y = 'Y')
