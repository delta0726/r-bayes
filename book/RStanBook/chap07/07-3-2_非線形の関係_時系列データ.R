# ***********************************************************************************************
# Title   : StanとRでベイズ統計モデリング
# Theme   : 3 発展偏
# Chapter : 7 回帰分析の悩みどころ（非線形の関係・時系列データ）
# Date    : 2022/05/10
# Page    : P106 - P111
# URL     : https://github.com/MatsuuraKentaro/RStanBook
# ***********************************************************************************************


# ＜概要＞
# - 時系列データは非線形なケースが多く当てはめが難しい
#   --- 物理法則などがあればそれに従う


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 モデル構築
# 3 予測分布の作成
# 4 時系列における曲線の例


# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(gridExtra)
library(magrittr)
library(rstan)


# データロード
d <- read_csv(file = 'chap07/csv/data-conc.txt')


# 1 データ確認 ------------------------------------------------------------------

# ＜ポイント＞
# - 気温(X)と電力消費量(Y)の関係
#   --- 二次曲線での当てはめが妥当


# データ確認
d %>% print()

# プロット作成
d %>%
  ggplot(aes(x = Time, y = Y)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  labs(x = 'Time (hour)', y = 'Y') +
  scale_x_continuous(breaks = d$Time, limit = c(0, 24)) +
  ylim(-2.5, 16) +
  theme_bw(base_size = 18)


# 2 モデル構築 ----------------------------------------------------------------

# 予測用データの作成
T_new <- 60
Time_new <- seq(from = 0, to = 24, length = T_new)

# Stan用データの作成
data <- list(T = nrow(d), Time = d$Time, Y = d$Y, T_new = T_new, Time_new = Time_new)

# モデル構築
fit <- stan(file = 'chap07/stan/model7-4.stan', data = data, seed = 1234)


# 3 予測分布の作成 -------------------------------------------------------------

# データ抽出
ms <- fit %>% rstan::extract()

# プロットデータの作成
d_est <-
  ms$y_new %>%
    apply(2, quantile, prob = c(0.025, 0.25, 0.5, 0.75, 0.975)) %>%
    t() %>%
    as.data.frame() %>%
    mutate(X = Time_new) %>%
    as_tibble()

# プロット作成
# --- 散布図 + 予測分布
ggplot() +
  geom_ribbon(data = d_est, aes(x = X, ymin = `2.5%`, ymax = `97.5%`), fill = 'black', alpha = 1 / 6) +
  geom_ribbon(data = d_est, aes(x = X, ymin = `25%`, ymax = `75%`), fill = 'black', alpha = 2 / 6) +
  geom_line(data = d_est, aes(x = X, y = `50%`), size = 0.5) +
  geom_point(data = d, aes(x = Time, y = Y), size = 3) +
  labs(x = 'Time (hour)', y = 'Y') +
  scale_x_continuous(breaks = d$Time, limit = c(0, 24)) +
  ylim(-2.5, 16) +
  theme_bw(base_size = 18)


# 4 時系列における曲線の例 -------------------------------------------------------------

data.frame(X = c(0, 5)) %>%
  ggplot(aes(x = X)) +
  stat_function(fun = function(x) 2 * exp(-1 * x), aes(linetype = '1'), size = 1) +
  stat_function(fun = function(x) 1.8 / (1 + 50 * exp(-2 * x)), aes(linetype = '2'), size = 1) +
  stat_function(fun = function(x) 8 * (exp(-x) - exp(-2 * x)), aes(linetype = '3'), size = 1) +
  scale_linetype_manual(values = c('solid', '52', '12')) +
  labs(linetype = 'Model', x = 'Time', y = 'y') +
  theme_bw(base_size = 18) +
  theme(legend.key.width = grid::unit(3, 'line'))

