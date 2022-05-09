# ***********************************************************************************************
# Title   : StanとRでベイズ統計モデリング
# Theme   : 3 発展偏
# Chapter : 7 回帰分析の悩みどころ（非線形の関係・断面データ）
# Date    : 2022/05/10
# Page    : P106 - P111
# URL     : https://github.com/MatsuuraKentaro/RStanBook
# ***********************************************************************************************


# ＜概要＞
# - データによっては線形な直線を当てはめて回帰するのではなく、非線形の曲線を当てはめたほうが良い場合がある
#   --- どのようなメカニズムでデータが生成されるのかは不明な場合が多い
#   --- なるべくシンプルで解釈しやすい曲線を当てはめる


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 モデル構築
# 3 予測分布の作成


# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(gridExtra)
library(magrittr)
library(rstan)


# データロード
d <- read_csv(file = 'chap07/csv/data-aircon.txt')


# 1 データ確認 ------------------------------------------------------------------

# ＜ポイント＞
# - 気温(X)と電力消費量(Y)の関係
#   --- 二次曲線での当てはめが妥当


# データ確認
d %>% print()

# プロット作成
d %>%
  ggplot(data =, aes(x = X, y = Y)) +
  geom_point(shape = 1, size = 2) +
  labs(x = 'X', y = 'Y') +
  scale_x_continuous(limits = c(-3, 32)) +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 50), limits = c(0, 130)) +
  theme_bw(base_size = 18)


# 2 モデル構築 ----------------------------------------------------------------

# 予測用データの作成
N_new <- 60
X_new <- seq(from = -3, to = 32, length = N_new)

# Stan用データの作成
data <- list(N = nrow(d), X = d$X, Y = d$Y, N_new = N_new, X_new = X_new)

# モデル構築
fit <- stan(file = 'chap07/stan/model7-3.stan', data = data, seed = 1234)


# 3 予測分布の作成 -------------------------------------------------------------

# データ抽出
ms <- fit %>% rstan::extract()

# プロットデータの作成
d_est <-
  ms$y_new %>%
    apply( 2, quantile, prob = c(0.025, 0.25, 0.5, 0.75, 0.975)) %>%
    t() %>%
    as.data.frame() %>%
    mutate(X = X_new) %>%
    as_tibble()

# プロット作成
# --- 散布図 + 予測分布
ggplot() +
  geom_line(data = d_est, aes(x = X, y = `50%`), size = 0.5) +
  geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 2) +
  geom_ribbon(data = d_est, aes(x = X, ymin = `2.5%`, ymax = `97.5%`), fill = 'black', alpha = 1 / 6) +
  geom_ribbon(data = d_est, aes(x = X, ymin = `25%`, ymax = `75%`), fill = 'black', alpha = 2 / 6) +
  labs(x = 'X', y = 'Y') +
  scale_y_continuous(breaks = seq(from = 0, to = 100, by = 50), limits = c(0, 130)) +
  theme_bw(base_size = 18)
