# ***********************************************************************************************
# Title   : StanとRでベイズ統計モデリング
# Theme   : 2 Stan入門編
# Chapter : 4-4 StanとRStanをはじめよう（stanを使ったスマートなコード）
# Date    : 2022/05/08
# Page    : P49 - P52
# URL     : https://github.com/MatsuuraKentaro/RStanBook
# ***********************************************************************************************


# ＜概要＞
# - 前回の｢ベイズ信頼区間とベイズ予想区間の算出｣ではStan側でMCMCサンプルを使ってR側で様々な量を計算した
#   コード量が多くなったり速度面で課題があるので、処理はできるだけstan側で実行するための工夫をする
#   --- transformed parametersブロックとgenerated quantitiesブロックを使用する


# ＜目次＞
# 0 準備
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


# 1 モデル構築 ---------------------------------------------------------------------------

# 予測用データ
X_new <- 23:60

# Stan用のデータ
data <-
  list(N = nrow(d),
       X = d$X,
       Y = d$Y,
       N_new = length(X_new),
       X_new = X_new)

# モデル構築
fit <- stan(file = 'chap04/stan/model4-4.stan', data = data, seed = 1234)


# 2 ベイズ信用区間のプロット --------------------------------------------------------------

# データ抽出
ms <- fit %>% rstan::extract()

# 分位点データの作成
qua <- ms$y_base_new %>% apply( 2, quantile, probs = c(0.025, 0.25, 0.50, 0.75, 0.975))
d_est <- data.frame(X = X_new, t(qua), check.names = FALSE)

# プロット作成
ggplot() +
  geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 3) +
  geom_line(data = d_est, aes(x = X, y = `50%`), size = 1) +
  geom_ribbon(data = d_est, aes(x = X, ymin = `2.5%`, ymax = `97.5%`), fill = 'black', alpha = 1 / 6) +
  geom_ribbon(data = d_est, aes(x = X, ymin = `25%`, ymax = `75%`), fill = 'black', alpha = 2 / 6) +
  coord_cartesian(xlim = c(22, 61), ylim = c(200, 1400)) +
  scale_y_continuous(breaks = seq(from = 200, to = 1400, by = 400)) +
  labs(y = 'Y') +
  theme_bw(base_size = 18)


# 3 ベイズ予測区間のプロット --------------------------------------------------------------

# データ抽出
ms <- fit %>% rstan::extract()

# 分位点データの作成
qua <- ms$y_new %>% apply( 2, quantile, probs = c(0.025, 0.25, 0.50, 0.75, 0.975))
d_est <- data.frame(X = X_new, t(qua), check.names = FALSE)

# プロット作成
ggplot() +
  geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 3) +
  geom_line(data = d_est, aes(x = X, y = `50%`), size = 1) +
  geom_ribbon(data = d_est, aes(x = X, ymin = `2.5%`, ymax = `97.5%`), fill = 'black', alpha = 1 / 6) +
  geom_ribbon(data = d_est, aes(x = X, ymin = `25%`, ymax = `75%`), fill = 'black', alpha = 2 / 6) +
  coord_cartesian(xlim = c(22, 61), ylim = c(200, 1400)) +
  scale_y_continuous(breaks = seq(from = 200, to = 1400, by = 400)) +
  labs(y = 'Y') +
  theme_bw(base_size = 18)
