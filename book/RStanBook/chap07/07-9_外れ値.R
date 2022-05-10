# ***********************************************************************************************
# Title   : StanとRでベイズ統計モデリング
# Theme   : 3 発展偏
# Chapter : 7 回帰分析の悩みどころ（外れ値）
# Date    : 2022/05/11
# Page    : P118 - P120
# URL     : https://github.com/MatsuuraKentaro/RStanBook
# ***********************************************************************************************


# ＜概要＞
# - 外れ値のモデリングにおける対処法を考える
#   --- 今回は裾の長いコーシー分布やStudentのt分布を用いることを検討する


# ＜目次＞
# 0 準備
# 1 データ変換
# 2 モデル構築


# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(gridExtra)
library(magrittr)
library(rstan)


# データロード
d <- read_csv(file = 'chap07/csv/data-outlier.txt')


# 1 データ確認 -------------------------------------------------------------------

# データ確認
d %>% print()

# プロット作成
# --- 外れ値の存在を確認
d %>%
  ggplot(aes(x = X, y = Y)) +
  geom_point(shape = 1, size = 3) +
  labs(x = 'X', y = 'Y') +
  coord_cartesian(xlim = c(-0.2, 11.2), ylim = c(-25, 75)) +
  theme_bw(base_size = 18)


# 2 モデル構築 -------------------------------------------------------------------

# ＜ポイント＞
# - ノイズの背景を｢たいていの場合は平均値付近で生成されるが、まれに大きな値を生成する｣と仮定
#   --- 裾の長い分布を使用する（コーシー分布）


# 予測用データの作成
X_new <- seq(from = 0, to = 11, length = 101)

# Stanデータの作成
data <- list(N = nrow(d), X = d$X, Y = d$Y, N_new = length(X_new), X_new = X_new)

# 学習
fit_1 <- stan(file = 'chap07/stan/model7-8.stan', data = data, seed = 1234)
fit_2 <- stan(file = 'chap07/stan/model7-9.stan', data = data, seed = 1234)

# 確認
fit_1 %>% print()
fit_2 %>% print()


# 3 信頼区間の比較 ---------------------------------------------------------------

# ＜ポイント＞
# - 正規分布の場合は予測分布の区間が外れ値に引っ張られて拡大する
# - コーシー分布の場合は外れ値の影響が小さくなっている


# 関数定義
# --- 信頼区間データの作成
define_est_data <- function(x, x_new){
  result <-
    x$y_new %>%
      apply( 2, quantile, prob = c(0.025, 0.25, 0.5, 0.75, 0.975)) %>%
      t() %>%
      as.data.frame() %>%
      mutate(X = x_new) %>%
      as_tibble()
  return(result)
}

# データ抽出
ms_1 <- fit_1 %>% rstan::extract()
ms_2 <- fit_2 %>% rstan::extract()

# 信頼区間データの作成
d_est_1 <- ms_1 %>% define_est_data(X_new)
d_est_2 <- ms_2 %>% define_est_data(X_new)

# プロット作成
# --- 正規分布を想定した場合
p1 <- ggplot() +
  geom_ribbon(data = d_est_1, aes(x = X, ymin = `2.5%`, ymax = `97.5%`), fill = 'black', alpha = 1 / 6) +
  geom_ribbon(data = d_est_1, aes(x = X, ymin = `25%`, ymax = `75%`), fill = 'black', alpha = 2 / 6) +
  geom_line(data = d_est_1, aes(x = X, y = `50%`), size = 1) +
  geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 3) +
  labs(x = 'X', y = 'Y') +
  coord_cartesian(xlim = c(-0.2, 11.2), ylim = c(-25, 75)) +
  theme_bw(base_size = 18)

# プロット作成
# --- コーシー分布を想定した場合
p2 <- ggplot() +
  geom_ribbon(data = d_est_2, aes(x = X, ymin = `2.5%`, ymax = `97.5%`), fill = 'black', alpha = 1 / 6) +
  geom_ribbon(data = d_est_2, aes(x = X, ymin = `25%`, ymax = `75%`), fill = 'black', alpha = 2 / 6) +
  geom_line(data = d_est_2, aes(x = X, y = `50%`), size = 1) +
  geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 3) +
  labs(x = 'X', y = 'Y') +
  coord_cartesian(xlim = c(-0.2, 11.2), ylim = c(-25, 75)) +
  theme_bw(base_size = 18)

# プロット表示
grid.arrange(p1, p2, nrow = 2)
