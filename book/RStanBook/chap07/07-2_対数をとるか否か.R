# ***********************************************************************************************
# Title   : StanとRでベイズ統計モデリング
# Theme   : 3 発展偏
# Chapter : 7 回帰分析の悩みどころ（対数を取るか否か）
# Date    : 2022/05/00
# Page    : P104 - P106
# URL     : https://github.com/MatsuuraKentaro/RStanBook
# ***********************************************************************************************


# ＜概要＞
# - 応答変数や説明変数の変数変換で解釈が難しくなる場合には変換すべきではない
#   --- ただし、対数変換はしばしば有用なので性質を理解すると有用（分布の正規化）


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 モデル構築
# 3 予測分布の作成
# 4 予測値と実測値の比較
# 5 ノイズの分布


# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(gridExtra)
library(magrittr)
library(rstan)


# データロード
d_raw <- read_csv(file = 'chap07/csv/data-rental.txt')
d <- d_raw %>% set_colnames(c('Y', 'X'))


# 1 データ確認 ------------------------------------------------------------------

# ＜ポイント＞
# - 対数軸を使用したほうが系列の密集度合いを緩和することができる
#   --- 対数変換により正規分布に近づけることができる


# 散布図(通常軸)
p1 <-
  d_raw %>%
    ggplot(aes(x = Area, y = Y)) +
    geom_point(shape = 1, size = 2) +
    scale_x_continuous(breaks = seq(from = 20, to = 120, by = 20)) +
    theme_bw(base_size = 18)

# 散布図(対数軸)
p2 <-
  d_raw %>%
    ggplot(aes(x = Area, y = Y)) +
    geom_point(shape = 1, size = 2) +
    scale_x_log10(breaks = c(1, 2, 5, 10, 20, 50, 100)) +
    scale_y_log10(breaks = c(10, 20, 50, 100, 200, 500, 1000, 2000)) +
    coord_cartesian(xlim = c(9, 120)) +
    theme_bw(base_size = 18)

# プロット表示
grid.arrange(p1, p2, nrow = 1)


# 2 モデル構築 --------------------------------------------------------------------

# ＜ポイント＞
# - 2種類の線形回帰モデルをstanを用いて定義する
#   --- 元データをそのまま使うモデル
#   --- YおよびAreaの対数を取ったモデル


# 予測用データ
X_new <- seq(from = 10, to = 120, length = 50)

# Stan用データ
data_1 <- list(N = nrow(d), Area = d$X, Y = d$Y, N_new = 50, Area_new = X_new)
data_2 <- list(N = nrow(d), Area = log10(d$X), Y = log10(d$Y), N_new = 50, Area_new = log10(X_new))

# モデル構築
fit_1 <- stan(file = 'chap07/stan/model7-1.stan', data = data_1, seed = 1234)
fit_2 <- stan(file = 'chap07/stan/model7-2.stan', data = data_2, seed = 1234)


# 3 予測分布の作成 --------------------------------------------------------------

# ＜ポイント＞
# - MCMCの4000サンプルのデータを用いて実際の分位点を計算
# - 散布図に予測区間をプロットして2つのモデルの違いを確認する
#   --- モデル1は信頼区間が一定
#   --- モデル2はAreaが大きくなるほど信頼区間が拡大


# データ抽出
ms_1 <- fit_1 %>% rstan::extract()
ms_2 <- fit_2 %>% rstan::extract()

# 使用データの確認
# --- 列ごとにアウト・オブ・サンプルの予測値が50個はいっている
# --- 行は4000レコード
ms_1$y_new %>% head()
ms_2$y_new %>% head()

# ベイズ予測区間を計算
# --- 予測値の分位点取得
# --- 列ごとに計算
qua_1 <- ms_1$y_new %>% apply(2, quantile, probs = c(0.1, 0.25, 0.50, 0.75, 0.9))
qua_2 <- 10^ms_2$y_new %>% apply(2, quantile, probs = c(0.1, 0.25, 0.50, 0.75, 0.9))

# プロット用データの作成
d_est_1 <- data.frame(X = X_new, t(qua_1), check.names = FALSE)
d_est_2 <- data.frame(X = X_new, t(qua_2), check.names = FALSE)

# プロット作成（モデル1）
# --- 線形回帰のため信頼区間が一定
p1 <-
  ggplot() +
    geom_line(data = d_est_1, aes(x = X, y = `50%`), size = 1) +
    geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 2) +
    geom_ribbon(data = d_est_1, aes(x = X, ymin = `10%`, ymax = `90%`), fill = 'black', alpha = 1 / 6) +
    geom_ribbon(data = d_est_1, aes(x = X, ymin = `25%`, ymax = `75%`), fill = 'black', alpha = 2 / 6) +
    coord_cartesian(xlim = c(11, 118), ylim = c(-50, 1900)) +
    labs(x = 'Area', y = 'Y') +
    theme_bw(base_size = 18)

# プロット作成（モデル2）
# --- 対数変換して回帰しているためXが大きいほど信頼区間が拡大
p2 <-
  ggplot() +
    geom_line(data = d_est_2, aes(x = X, y = `50%`), size = 1) +
    geom_point(data = d, aes(x = X, y = Y), shape = 1, size = 2) +
    geom_ribbon(data = d_est_2, aes(x = X, ymin = `10%`, ymax = `90%`), fill = 'black', alpha = 1 / 6) +
    geom_ribbon(data = d_est_2, aes(x = X, ymin = `25%`, ymax = `75%`), fill = 'black', alpha = 2 / 6) +
    coord_cartesian(xlim = c(11, 118), ylim = c(-50, 1900)) +
    labs(x = 'Area', y = 'Y') +
    theme_bw(base_size = 18)

# プロット表示
grid.arrange(p1, p2, nrow = 2)


# 4 予測値と実測値の比較 ----------------------------------------------------------------

# データ抽出
ms_1 <- fit_1 %>% rstan::extract()
ms_2 <- fit_2 %>% rstan::extract()

# 使用データの確認
# --- 列ごとにイン・オブ・サンプルの予測値が100個はいっている
# --- 行は4000レコード
ms_1$y_pred %>% head()
ms_2$y_pred %>% head()

# ベイズ予測区間を計算
# --- 予測値の分位点取得
# --- 列ごとに計算
qua_1 <- ms_1$y_pred %>% apply(2, quantile, probs = c(0.1, 0.25, 0.50, 0.75, 0.9))
qua_2 <- 10^ms_2$y_pred %>% apply(2, quantile, probs = c(0.1, 0.25, 0.50, 0.75, 0.9))

# プロット用データの作成
d_est_1 <- data.frame(X = X_new, t(qua_1), check.names = FALSE)
d_est_2 <- data.frame(X = X_new, t(qua_2), check.names = FALSE)

# プロット作成（モデル1）
p1 <-
  d_est_1 %>%
    ggplot(aes(x = X, y = `50%`)) +
    geom_pointrange(aes(ymin = `10%`, ymax = `90%`), color = 'grey5', fill = 'grey95', shape = 21) +
    geom_abline(aes(slope = 1, intercept = 0), color = 'black', alpha = 3 / 5, linetype = 'dashed') +
    coord_fixed(ratio = 1, xlim = c(-50, 1900), ylim = c(-50, 1900)) +
    labs(x = 'Observed', y = 'Predicted') +
    theme_bw(base_size = 18)

# プロット作成（モデル2）
p2 <-
  d_est_2 %>%
    ggplot(aes(x = X, y = `50%`)) +
    geom_pointrange(aes(ymin = `10%`, ymax = `90%`), color = 'grey5', fill = 'grey95', shape = 21) +
    geom_abline(aes(slope = 1, intercept = 0), color = 'black', alpha = 3 / 5, linetype = 'dashed') +
    coord_fixed(ratio = 1, xlim = c(-50, 1900), ylim = c(-50, 1900)) +
    labs(x = 'Observed', y = 'Predicted') +
    theme_bw(base_size = 18)

# プロット表示
grid.arrange(p1, p2, nrow = 1)


# 5 ノイズの分布 -----------------------------------------------------------------------

# 関数定義
calc_mode <- function(x) {
  dens <- density(x)
  mode_i <- which.max(dens$y)
  mode_x <- dens$x[mode_i]
  mode_y <- dens$y[mode_i]
  c(mode_x, mode_y)
}

# データ抽出
ms_1 <- fit_1 %>% rstan::extract()
ms_2 <- fit_2 %>% rstan::extract()

# サンプル数
N_mcmc_1 <- ms_1$lp__ %>% length()
N_mcmc_2 <- ms_2$lp__ %>% length()

noise_mcmc_1 <- t(replicate(N_mcmc_1, d$Y)) - ms_1$mu
noise_mcmc_2 <- t(replicate(N_mcmc_2, log10(d$Y))) - ms_2$mu


d_mode_1 <-
  noise_mcmc_1 %>%
    apply(2, calc_mode) %>%
    t() %>%
    data.frame() %>%
    magrittr::set_colnames(c('X', 'Y'))

d_mode_2 <-
  noise_mcmc_2 %>%
    apply(2, calc_mode) %>%
    t() %>%
    data.frame() %>%
    magrittr::set_colnames(c('X', 'Y'))


s_dens_1 <- density(ms_1$s)
s_dens_2 <- density(ms_2$s)

s_MAP_1 <- s_dens_1$x[which.max(s_dens_1$y)]
s_MAP_2 <- s_dens_2$x[which.max(s_dens_2$y)]

bw_1 <- 25
bw_2 <- 0.02

p1 <-
  d_mode_1 %>%
    ggplot(aes(x = X)) +
    geom_histogram(binwidth = bw_1, color = 'black', fill = 'white') +
    geom_density(eval(bquote(aes(y = ..count.. * .(bw_1)))), alpha = 0.5, color = 'black', fill = 'gray20') +
    geom_rug(sides = 'b') +
    stat_function(fun = function(x) nrow(d) * bw_1 * dnorm(x, mean = 0, sd = s_MAP_1), linetype = 'dashed') +
    labs(x = 'value', y = 'count') +
    scale_x_continuous(breaks = seq(from = -400, to = 400, by = 200), limits = range(density(d_mode_1$X)$x)) +
    scale_y_continuous(breaks = seq(from = 0, to = 16, by = 2)) +
    theme_bw(base_size = 18)


p2 <-
  d_mode_2 %>%
    ggplot(aes(x = X)) +
    geom_histogram(binwidth = bw_2, color = 'black', fill = 'white') +
    geom_density(eval(bquote(aes(y = ..count.. * .(bw_2)))), alpha = 0.5, color = 'black', fill = 'gray20') +
    geom_rug(sides = 'b') +
    stat_function(fun = function(x) nrow(d) * bw_2 * dnorm(x, mean = 0, sd = s_MAP_2), linetype = 'dashed') +
    labs(x = 'value', y = 'count') +
    scale_y_continuous(breaks = seq(from = 0, to = 12, by = 2)) +
    scale_x_continuous(breaks = seq(from = -0.25, to = 0.25, by = 0.25), limits = range(density(d_mode_2$X)$x)) +
    theme_bw(base_size = 18)


# プロット表示
grid.arrange(p1, p2, nrow = 2)
