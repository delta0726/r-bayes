# ***********************************************************************************************
# Library   : tidybayes
# Title     : StanのMCMC結果&パラメータ結果を可視化する
# Created by: Owner
# Created on: 2021/04/06
# URL       : https://knknkn.hatenablog.com/entry/2019/05/19/182053
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

# ライブラリ
library(readxl)
library(tidybayes)
library(tidyverse)
library(magrittr)
library(ggstance)
library(emmeans)
library(brms)
library(rstan)
library(bayesplot)


# データ取り込み
dat <- read_excel("library/tidybayes/blog/data/H25fukushima_suicide.xlsx")

# データ確認
dat %>% print()
dat %>% glimpse()


# 1 Stanの準備 -----------------------------------------------------------------------

# ＜ポイント＞
# - 市区町村の標準化死亡比を推定するpoisson-gammaモデルを実行


# オプション設定
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

# モデル取り込み
# --- 日本語コメントがあるとエラー
modelGP <- stan_model("library/tidybayes/blog/stan/HPGmodel.stan")

# 確認
modelGP %>% print()


# 2 Stanによる推定 --------------------------------------------------------------------

# 推定
fit <-
  modelGP %>%
    sampling(data = list(d     = dat$d,
                         exp_d = dat$exp_d,
                         r     = nrow(dat)))



# 変数抽出（wideフォーマット）
# --- chain/iteration/drawごと、地域(r)ごとのパラメータの推定結果を表示
fit %>%
  spread_draws(theta[r], alpha, beta)


# 変数抽出（longフォーマット）
# --- spread_draws()をロング型にしたもの
# --- ggplotで可視化するときに便利
fit %>%
  gather_draws(theta[r], alpha, beta)


# 3 事後分布の要約統計量系関数 ----------------------------------------------------------

# ＜ポイント＞
# - spread_draws()で整然化されたデータを以下の関数に流すことで事後分布の統計量を取得

# ＜関数＞
# median_hdci : 中央値で最高密度区間
# mean_hdci   : 平均値で最高密度区間
# mode_hdci   : 最頻値で最高密度区間
# median_qi   : 中央値でパーセンタイル
# mean_qi     ：平均値でパーセンタイル
# mode_qi     ：最頻値でパーセンタイル


# 事後分布の中央値と最高密度区間を抽出
fit %>%
  spread_draws(theta[r]) %>%
  median_hdci()


# 4 事後分布のプロット -----------------------------------------------------------------

# ＜関数＞
# geom_eyeh     ： 事後分布を両側に出力
# geom_harlfeyeh： 事後分布を片側だけ出力
# ※どちらも事後分布と共に、点推定値と確信区間が表示


# プロット作成
# --- 事後分布を両側に出力
fit %>%
  spread_draws(theta[r]) %>%
  filter(between(r, 1, 2)) %>%
  ggplot()+
  stat_eye(aes(x = theta, y = as.factor(r)), point_interval = median_hdci,
           .width = c(.66, 0.96)) +
  ylab("City")

# プロット作成
# --- 事後分布を片側だけ出力
fit %>%
  spread_draws(theta[r]) %>%
  filter(between(r, 1, 2)) %>%
  ggplot() +
  stat_halfeye(aes(x = theta, y = as.factor(r)), point_interval = median_hdci,
               .width = c(.66, 0.96)) +
  ylab("City")

# プロット作成
# --- 線だけで分布を表現
fit %>%
  spread_draws(theta[r]) %>%
  filter(between(r, 1, 2)) %>%
  ggplot() +
  stat_pointinterval(aes(y = r, x = theta),
                     point_interval = median_hdci,.width = c(.66,0.96))


# 5 他のライブラリとの橋渡し ---------------------------------------------------------

# ＜ポイント＞
# - tidybayesパッケージでは、収束診断系の統計指標やプロットのための関数が用意されていない
# - {bayesplot}など他のライブラリを活用


# 収束結果の確認
fit %>%
  spread_draws(theta[r]) %>%
  filter(.chain != 3) %>%
  unspread_draws(theta[r], drop_indices = T) %>%
  mcmc_acf(pars = c("theta[1]", "theta[2]"))

# トレースプロットの作成
fit %>%
  spread_draws(theta[r]) %>%
  filter(.chain != 3, between(r, 1, 3)) %>%
  ggplot(aes(x = .iteration, y = theta, group = as.factor(.chain),
             color = as.factor(.chain))) +
  geom_line(alpha = 0.5) +
  facet_wrap(~r, ncol = 1)
