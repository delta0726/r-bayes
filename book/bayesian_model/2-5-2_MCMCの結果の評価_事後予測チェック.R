#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 2-5-2 MCMCの結果の評価（事後予測チェック）
# Objective : TODO
# Created by: Owner
# Created on: 2021/4/12
# Page      : P134 - P140
#***************************************************************************************


# ＜テーマ＞
# - MCMCサンプルの取扱いと事後チェック
# - {bayesplot}を用いた描画方法


# ＜目次＞
# 0 準備
# 1 MCMCの実行
# 2 MCMCサンプルの構造
# 3 MCMCサンプルの代表値の計算


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(rstan)
library(bayesplot)
library(ggfortify)


# オプション設定
# --- 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# データ準備
# --- 小動物の個数
animal_num        <- read_csv("book/bayesian_model/csv/2-5-1-animal-num.csv")

# データの確認
animal_num %>% as_tibble()
animal_num$animal_num %>% hist()


# 1 MCMCの実行 ----------------------------------------------------------------

# データ作成
data_list <- list(animal_num = animal_num$animal_num, N = nrow(animal_num))

# MCMCの実行
# --- 正規分布仮定のモデル
mcmc_normal <-
  stan(file = "book/bayesian_model/stan/2-5-1-normal-dist.stan",
       data = data_list,
       seed = 1)

# MCMCの実行：ポアソン分布仮定のモデル
mcmc_poisson <-
  stan(file = "2-5-2-poisson-dist.stan",
       data = data_list,
       seed = 1)

# 参考：推定されたパラメタ
mcmc_normal %>% print(par = c("mu", "sigma", "lp__"))
mcmc_poisson %>% print(par = c("lambda", "lp__"))


# 事後予測チェックの実施 -------------------------------------------------------------


# 事後予測値のMCMCサンプルの取得
y_rep_normal <- rstan::extract(mcmc_normal)$pred
y_rep_poisson <- rstan::extract(mcmc_poisson)$pred

# サンプルサイズ(nrow(animal_num))は200
# 4000回分のMCMCサンプル
y_rep_normal %>% dim()


# 事後予測値の1回目のMCMCサンプルを抽出
# 正規分布を仮定したモデル
y_rep_normal[1,]
# ポアソン分布を仮定したモデル
y_rep_poisson[1,]

# 参考；観測データの分布と、事後予測分布の比較
animal_num$animal_num %>% hist() # 観測データの分布
y_rep_normal[1,] %>% hist()      # 正規分布を仮定した事後予測分布
y_rep_poisson[1,] %>% hist()     # ポアソン分布を仮定した事後予測分布

# 元データのヒストグラムと、
# 1~5回分のMCMCサンプルの事後予測値のヒストグラム

# 正規分布を仮定したモデル
animal_num$animal_num %>%
  ppc_hist(yrep = y_rep_normal[1:5, ])

# ポアソン分布を仮定したモデル
animal_num$animal_num %>%
  ppc_hist(yrep = y_rep_poisson[1:5, ])


# ヒストグラムの代わりにカーネル密度推定を利用した結果

# 正規分布を仮定したモデル
animal_num$animal_num %>%
  ppc_dens(yrep = y_rep_normal[1:10, ])

# ポアソン分布を仮定したモデル
animal_num$animal_num %>%
  ppc_dens(yrep = y_rep_poisson[1:10, ])


# 正規分布を仮定したモデル
animal_num$animal_num %>%
  ppc_dens_overlay(yrep = y_rep_normal[1:10, ])

# ポアソン分布を仮定したモデル
animal_num$animal_num %>%
  ppc_dens_overlay(yrep = y_rep_poisson[1:10, ])

