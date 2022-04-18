# **********************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 2 RとStanによるデータ分析
# Theme   : 5-2 MCMCの結果の評価（事後予測チェック）
# Date    : 2022/4/18
# Page    : P134 - P140
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# **********************************************************************************


# ＜概要＞
# - 事後予測チェックは統計モデルの事後分布と実際の分布がどの程度類似しているかを分析するもの
#   --- 統計モデルは観測したデータを生み出す確率的な過程を簡潔に記述したもの


# ＜メモ＞
# - R.4.1.3を使用する
#   --- R.4.0.3の場合は｢2-5-2-poisson-dist.stan｣の実行時にエラーが発生


# ＜目次＞
# 0 準備
# 1 データセットの準備
# 2 事後予測チェックのためのMCMCの実行
# 3 事後予測チェック
# 4 bayesplotによる事後予測チェック


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


# 1 データセットの準備 ---------------------------------------------------------

# ＜ポイント＞
# - ある小動物の発見個体数のモデル化を行う
#   --- 個体数はゼロ又は正の整数をとる


# データ準備
# --- 小動物の個数
animal_num <- read_csv("csv/2-5-1-animal-num.csv")

# データの確認
animal_num %>% as_tibble()
animal_num %>% table()
animal_num$animal_num %>% hist()


# 2 事後予測チェックのためのMCMCの実行 --------------------------------------

# ＜ポイント＞
# - 個体数はゼロ又は正の整数をとるためポアソン分布を想定する
#   --- 誤ったモデルとして正規分布を想定したモデルも想定する
# - 今回のstanファイルにはgenerated quantitiesブロックが追加されている
#   --- 乱数を得るために追加（モデル推定だけなら不要）


# データ作成
# --- rstan用のデータ形式
data_list <- list(animal_num = animal_num$animal_num, N = nrow(animal_num))

# モデル構築
# --- 正規分布仮定のモデル
mcmc_normal <-
  stan(file = "stan/2-5/2-5-1-normal-dist.stan",
       data = data_list,
       seed = 1)

# モデル構築
# --- ポアソン分布仮定のモデル
mcmc_poisson <-
  stan(file = "stan/2-5/2-5-2-poisson-dist.stan",
       data = data_list,
       seed = 1)

# 確認
# ---推定されたパラメータ
mcmc_normal %>% print(par = c("mu", "sigma", "lp__"))
mcmc_poisson %>% print(par = c("lambda", "lp__"))


# 3 事後予測チェック ----------------------------------------------------

# ＜ポイント＞
# - generated quantitiesブロックで生成したデータが観測データの分布を再現できているかを確認する
#   --- モデルが適切に表現されているかの確認


# MCMCサンプルの取得
# --- generated quantitiesブロックで生成したデータ
# --- 行列形式で出力される
y_rep_normal  <- mcmc_normal %>% rstan::extract() %>% .$pred
y_rep_poisson <- mcmc_poisson %>% rstan::extract() %>% .$pred

# 確認
y_rep_normal %>% class()
y_rep_normal[1:5, 1:5]

# 行列サイズ
# --- サンプルサイズ：200
# --- MCMCサンプル：4000（iter=2000, warmup=1000, 4chain ⇒ (2000-1000)*4 = 4000）
y_rep_normal %>% dim()

# サンプルの抽出
# --- 事後予測値の1回目のMCMCサンプルを抽出
# --- 正規分布を仮定したモデル
# --- ポアソン分布を仮定したモデル
y_rep_normal[1,]
y_rep_poisson[1,]

# ヒストグラムの確認
# --- 観測データの分布
# --- 事後予測分布（正規分布を仮定）
# --- 事後予測分布（ポアソン分布を仮定）
animal_num$animal_num %>% hist()
y_rep_normal[1,] %>% hist()
y_rep_poisson[1,] %>% hist()


# 4 bayesplotによる事後予測チェック --------------------------------------

# ＜ポイント＞
# - 観測データの分布と事後分布をヒストグラムや密度プロットで比較する
#   --- {bayesplot}はrstanの結果を可視化するための関数が揃っている


# ヒストグラム
# --- 正規分布を仮定したモデル
# --- ポアソン分布を仮定したモデル
animal_num$animal_num %>% ppc_hist(yrep = y_rep_normal[1:5, ])
animal_num$animal_num %>% ppc_hist(yrep = y_rep_poisson[1:5, ])

# 密度プロット
# --- 正規分布を仮定したモデル
# --- ポアソン分布を仮定したモデル
animal_num$animal_num %>% ppc_dens(yrep = y_rep_normal[1:10, ])
animal_num$animal_num %>% ppc_dens(yrep = y_rep_poisson[1:10, ])

# 複数の密度プロット
# --- 正規分布を仮定したモデル
# --- ポアソン分布を仮定したモデル
animal_num$animal_num %>% ppc_dens_overlay(yrep = y_rep_normal[1:10, ])
animal_num$animal_num %>% ppc_dens_overlay(yrep = y_rep_poisson[1:10, ])
