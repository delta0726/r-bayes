#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 2-6 Stanコーディングの詳細
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/10
# Page      : P141 - P150
#***************************************************************************************


# ＜テーマ＞
# - Stanのコーディング方法


# ＜目次＞
# 0 準備
# 1 共通データの確認
# 2 サンプリング文
# 3 対数密度加算文
# 4 平均値の差の評価とgenerated quantitiesブロック


# 0 準備 --------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(rstan)
library(bayesplot)


# オプション設定
# --- 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


# データロード
file_beer_sales_1 <- read_csv("book/bayesian_model/csv/2-4-1-beer-sales-1.csv")
file_beer_sales_ab <- read_csv("book/bayesian_model/csv/2-6-1-beer-sales-ab.csv")


# 1 共通データの確認 --------------------------------------------------------------------

# データ確認
file_beer_sales_1 %>% print()

# データ作成
data_list <-
  list(sales = file_beer_sales_1$sales,
       N     = nrow(file_beer_sales_1))

# 動作確認
# --- 乱数の生成(3章と同じモデル)
mcmc_result_1 <-
  stan(file = "book/bayesian_model/stan/2-4-1-calc-mean-variance.stan",
       data = data_list,
       seed = 1)

# 結果確認
mcmc_result_1 %>% print(probs = c(0.025, 0.5, 0.975))


# 2 サンプリング文 ----------------------------------------------------------------

# ＜ポイント＞
# - Stanファイルのmodelブロックにサンプリングのコードを記述する
#   --- MCMCサンプルの生成ではなく、確率分布(正規分布)を指定するサンプリング


# 乱数の生成
# --- 正規分布に従う事前分布を指定
mcmc_result_2 <-
  stan(file = "book/bayesian_model/stan/2-6-1-normal-prior.stan",
       data = data_list,
       seed = 1)

# 確認
mcmc_result_2 %>% print(probs = c(0.025, 0.5, 0.975))


# 3 対数密度加算文 ----------------------------------------------------------------

# モデル構築
# --- 乱数の生成(対数密度加算文の使用)
mcmc_result_3 <-
  stan(file = "book/bayesian_model/stan/2-6-2-lp.stan",
       data = data_list,
       seed = 1)

# 確認
mcmc_result_3 %>% print(probs = c(0.025, 0.5, 0.975))


# 乱数の生成
# --- 対数密度加算文の使用、事前分布を正規分布にした
mcmc_result_4 <-
  stan(file = "book/bayesian_model/stan/2-6-3-lp-normal-prior.stan",
       data = data_list,
       seed = 1)

# 確認
mcmc_result_4 %>% print(probs = c(0.025, 0.5, 0.975))


# 乱数の生成
# --- 対数密度加算文の使用、ベクトル化
mcmc_result_5 <-
  stan(file = "book/bayesian_model/stan/2-6-4-lp-normal-prior-vec.stan",
       data = data_list,
       seed = 1)

# 確認
mcmc_result_5 %>% print(probs = c(0.025, 0.5, 0.975))


# 4 平均値の差の評価とgenerated quantitiesブロック --------------------------------------

# データ確認
file_beer_sales_ab %>% print()
file_beer_sales_ab %>% glimpse()
file_beer_sales_ab$beer_name %>% table()


# 分析対象のデータ読み込み
file_beer_sales_ab %>% head(n = 3)


# プロット作成
# --- ビールの種類別のヒストグラム
file_beer_sales_ab %>%
  ggplot(aes(x = sales, y = ..density..,
             color = beer_name, fill = beer_name)) +
  geom_histogram(alpha = 0.5, position = "identity")+
  geom_density(alpha = 0.5, size = 0)

# データ作成
# --- ビールの種類別にデータを分ける
data_list_ab <-
  list(sales_a = file_beer_sales_ab$sales[1:100],
       sales_b = file_beer_sales_ab$sales[101:200],
       N       = 100)

# モデル構築
# --- 乱数の生成
mcmc_result_6 <-
  stan(file = "book/bayesian_model/stan/2-6-5-difference-mean.stan",
       data = data_list_ab,
       seed = 1)

# 確認
mcmc_result_6 %>% print(probs = c(0.025, 0.5, 0.975))


# 参考
mcmc_sample <- rstan::extract(mcmc_result_6, permuted = FALSE)
mcmc_sample %>% mcmc_dens(pars = "diff")

