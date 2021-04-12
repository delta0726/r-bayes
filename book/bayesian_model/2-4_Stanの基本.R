#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 2-4 stanの基本
# Objective : TODO
# Created by: Owner
# Created on: 2021/2/10
# Page      : P111 - P124
#***************************************************************************************


# ＜用語整理＞
# - ｢サンプル｣と｢MCMCサンプル｣の違いは明確にしておく
# - ｢サンプル｣とは、母集団から標本抽出することを指す
# - ｢MCMCサンプル｣とは、MCMCを用いて得られた乱数を指す
#   --- MCMCを使うことで、事後分布に従う乱数を得ることができる


# ＜stanの構成＞
# - dataブロック      ： 使用されるデータやサンプルサイズなどの情報を記述
# - parameterブロック ： 事後分布を得たいパラメータの一覧を定義
# - modelブロック     ： 事前分布や尤度を指定（メインコーディング）


# ＜stanの記法＞
# - Rstudioでstanを記述すると入力支援を得ることができるのでおすすめ
# - ファイルにコーディングする際には特有の記法が存在する
#   --- 1 ブロックは中括弧で囲む
#   --- 2 各行の末端にはセミコロン(;)が必要
#   --- 3 コメントにはスラッシュ2本(//)を使う
#   --- 4 ファイルの最終行には空白行が必要


# ＜目次＞
# 0 準備
# 1 Stanに渡すためのデータ整形
# 2 MCMCによるサンプリングの実施
# 3 収束の確認
# 4 ベクトル化


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(rstan)
library(tidybayes)


# 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# データ準備
# ---ビールの売上高
file_beer_sales_1 <- read_csv("book/bayesian_model/csv/2-4-1-beer-sales-1.csv")

# データの確認
file_beer_sales_1 %>% as_tibble()

# ヒストグラム
file_beer_sales_1 %>%
  ggplot(aes(x = sales)) +
  geom_histogram()


# 1 Stanに渡すためのデータ整形 --------------------------------------------------

# ＜ポイント＞
# - RのデータをStanに読み込ませるためにはlistにデータを整形しておく必要がある
#   --- tidybayes::compose_data()はパイプラインで前処理を行う


# サンプルサイズ
sample_size <- file_beer_sales_1 %>% nrow()
sample_size %>% print()

# listにまとめる
# --- データとサンプルサイズ
data_list <- list(sales = file_beer_sales_1$sales, N = sample_size)
data_list %>% print()

# 参考：{tidybayes}による前処理
data_list_2 <- file_beer_sales_1 %>% compose_data()


# 2 MCMCによるサンプリングの実施 --------------------------------------------------

# 乱数の生成
# --- seed  ： 乱数シード（固定しておくのがセオリー）
# --- chains： チェーン数
# --- iter  ： 乱数生成の繰り返し数
# --- warmup： バーンイン期間
# --- thin  ： 間引き数(1なら間引き無し)
mcmc_result <-
  stan(file = "book/bayesian_model/stan/2-4-1-calc-mean-variance.stan",
       data = data_list,
       seed = 1,
       chains = 4,
       iter = 2000,
       warmup = 1000,
       thin = 1)

# 結果の表示
mcmc_result %>% print()
mcmc_result %>% names()
mcmc_result %>% glimpse()

# 結果の表示
# --- 分位点を指定して出力
# --- probs引数で出力値を指定（中央値と95%信用区間）
mcmc_result %>% print(probs = c(0.025, 0.5, 0.975))


# 3 収束の確認 -------------------------------------------------------------------

# ＜ポイント＞
# - 適切に収束しているかを統計量で確認（n_eff/Rhat）
# - トレースプロットで視覚的に確認


# n_effとRhat
# --- n_eff：MCMCにおける有効サンプル数（100くらいが目安）
# --- Rhat ：MCMCが収束しているかを判断する指標（1.1未満が目安）
mcmc_result %>% print()

# トレースプロット
# --- バーンイン期間無し
mcmc_result %>% traceplot()

# トレースプロット
# --- バーンイン期間あり（灰色の区間）
mcmc_result %>% traceplot(inc_warmup = T)


# 4 ベクトル化 -------------------------------------------------------------------

# ＜ポイント＞
# - Stanファイルの書き方を通常の記法からベクトル方式に変更することも可能
#   --- ベクトル化により高速化が期待できる場面がある


# ＜通常の記法＞
# model {
#   for (i in 1:N) {
#     sales[i] ~ normal(mu, sigma);
#   }
# }

# ＜ベクトル化＞
# model {
#   sales ~ normal(mu, sigma);
# }



# 乱数の生成
# --- ファイルのみ変更
mcmc_result_vec <-
  stan(file = "book/bayesian_model/stan/2-4-2-calc-mean-variance-vec.stan",
       data = data_list,
       seed = 1,
       chains = 4,
       iter = 2000,
       warmup = 1000,
       thin = 1)

# 結果の表示
# --- MCMCサンプリングの結果
# --- 事後分布の四分位点を出力
mcmc_result_vec %>%
  print(probs = c(0.025, 0.5, 0.975))
