# **********************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 2 RとStanによるデータ分析
# Theme   : 5-1 MCMCの結果の評価（bayesplot）
# Date    : 2022/4/18
# Page    : P125 - P134
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# **********************************************************************************


# ＜テーマ＞
# - MCMC実行後の処理プロセスを学ぶ
#   --- MCMCサンプルの取扱いと事後チェック{bayesplot}を用いた描画方法


# ＜目次＞
# 0 準備
# 1 MCMCの実行
# 2 MCMCサンプルの構造
# 3 MCMCサンプルの代表値の計算
# 4 トレースプロットの作成
# 5 事後分布のプロット


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
# --- ビールの売上高
# --- 小動物の個数
file_beer_sales_1 <- read_csv("csv/2-4-1-beer-sales-1.csv")
animal_num        <- read_csv("csv/2-5-1-animal-num.csv")

# データ確認
file_beer_sales_1 %>% print()
file_beer_sales_1$sales %>% hist()


# 1 MCMCの実行 ----------------------------------------------------------------

# ＜ポイント＞
# - ｢2-4 stanの基本｣と同様の計算
#   --- stanfitオブジェクトが作成される


# データ準備
data_list <-
  list(sales = file_beer_sales_1$sales,
       N = nrow(file_beer_sales_1))


# MCMCの実行
mcmc_result <-
  stan(file = "stan/2-4/2-4-1-calc-mean-variance.stan",
       data = data_list,
       seed = 1,
       chains = 4,
       iter = 2000,
       warmup = 1000,
       thin = 1)

# クラス確認
# --- rstanfitオブジェクト
mcmc_result %>% class()

# データ確認
mcmc_result %>% print()
mcmc_result %>% glimpse()



# 2 MCMCサンプルの構造 -------------------------------------------------------------

# ＜ポイント＞
# - stanfitオブジェクトの構造と格納されているデータを確認する


# オブジェクト抽出
# --- MCMCサンプルの抽出
mcmc_sample <- mcmc_result %>% rstan::extract(permuted = FALSE)

# データ確認
mcmc_sample %>% class()
mcmc_sample %>% head()

# 次元数
# --- chains    ：Chain
# --- parameters：推定されたパラメータ
mcmc_sample %>% dim()
mcmc_sample %>% dimnames()


# データ抽出
# --- パラメタmuの1回目のチェーンのMCMCサンプルのburn-in後の1つめのサンプル
# --- パラメタmuの1回目のチェーンの全サンプル
mcmc_sample[1, "chain:1", "mu"]
mcmc_sample[, "chain:1", "mu"]

# MCMCサンプルの個数
# --- パラメタmuの1回目のチェーン
# --- warmup引数で設定した1000
mcmc_sample[, "chain:1", "mu"] %>% length()

# MCMCサンプルの個数
# --- 4つのチェーンすべて
mcmc_sample[ , , "mu"] %>% length()

# 4つのチェーンがあるので、1000iter×4ChainのMatrix
mcmc_sample[ , , "mu"] %>% dim()
mcmc_sample[ , , "mu"] %>% class()


# 3 MCMCサンプルの代表値の計算 ---------------------------------------------------------

# ＜ポイント＞
# - MCMCで作成したサンプルを元に統計量を計算する
#   --- 事後分布に基づく統計量


# MCMCサンプルの抽出
# --- 全チェーン(4000個)のmu
mu_mcmc_vec <- mcmc_sample[ , ,"mu"] %>% as.vector()

# 代表値の計算
# --- 事後中央値
# --- 事後期待値
# --- 95%ベイズ信用区間
mu_mcmc_vec %>% median()
mu_mcmc_vec %>% mean()
mu_mcmc_vec %>% quantile(probs = c(0.025, 0.975))


# ＜参考＞
# 上記の計算結果の確認
# --- stanfitオブジェクトの出力結果と一致
mcmc_result %>% print(probs = c(0.025, 0.5, 0.975))


# 4 トレースプロットの作成 -------------------------------------------------------------

# ＜ポイント＞
# - トレースプロットによりMCMCが適切に収束しているかを確認する


# トレースプロット
# --- rstan::traceplot()
mcmc_result %>% traceplot(par = "mu")

# トレースプロット
# --- 4つのChainをまとめて1つのグラフにする
mcmc_sample[,,"mu"] %>%
  ts() %>%
  autoplot(facets = F,
           ylab = "mu",
           main = "TracePlot")


# 5 事後分布のプロット ----------------------------------------------------------------

# ＜ポイント＞
# - MCMCで作成したサンプルからmuの事後分布を確認する
#   --- MCMCサンプルをカーネル密度推定して作成した密度プロットと一致


# MCMCサンプルの抽出
# --- 全チェーン(4000個)
mu_mcmc_vec <- mcmc_sample[ , ,"mu"] %>% as.vector()

# プロット作成
# --- 事後分布を密度プロットで表現
# --- {ggplot2}を用いて作成
tibble(mu_mcmc_sample = mu_mcmc_vec) %>%
  ggplot(mapping = aes(x = mu_mcmc_sample)) +
  geom_density(size = 1.5)


# プロット作成
# --- {bayesplot}の関数を用いて事後分布のヒストグラムと密度プロットを作成
mcmc_sample %>% mcmc_hist(pars = c("mu", "sigma"))
mcmc_sample %>% mcmc_dens(pars = c("mu", "sigma"))


# 6 その他のプロット -----------------------------------------------------------------

# ＜ポイント＞
# - {bayesplot}は多数のプロットのうち代表的なプロットのみを確認する

# ＜参考＞
# bayesplot
# https://mc-stan.org/bayesplot/


# トレースプロット
mcmc_sample %>% mcmc_trace(pars = c("mu", "sigma"))

# 複数プロット
# --- 事後分布とトレースプロットをまとめて図示
mcmc_sample %>% mcmc_combo(pars = c("mu", "sigma"))


# 7 事後分布の範囲を比較 ------------------------------------------------------------

# ＜ポイント＞
# - 複数のパラメータの信頼区間を比較する
#   --- 平均同士ではなく平均(mu)/標準偏差(sigma)を比較している点に注意


# 事後分布の範囲を比較
mcmc_sample %>%
  mcmc_intervals(pars = c("mu", "sigma"),
                 prob = 0.8,
                 prob_outer = 0.95)

# 密度の情報も加える
mcmc_sample %>%
  mcmc_areas(pars = c("mu", "sigma"),
             prob = 0.6,
             prob_outer = 0.99)


# 8 MCMCサンプルの自己相関 ---------------------------------------------------------

# ＜ポイント＞
# - MCMCサンプリングに自己相関が存在しないかをチェックする


# MCMCサンプルのコレログラム
mcmc_sample %>% mcmc_acf_bar(pars = c("mu", "sigma"))

# (参考)チェーン別の事後分布
mcmc_sample %>% mcmc_dens_overlay(pars = c("mu", "sigma"))

# (参考)チェーン別のヒストグラム
mcmc_sample %>% mcmc_hist_by_chain(pars = c("mu", "sigma"))
