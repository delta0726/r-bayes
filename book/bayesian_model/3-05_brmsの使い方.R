#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 3-5 brmsの使い方
# Objective : TODO
# Created by: Owner
# Created on: 2021/4/11
# Page      : P185 - P200
#***************************************************************************************


# ＜テーマ＞
# - {brms}ライブラリの使い方を学ぶ
#   --- Bayesian Regression Models using Stan
#   --- Stanコードを直接記述する必要がないので実装のハードルが低い


# ＜目次＞
# 0 準備
# 1 単回帰モデルの推定
# 2 brmsの基本的な使い方
# 3 事前分布の変更
# 4 Stanコードの作成（make_stancode関数）
# 5 Stanに渡すデータの作成（make_standata関数）
# 6 rstanでbrmsの結果を再現
# 7 事後分布の可視化
# 8 brmsによる予測
# 9 predict関数を使わない予測の実装
# 10 回帰直線の図示


# 0 準備 --------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(rstan)
library(brms)


# オプション設定
# --- 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# 分析対象のデータ
file_beer_sales_2 <- read_csv("book/bayesian_model/csv/3-2-1-beer-sales-2.csv")

# データ確認
file_beer_sales_2 %>% print()


# 1 単回帰モデルの推定 --------------------------------------------------------------

# モデル構築
# --- formula： modelの構造を指定
# --- family： 正規分布を使う
simple_lm_brms <-
  brm(formula = sales ~ temperature,
      family  = gaussian(link = "identity"),
      data    = file_beer_sales_2,
      seed    = 1)

# 結果確認
simple_lm_brms %>% print()

# MCMCサンプルの取得
simple_lm_brms %>%
  as.mcmc(combine_chains = TRUE) %>%
  as_tibble()

# プロット作成
# --- 事後分布の図示
simple_lm_brms %>% plot()


# 2 brmsの基本的な使い方 --------------------------------------------------------

# 線形予測子
# --- formulaとfamilyでモデルの構造を指定する
# --- 複雑なformulaはbf関数で作成（formula関数のようなもの）
simple_lm_formula <- bf(sales ~ temperature)

# リンク関数
# --- 正規分布
# --- 二項分布
# --- ポアソン分布
gaussian()
binomial()
poisson()

# モデル構築
# --- simple_lm_brmsと結果は同じ
# --- 正規分布を使う
simple_lm_brms_2 <-
  brm(formula = simple_lm_formula,
      family = gaussian(),
      data = file_beer_sales_2,
      seed = 1,
      chains = 4,
      iter = 2000,
      warmup = 1000,
      thin = 1)

# 結果確認
simple_lm_brms_2 %>% print()


# 3 事前分布の変更 --------------------------------------------------------------

# 事前分布の取得
simple_lm_brms %>% prior_summary()

# 事前分布を無情報事前分布にする
simple_lm_brms_3 <-
  brm(formula = sales ~ temperature,
      family = gaussian(),
      data = file_beer_sales_2,
      seed = 1,
      prior = c(set_prior("", class = "Intercept"),
                set_prior("", class = "sigma")))

# 結果確認
simple_lm_brms_3 %>% print()

# 事前分布の取得
# --- 弱情報事前分布が設定されなくなった
simple_lm_brms_3 %>% prior_summary()


# ＜参考1＞
# その他の係数の事前分布の変更
# 気温の係数
set_prior("normal(0,100000)", class = "b", coef = "temperature")

# 事前分布の標準設定の確認
get_prior(formula = sales ~ temperature,
          family = gaussian(),
          data = file_beer_sales_2)


# ＜参考2＞
# stanコードの抽出
simple_lm_brms_3 %>% stancode()

# Stanに渡すデータの抽出
simple_lm_brms_3 %>% standata()


# 4 Stanコードの作成（make_stancode関数） ---------------------------------------------

# Stanコードの作成
make_stancode(
  formula = sales ~ temperature,
  family = gaussian(),
  data = file_beer_sales_2, 
  prior = c(prior("", class = "Intercept"),
            prior("", class = "sigma")))



# 5 Stanに渡すデータの作成（make_standata関数） ---------------------------------------

# rstanに渡すデータの作成
standata_brms <-
  make_standata(formula = sales ~ temperature,
                family = gaussian(),
                data = file_beer_sales_2)

# 確認
standata_brms %>% print()
standata_brms %>% glimpse()


# 6 rstanでbrmsの結果を再現 ---------------------------------------------------------

# モデル構築
# --- rstanでbrmsのモデルを実行
simple_lm_brms_stan <-
  stan(file = "book/bayesian_model/stan/3-5-1-brms-stan-code.stan",
       data = standata_brms,
       seed = 1)

# 結果確認
# --- rstanを使ったときの実行結果
simple_lm_brms_stan %>%
  print(pars = c("b_Intercept", "b[1]", "sigma"),
        probs = c(0.025, 0.5, 0.975))

# brmsを使ったときの実行結果
simple_lm_brms_3 %>% print()


# 7 事後分布の可視化 ----------------------------------------------------------------

# 係数の95%ベイズ信用区間
simple_lm_brms %>%
  stanplot(type = "intervals",
           pars = "^b_",
           prob = 0.8,
           prob_outer = 0.95)


# 8 brmsによる予測 -----------------------------------------------------------------

# データ作成
# --- 予測のための説明変数
new_data <- data.frame(temperature = 20)

# 回帰直線の信用区間付きの予測値
simple_lm_brms %>% fitted(new_data)

# 予測区間付きの予測値
set.seed(1)
simple_lm_brms %>% predict(new_data)


# 9 predict関数を使わない予測の実装 ----------------------------------------------------------------

# ***** 準備 ******

# MCMCサンプルの取得
mcmc_sample <- simple_lm_brms %>% as.mcmc(combine_chains = TRUE)

# データ確認
mcmc_sample %>% as_tibble()

# データ格納
# --- 推定されたパラメタ別に保存しておく
mcmc_b_Intercept   <- mcmc_sample[,"b_Intercept"]
mcmc_b_temperature <- mcmc_sample[,"b_temperature"]
mcmc_sigma         <- mcmc_sample[,"sigma"]

# 予測作成
# --- 予測された期待値のMCMCサンプルを得る
saigen_fitted <- mcmc_b_Intercept + 20 * mcmc_b_temperature


# ***** 回帰直線の信用区間付きの予測値 ******

# fittedの再現
saigen_fitted %>% mean()
saigen_fitted %>% quantile(probs = c(0.025, 0.975))

# 確認
simple_lm_brms %>% fitted(new_data)


# ***** 予測区間付きの予測値 ******

# 予測分布のMCMCサンプルを得る
set.seed(1)
saigen_predict <-
  rnorm %>%
    do.call(c(4000, list(mean = saigen_fitted, sd = mcmc_sigma)))

# predictの再現
saigen_predict %>% mean()
saigen_predict %>% quantile(probs = c(0.025, 0.975))

# 確認
set.seed(1)
simple_lm_brms %>% predict(data.frame(temperature = 20))


# 10 回帰直線の図示 --------------------------------------------------------

# 回帰直線の95%ベイズ信用区間付きのグラフ
eff <- simple_lm_brms %>% marginal_effects()
eff %>% plot(points = TRUE)

# 95%予測区間付きのグラフ
set.seed(1)
eff_pre <- simple_lm_brms %>% marginal_effects(method = "predict")
eff_pre %>% plot(points = TRUE)


# 参考：複数の説明変数があるときは、特定の要因だけを切り出せる
simple_lm_brms %>% marginal_effects(effects = "temperature")

# 参考：複数の説明変数を同時に図示(このコードは動きません)
# marginal_effects(brms_model,
#                  effects = "x1:x2")





