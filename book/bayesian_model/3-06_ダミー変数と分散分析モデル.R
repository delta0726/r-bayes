# **********************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 3 一般化線形モデル
# Theme   : 6 ダミー変数と分散分析モデル
# Date    : 2022/4/22
# Page    : P201 - P206
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# **********************************************************************************


# ＜概要＞
# - 分散分析モデルとは説明変数に質的データを用いて確率分布に正規分布を仮定したモデルのことを指す
# - ダミー変数について学んだ上で分散分析モデルをbrmsやrstanで推定する


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 分散分析モデルの推定（bram）
# 3 分散分析モデルの推定（rstan）


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(rstan)
library(brms)


# オプション設定
# --- 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# データロード
sales_weather <- read_csv("csv/3-6-1-beer-sales-3.csv")


# 1 データ確認 -----------------------------------------------------------------

# ＜ポイント＞
# - 説明変数がカテゴリカル変数のみになっている
# - 分布を見たところcloudyとrainyには大きな差がなさそう


# データ確認
sales_weather %>% print()
sales_weather$weather %>% table()

# データの要約
sales_weather %>% summary()

# プロット作成
sales_weather %>%
  ggplot(mapping = aes(x = weather, y = sales)) +
    geom_violin() +
    geom_point(aes(color = weather)) +
    labs(title = "Relationship between beer sales and temperture")


# 2 分散分析モデルの推定（bram） --------------------------------------------------------

# ＜ポイント＞
# - 分散分析モデルとは説明変数に質的データを用いて確率分布に正規分布を仮定したモデルのことを指す
# - カテゴリカル変数はそのままインプットすると関数内部でダミー変換して適切に処理してくれる
#   --- weatherrainyとweathersunnyは曇りとの違いを示している
#   --- weatherrainyのベイズ信用区間は-6.92~6.29と0を中心に分布（やはりcloudyとの差は小さい）


# 分散分析モデルを作る
# --- 正規分布を使う
anova_brms <-
  brm(formula = sales ~ weather,
      family = gaussian(),
      data = sales_weather,
      seed = 1,
      prior = c(set_prior("", class = "Intercept"),
                set_prior("", class = "sigma")))

# MCMCの結果確認
anova_brms %>% print()

# プロット作成
# --- 推定された天気別の売上の分布
anova_brms %>%
  marginal_effects() %>%
  plot(points = FALSE)


# 3 分散分析モデルの推定（rstan） ------------------------------------------------------

# ＜ポイント＞
# - 分散分析はデザイン行列からstanを用いて実行することも可能
#   --- デザイン行列ではダミー変換が行われている


# デザイン行列の作成
# --- ダミー変換が行われている
formula_anova <- formula(sales ~ weather)
design_mat <- model.matrix(formula_anova, sales_weather)

# stanデータ作成
# --- N：サンプルサイズ
# --- K：デザイン行列の列数
# --- Y：応答変数
# --- X：デザイン行列
data_list <-
  list(
  N = nrow(sales_weather),
  K = 3,
  Y = sales_weather$sales,
  X = design_mat)

# データ確認
data_list %>% print()

# rstanで分散分析モデルを実行
anova_stan <-
  stan(file = "stan/3-4/3-4-1-lm-design-matrix.stan",
       data = data_list,
       seed = 1)

# 結果確認
# --- rstan
# --- brams
anova_stan %>% print(probs = c(0.025, 0.5, 0.975))
anova_brms %>% print(probs = c(0.025, 0.5, 0.975))





