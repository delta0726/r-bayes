# **********************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 3 一般化線形モデル
# Theme   : 9 ロジスティック回帰モデル
# Date    : 2022/4/23
# Page    : P220 - P227
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# **********************************************************************************


# ＜概要＞
# - 目的変数がバイナリデータであるロジスティック回帰のbrmsによる実装を確認する


# ＜目次＞
# 0 準備
# 1 データ確認
# 2 ロジスティック回帰モデルの推定
# 3 推定されたモデルの解釈
# 4 回帰曲線のプロット
# 5 rstanを用いた実装
# 6 試行回数が常に1の場合


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
germination_dat <- read_csv("csv/3-9-1-germination.csv")


# 1 データ確認 -------------------------------------------------------------------

# ＜ポイント＞
# - ある植物に10粒(size)の種子をまいて何粒が発芽(germination)したかを調査する
#   --- サンプルごとに栄養素の量(nutrition)を変化させている


# データ確認
germination_dat %>% print()
germination_dat %>% map(table)

# データの要約
germination_dat %>% summary()

# プロット作成
germination_dat %>%
  ggplot(mapping = aes(x = nutrition, y = germination, color = solar)) +
    geom_point() +
    labs(title = "Relationship between shade and sunshine")


# 2 ロジスティック回帰モデルの推定 -------------------------------------------------------

# ＜ポイント＞
# - size個の種子のうちgermination個が発芽したことを示すため以下の記法を用いる
#   --- germination | trials(size)


# モデル構築
# --- family：二項分布を使う（binomial）
# --- prior ：無情報事前分布にする
glm_binom_brms <-
  brm(germination | trials(size) ~ solar + nutrition,
      family = binomial(),
      data = germination_dat,
      seed = 1,
      prior = set_prior("", class = "Intercept"))

# 確認
# --- 収束に問題なし（Rhat = 1）
glm_binom_brms %>% print()


# 3 推定されたモデルの解釈 ----------------------------------------------------------------

# 解釈用のデータ作成
newdata_1 <-
  data.frame(solar = c("shade", "sunshine", "sunshine"),
             nutrition = c(2, 2, 3),
             size = c(10, 10, 10))

# 確認
newdata_1 %>% print()

# 発芽率を予測
# --- 線形予測子の予測値
linear_fit <-
  glm_binom_brms %>%
    fitted(newdata_1, scale = "linear") %>%
    .[,1]

# 成功確率の計算
# --- ロジスティック関数を適用して計算
fit <- 1 / (1 + exp(-linear_fit))
fit

# オッズの計算
odds_1 <- fit[1] / (1 - fit[1])
odds_2 <- fit[2] / (1 - fit[2])
odds_3 <- fit[3] / (1 - fit[3])

# モデルの係数を取得
coef <- glm_binom_brms %>% fixef() %>% .[,1]
coef

# データ検証
# --- solarがshadeからsunshineに変わった時のオッズ比
odds_2 / odds_1
coef["solarsunshine"] %>% exp()

# データ検証
# --- nutritionが1から2に変わった時のオッズ比
odds_3 / odds_2
coef["nutrition"] %>% exp()


# 4 回帰曲線のプロット ------------------------------------------------

# 回帰曲線のプロット
# --- 95%ベイズ信用区間付きの回帰曲線
glm_binom_brms %>%
  marginal_effects(effects = "nutrition:solar") %>%
  plot(points = TRUE)

# その他のプロット
# --- 事後分布の図示
# --- 係数の信頼区間
glm_binom_brms %>% plot(pars = "^b_")
glm_binom_brms %>% stanplot(type = "intervals", pars = "^b_")

# 参考：95%ベイズ予測区間付きのグラフ
set.seed(1)
glm_binom_brms %>%
  marginal_effects(method = "predict",
                   effects = "nutrition:solar") %>%
  plot(points = TRUE)


# 5 rstanを用いた実装 ----------------------------------------------------------

# ダミー変数の作成
solar_dummy <- as.numeric(germination_dat$solar == "sunshine")

# データの作成
data_list_1 <-
  list(N = nrow(germination_dat),
       germination = germination_dat$germination,
       binom_size = germination_dat$size,
       solar = solar_dummy,
       nutrition = germination_dat$nutrition)

# モデル構築
glm_binom_stan <-
  stan(file = "stan/3-9/3-9-1-glm-binom-1.stan",
       data = data_list_1,
       seed = 1)

# 確認
glm_binom_stan %>% print(probs = c(0.025, 0.5, 0.975))


# 6 試行回数が常に1の場合 ----------------------------------------------------------

# ＜ポイント＞
# - 試行回数が1の場合はベルヌーイ分布の方が適切
#   --- 以下は実装イメージ

# ベルヌーイ分布
# glm_bernoulli_brms <- brm(
#   formula = 0/1データ ~ 説明変数,
#   family = bernoulli(),
#   data = データ,
#   seed = 1,
#   prior = c(set_prior("", class = "Intercept"))
# )
