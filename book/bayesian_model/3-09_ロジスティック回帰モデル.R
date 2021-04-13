#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 3-9 ロジスティック回帰モデル
# Objective : TODO
# Created by: Owner
# Created on: 2021/4/14
# Page      : P220 - P227
#***************************************************************************************


# ＜テーマ＞
# -


# ＜目次＞
# 0 準備
# 1 ロジスティック回帰モデルの推定
# 2 モデルの解釈
# 3 brmsを用いない実装の方法


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
germination_dat <- read_csv("book/bayesian_model/csv/3-9-1-germination.csv")

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



# 1 ロジスティック回帰モデルの推定 -------------------------------------------------------

# モデル構築
# --- family：二項分布を使う
# --- prior ：無情報事前分布にする
glm_binom_brms <-
  brm(germination | trials(size) ~ solar + nutrition,
      family = binomial(),
      data = germination_dat,
      seed = 1,
      prior = set_prior("", class = "Intercept"))

# 結果確認
glm_binom_brms %>% print()


# 2 モデルの解釈 ----------------------------------------------------------------------

# 新データの作成
# --- 係数の解釈
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
    fitted(newdata_1, scale = "linear")[,1]

# ロジスティック関数を適用して、成功確率を計算
fit <- 1 / (1 + exp(-linear_fit))
fit

# オッズを計算
odds_1 <- fit[1] / (1 - fit[1])
odds_2 <- fit[2] / (1 - fit[2])
odds_3 <- fit[3] / (1 - fit[3])

# モデルの係数を取得
coef <- fixef(glm_binom_brms)[,1]
coef

# solarがshadeからsunshineに変わった時のオッズ比
odds_2 / odds_1
exp(coef["solarsunshine"])

# nutritionが1から2に変わった時のオッズ比
odds_3 / odds_2
exp(coef["nutrition"])


# 95%ベイズ信用区間付きの回帰曲線
eff <- marginal_effects(glm_binom_brms, 
                        effects = "nutrition:solar")

plot(eff, points = TRUE)



# 参考：事後分布の図示
plot(glm_binom_brms, pars = "^b_")

# 参考：係数の信頼区間
stanplot(glm_binom_brms, type = "intervals", pars = "^b_")

# 参考：95%ベイズ予測区間付きのグラフ
set.seed(1)
eff_pre <- marginal_effects(glm_binom_brms, 
                            method = "predict",
                            effects = "nutrition:solar")
plot(eff_pre, points = TRUE)


# 3 brmsを用いない実装の方法 ----------------------------------------------------------

# ダミー変数の作成
solar_dummy <- as.numeric(germination_dat$solar == "sunshine")

# データの作成
data_list_1 <-
  list(N = nrow(germination_dat),
       germination = germination_dat$germination,
       binom_size = germination_dat$size,
       solar = solar_dummy,
       nutrition = germination_dat$nutrition)

# 確認
data_list_1 %>% print()

# MCMC実行
glm_binom_stan <-
  stan(file = "book/bayesian_model/stan/3-9-1-glm-binom-1.stan",
       data = data_list_1,
       seed = 1)

# 結果確認
glm_binom_stan %>% print(probs = c(0.025, 0.5, 0.975))



# 補足：試行回数が常に1の場合 ----------------------------------------------------------

# 参考：0/1データの場合（このコードは実行できません）
# glm_bernoulli_brms <- brm(
#   formula = 0/1データ ~ 説明変数,              # modelの構造を指定
#   family = bernoulli(),                        # ベルヌーイ分布を使う
#   data = データ,                               # データ
#   seed = 1,                                    # 乱数の種
#   prior = c(set_prior("", class = "Intercept"))# 無情報事前分布にする
# )
