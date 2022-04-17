#***************************************************************************************
# Title     : ベイズ統計モデリングによるデータ分析入門
# Chapter   : 3-10 交互作用
# Objective : TODO
# Created by: Owner
# Created on: 2021/4/11
# Page      : P228 - P242
#***************************************************************************************


# ＜テーマ＞
# -


# ＜目次＞
# 0 準備
# 1 モデル構築（カテゴリ×カテゴリ）
# 2 係数の解釈（カテゴリ×カテゴリ）
# 3 モデルの図示（カテゴリ×カテゴリ）
# 4 モデル構築（カテゴリ×数量）
# 5 係数の解釈（カテゴリ×数量）
# 6 モデルの図示（カテゴリ×数量）
# 7 モデル構築（数量×数量）
# 8 係数の解釈（数量×数量）
# 9 モデルの図示（数量×数量）


# 0 準備 -------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(rstan)
library(brms)


# オプション設定
# --- 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# データロード
interaction_1 <- read_csv("book/bayesian_model/csv/3-10-1-interaction-1.csv")
interaction_2 <- read_csv("book/bayesian_model/csv/3-10-2-interaction-2.csv")
interaction_3 <- read_csv("book/bayesian_model/csv/3-10-3-interaction-3.csv")

# データ確認
interaction_1 %>% print()
interaction_1 %>% select(publicity, bargen) %>% map(table)

interaction_2 %>% print()
interaction_2 %>% select(publicity) %>% map(table)

interaction_3 %>% print()
interaction_3 %>% select(clerk) %>% map(table)

# データの要約
interaction_1 %>% summary()
interaction_2 %>% summary()
interaction_3 %>% summary()


# 1 モデル構築（カテゴリ×カテゴリ）-----------------------------------------------------

# デザイン行列の作成
model.matrix(sales ~ publicity * bargen, interaction_1)

# モデル構築1
# --- 通常モデル
interaction_brms_1 <-
  brm(formula = sales ~ publicity * bargen,
      family = gaussian(link = "identity"),
      data = interaction_1,
      seed = 1,
      prior = c(set_prior("", class = "Intercept"),
                set_prior("", class = "sigma")))

# モデル構築2
# --- 交互作用ありモデル
interaction_brms_1_2 <-
  brm(formula = sales ~ publicity + bargen + publicity:bargen,
      family = gaussian(link = "identity"),
      data = interaction_1,
      seed = 1,
      prior = c(set_prior("", class = "Intercept"),
                set_prior("", class = "sigma")))

# 結果確認
interaction_brms_1 %>% print()
interaction_brms_1_2 %>% print()

# 参考：事後分布の図示
interaction_brms_1 %>% plot()
interaction_brms_1_2 %>% plot()


# 2 係数の解釈（カテゴリ×カテゴリ）-------------------------------------------------------

# 交互作用の効果の確認
# 説明変数を作る
newdata_1 <-
  data.frame(publicity = rep(c("not", "to_implement"),2),
             bargen = rep(c("not", "to_implement"),each = 2))

# 結果確認
newdata_1 %>% print()

# 予測
round(fitted(interaction_brms_1, newdata_1), 2)

# プロット作成
# --- 事後分布とトレースプロット
interaction_brms_1 %>% plot()


# 3 モデルの図示（カテゴリ×カテゴリ）--------------------------------------------------

# モデルの図示
interaction_brms_1 %>%
  marginal_effects(effects = "publicity:bargen") %>%
  plot(points = T)


# 4 モデル構築（カテゴリ×数量）-------------------------------------------------------

# デザイン行列の作成
model.matrix(sales ~ publicity * temperature, interaction_2)

# モデル化
interaction_brms_2 <-
  brm(formula = sales ~ publicity * temperature,
      family = gaussian(link = "identity"),
      data = interaction_2,
      seed = 1,
      prior = c(set_prior("", class = "Intercept"),
                set_prior("", class = "sigma")))

# 結果確認
interaction_brms_2 %>% print()

# プロット作成
# --- 事後分布とトレースプロット
interaction_brms_2 %>% plot()


# 5 係数の解釈（カテゴリ×数量）------------------------------------------------------

# 交互作用の効果の確認
# 説明変数を作る
newdata_2 <-
  data.frame(publicity   = rep(c("not", "to_implement"), each = 2),
             temperature = c(0,10,0,10))

# 確認
newdata_2 %>% print()

# 予測
round(fitted(interaction_brms_2, newdata_2), 2)


# 6 モデルの図示（カテゴリ×数量）----------------------------------------------------

# 回帰直線の図示
interaction_brms_2 %>%
  marginal_effects(effects = "temperature:publicity") %>%
  plot(points = T)


# 7 モデル構築（数量×数量）-------------------------------------------------------------

# プロット作成
interaction_3 %>%
  ggplot(aes(x = product, y = sales, color = factor(clerk)))+
    geom_point()


# デザイン行列の作成
model.matrix(sales ~ product * clerk, interaction_3)

# モデル化
interaction_brms_3 <-
  brm(formula = sales ~ product * clerk,
      family = gaussian(link = "identity"),
      data = interaction_3,
      seed = 1,
      prior = c(set_prior("", class = "Intercept"),
                set_prior("", class = "sigma")))

# 結果確認
interaction_brms_3 %>% print()

# プロット作成
# --- 事後分布とトレースプロット
interaction_brms_3 %>% plot()


# 8 係数の解釈（数量×数量） ---------------------------------------------------------

# 交互作用の効果の確認
# 説明変数を作る
newdata_3 <-
  data.frame(product = c(0,10,0,10),
              clerk   = c(0,0,10,10))

# 確認
newdata_3 %>% print()

# 予測
round(fitted(interaction_brms_3, newdata_3), 2)


# 9 モデルの図示（数量×数量）---------------------------------------------------------

# 回帰直線の図示
# 1つのグラフに回帰直線をまとめて描画する
int_conditions <- list(
  clerk = setNames(1:9, paste("clerk=", 1:9, sep=""))
)
int_conditions

eff_3 <- marginal_effects(interaction_brms_3,
                          effects = "product:clerk",
                          int_conditions = int_conditions)
plot(eff_3, points = TRUE)


# 回帰直線の図示
# 働く人数ごとにグラフを分ける
conditions <- data.frame(clerk = 1:9)
conditions

eff_4 <- marginal_effects(interaction_brms_3,
                          effects = "product",
                          conditions = conditions)
plot(eff_4, points = FALSE)


