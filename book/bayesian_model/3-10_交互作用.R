# **********************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 3 一般化線形モデル
# Theme   : 10 交互作用
# Date    : 2022/4/23
# Page    : P228 - P242
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# **********************************************************************************


# ＜概要＞
# - 線形予測子に説明変数同士の交互作用を導入する


# ＜目次＞
# 0 準備
# 1 データ確認
# 2-1 モデル構築（カテゴリ×カテゴリ）
# 2-2 係数の解釈（カテゴリ×カテゴリ）
# 2-3 モデルの図示（カテゴリ×カテゴリ）
# 3-1 モデル構築（カテゴリ×数量）
# 3-2 係数の解釈（カテゴリ×数量）
# 3-3 モデルの図示（カテゴリ×数量）
# 4-1 モデル構築（数量×数量）
# 4-2 係数の解釈（数量×数量）
# 4-3 モデルの図示（数量×数量）


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
interaction_1 <- read_csv("csv/3-10-1-interaction-1.csv")
interaction_2 <- read_csv("csv/3-10-2-interaction-2.csv")
interaction_3 <- read_csv("csv/3-10-3-interaction-3.csv")


# 1 データ確認 ---------------------------------------------------------------------

# データ確認
# --- Y ：数値データ
# --- X ：カテゴリカルデータ × カテゴリカルデータ
interaction_1 %>% print()
interaction_1 %>%
  select(publicity, bargen) %>%
  map(table)

# データ確認
# --- Y ：数値データ
# --- X ：カテゴリカルデータ × 数値データ
interaction_2 %>% print()
interaction_2 %>%
  select(publicity) %>%
  map(table)

# データ確認
# --- Y ：数値データ
# --- X ：数値データ × 数値データ
interaction_3 %>% print()
interaction_3 %>% select(clerk) %>% map(table)

# データの要約
interaction_1 %>% summary()
interaction_2 %>% summary()
interaction_3 %>% summary()


# 2-1 モデル構築（カテゴリ×カテゴリ）-----------------------------------------------

# ＜ポイント＞
# - 交互効果は｢*｣や｢:｣の演算子を使って定義することができる
# - モデル定義の際は説明変数がカテゴリカルデータであることは意識しなくてもよい


# デザイン行列
# --- 線形予測子のイメージ確認
model.matrix(sales ~ publicity * bargen, interaction_1) %>% head()

# モデル構築1
# --- フォーミュラを｢*｣でつなぐと交互作用を自動的に定義してくれる
interaction_brms_1 <-
  brm(formula = sales ~ publicity * bargen,
      family = gaussian(link = "identity"),
      data = interaction_1,
      seed = 1,
      prior = c(set_prior("", class = "Intercept"),
                set_prior("", class = "sigma")))

# モデル構築2
# --- 自分で交互効果を定義する際は対象変数を｢:｣でつなぐ
# --- 交互変数を出力するパターンをコントロールすることができる
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


# 2-2 係数の解釈（カテゴリ×カテゴリ）-------------------------------------------------------

# ＜ポイント＞
# - カテゴリカルデータの各パターンを網羅したデータセットを作成して予測値を確認する


# 解釈用データの作成
# --- 各パターンを網羅したデータセットを作成
newdata_1 <-
  data.frame(publicity = rep(c("not", "to_implement"), 2),
             bargen = rep(c("not", "to_implement"), each = 2))

# 確認
newdata_1 %>% print()

# 予測
interaction_brms_1 %>%
  fitted(newdata_1) %>%
  round(2)


# 2-3 モデルの図示（カテゴリ×カテゴリ）--------------------------------------------------

# ＜ポイント＞
# - カテゴリカルデータなので散布図はカテゴリごとに作成される


# モデルの図示
interaction_brms_1 %>%
  marginal_effects(effects = "publicity:bargen") %>%
  plot(points = T)


# 3-1 モデル構築（カテゴリ×数量）-------------------------------------------------------

# ＜ポイント＞
# - 数量データとカテゴリカルデータを含むデータセットを使用


# デザイン行列の作成
# --- 線形予測子のイメージ確認
model.matrix(sales ~ publicity * temperature, interaction_2) %>% head()

# モデル構築
# --- フォーミュラを｢*｣でつなぐことで交互効果を導入
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


# 3-2 係数の解釈（カテゴリ×数量）------------------------------------------------------

# 解釈用データの作成
# --- 各パターンを網羅したデータセットを作成
newdata_2 <-
  data.frame(publicity = rep(c("not", "to_implement"), each = 2),
             temperature = c(0, 10, 0, 10))

# 確認
newdata_2 %>% print()

# 予測
interaction_brms_2 %>%
  fitted(newdata_2) %>%
  round(2)


# 3-3 モデルの図示（カテゴリ×数量）--------------------------------------------------

# ＜ポイント＞
# - カテゴリごとに回帰直線と信頼区間を表示することで効果の違いを明確にする


# 回帰直線の図示
interaction_brms_2 %>%
  marginal_effects(effects = "temperature:publicity") %>%
  plot(points = T)


# 4-1 モデル構築（数量×数量）-------------------------------------------------------------

# ＜ポイント＞
# - 数値データのうち店員数(clerk)は離散値なのでファクターとして扱う
#   --- 数値データを分位化するなどして離散化しておくと効果を認識しやすくなる


# プロット作成
# --- 店員数(clerk)が多いほど同じ条件での売上高は上昇していることが分かる
interaction_3 %>%
  mutate(clerk = factor(clerk)) %>%
  ggplot(aes(x = product, y = sales, color = clerk)) +
  facet_wrap(~clerk) +
  geom_point()

# デザイン行列の作成
model.matrix(sales ~ product * clerk, interaction_3) %>% head()

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


# 4-2 係数の解釈（数量×数量） ---------------------------------------------------------

# 解釈用データの作成
# --- 各パターンを網羅したデータセットを作成
newdata_3 <-
  data.frame(product = c(0, 10, 0, 10),
             clerk = c(0, 0, 10, 10))

# 確認
newdata_3 %>% print()

# 予測
interaction_brms_3 %>%
  fitted(newdata_3) %>%
  round(2)


# 4-3 モデルの図示（数量×数量）---------------------------------------------------------

# ＜ポイント＞
# - 店員数(clerk)をカテゴリとして扱ったことで店員数ごとの効果を測定することができた


# 回帰直線の図示
# 1つのグラフに回帰直線をまとめて描画する
int_conditions <- list(
  clerk = setNames(1:9, str_c("clerk=", 1:9, sep = ""))
)
int_conditions

interaction_brms_3 %>%
  marginal_effects(effects = "product:clerk",
                   int_conditions = int_conditions) %>%
  plot(points = TRUE)


# 回帰直線の図示
# 働く人数ごとにグラフを分ける
conditions <- data.frame(clerk = 1:9)
conditions

interaction_brms_3 %>%
  marginal_effects(effects = "product",
                   conditions = conditions) %>%
  plot(points = FALSE)
