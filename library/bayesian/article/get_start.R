# ***********************************************************************************************
# Library   : bayesian
# Title     : General Interface for Bayesian TidyModels
# Created by: Owner
# Created on: 2021/08/29
# URL       : https://mjskay.github.io/tidybayes/articles/tidybayes.html
# ***********************************************************************************************


# ＜概要＞
# - {bayesian}は{brms}によるベイズモデリングを{parsnip}の形式で行うためのライブラリ
# - ベイズモデリングをtidymodelsのフレームワークで処理することが可能
#   --- {brms}を用いるので、stanによるコーディングを読込む必要はない


# ＜目次＞
# 0 準備
# 1 特徴量エンジニアリング
# 2 モデル構築
# 3 ワークフローの設定
# 4 学習
# 5 予測


# 0 準備  ----------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidymodels)
library(bayesian)


# データ確認
epilepsy %>% print()
epilepsy %>% glimpse()



# 1 特徴量エンジニアリング ----------------------------------------------

# レシピ定義
# --- AgeとBaseを基準化
epi_recipe <- 
  epilepsy %>%
    recipe() %>%
    update_role(count, new_role = "outcome") %>%
    update_role(Trt, Age, Base, patient, new_role = "predictor") %>%
    add_role(patient, new_role = "group") %>%
    step_normalize(Age, Base)

# レシピ確認
epi_recipe %>% summary()

# データ出力
epi_recipe %>% prep() %>% juice() %>% glimpse()



# 2 モデル構築 --------------------------------------------------------

# ＜ポイント＞
# - bayesian()はベイジアンモデルを初期化するための関数
#   --- 関数内で直接多くの情報を設定したり、updateメソッドを介して後で情報を更新することが可能


#　モデル定義
epi_model <- 
  bayesian(family = poisson()) %>%
  set_engine("brms") %>%
  set_mode("regression")

# 確認
epi_model %>% print()


# モデル更新
epi_model <- 
  epi_model %>%
    update(family = poisson())

# 確認
epi_model %>% print()


# 3 ワークフローの設定 --------------------------------------------------

# ワークフロー定義
epi_workflow <- 
  workflow() %>%
    add_recipe(epi_recipe) %>%
    add_model(spec = epi_model, 
              formula = count ~ Trt + Base + Age + (1 | patient))

# 確認
epi_workflow %>% print()


# 4 学習 ---------------------------------------------------------------

# 学習
epi_workflow_fit <- 
  epi_workflow %>%
    fit(data = epilepsy)

# 確認
epi_workflow_fit %>% print()
epi_workflow_fit %>% names()

# モデル取得
# --- brmsクラスのモデルが確認できる
epi_fit <- epi_workflow_fit %>% extract_fit_parsnip()
epi_brmsfit <- epi_fit$fit
epi_brmsfit %>% class()


# 5 予測 ---------------------------------------------------------------

# 予測用データ
newdata <- epilepsy[1:5, ]

# 予測
# --- 信頼区間
epi_workflow_fit %>%
  predict(new_data = newdata, 
          type = "conf_int", 
          level = 0.95)

# 予測
# --- 標準誤差あり
epi_workflow_fit %>%
  predict(new_data = newdata, 
          type = "conf_int", 
          level = 0.95, 
          std_error = TRUE)

