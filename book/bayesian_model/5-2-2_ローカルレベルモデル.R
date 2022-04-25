# **********************************************************************************
# Title   : ベイズ統計モデリングによるデータ分析入門
# Chapter : 5 状態空間モデル
# Theme   : 2-2 ローカルレベルモデル
# Date    : 2022/4/26
# Page    : P275 - P288
# URL     : https://logics-of-blue.com/r-stan-bayesian-model-intro-book-support/
# **********************************************************************************


# ＜概要＞
# - 動的線形モデル(DLM)の最も基本的な構造であるローカルレベルモデルを扱う


# ＜目次＞
# 0 準備
# 1 データ操作
# 2 ローカルレベルモデルの推定
# 3 ローカルレベルモデルの可視化
# 4 プロット用の関数定義


# 0 準備 ----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(magrittr)
library(rstan)
library(bayesplot)
library(ggfortify)
library(gridExtra)

# オプション設定
# --- 計算の高速化
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# データロード
sales_df <- read.csv("csv/5-2-1-sales-ts-1.csv")


# 1 データ操作 --------------------------------------------------------------------

# ＜ポイント＞
# - 日次の時系列売上高のデータ


# データ変換
sales_df <-
  sales_df %>%
    mutate(date = as.POSIXct(date))

# データ確認
sales_df %>% as_tibble()


# 2 ローカルレベルモデルの推定 --------------------------------------------------------

# ＜ポイント＞
# - stanによるMCMCを用いてローカルレベルモデルを推定する


# Stanデータの作成
data_list <- list(
  y = sales_df$sales, 
  T = nrow(sales_df)
)

# モデルの推定
local_level_stan <- stan(
  file = "stan/5-2/5-2-1-local-level.stan",
  data = data_list,
  seed = 1
)

# 収束の確認
local_level_stan %>% rhat() %>% mcmc_rhat()

# 結果の表示
local_level_stan %>%
  print(pars = c("s_w", "s_v","lp__"),
        probs = c(0.025, 0.5, 0.975))


# 3 ローカルレベルモデルの可視化 ----------------------------------------------------

# ＜ポイント＞
# - 時系列のポイントごとにMCMCサンプリングから取得したベイズ信用区間を設定する


# データ取得
# --- 生成された乱数を格納
mcmc_sample <- local_level_stan %>% rstan::extract()

# ベイズ信用区間の確認
# --- 1時点目の状態の95%ベイズ信用区間と中央値を得る
mcmc_sample$mu %>% .[, 1] %>% quantile(probs=c(0.025, 0.5, 0.975))


# データ加工
# --- 全シミュレーションのベイズ信用区間(95%)を取得
# --- プロット用にデータ加工
result_df <-
  mcmc_sample$mu %>%
    apply(MARGIN = 2, FUN = quantile, probs=c(0.025, 0.5, 0.975)) %>%
    t() %>%
    data.frame() %>%
    set_colnames(c("lwr", "fit", "upr")) %>%
    mutate(time = sales_df$date,
           obs = sales_df$sales) %>%
    as_tibble()

# データ確認
result_df %>% print()

# プロット作成
result_df %>%
  ggplot(aes(x = time, y = obs)) +
    labs(title="ローカルレベルモデルの推定結果") +
    ylab("sales") +
    geom_point(alpha = 0.6, size = 0.9) +
    geom_line(aes(y = fit), size = 1.2) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
    scale_x_datetime(date_labels = "%Y年%m月")


# 4 プロット用の関数定義 --------------------------------------------------------------

# ＜ポイント＞
# - 上記の可視化プロセスを関数として定義する
#   --- funcフォルダに保存済


# 関数定義
plotSSM <- function(mcmc_sample, time_vec, obs_vec = NULL,
                    state_name, graph_title, y_label,
                    date_labels = "%Y%m"){
  # 状態空間モデルを図示する関数
  #
  # Args:
  #   mcmc_sample : MCMCサンプル
  #   time_vec    : 時間軸(POSIXct)のベクトル
  #   obs_vec     : (必要なら)観測値のベクトル
  #   state_name  : 図示する状態の変数名
  #   graph_title : グラフタイトル
  #   y_label     : y軸のラベル
  #   date_labels : 日付の書式
  #
  # Returns:
  #   ggplot2により生成されたグラフ
  
  # すべての時点の状態の、95%区間と中央値
  result_df <- data.frame(t(apply(
    X = mcmc_sample[[state_name]],
    MARGIN = 2, quantile, probs = c(0.025, 0.5, 0.975)
  )))
  
  # 列名の変更
  colnames(result_df) <- c("lwr", "fit", "upr")
  
  # 時間軸の追加
  result_df$time <- time_vec
  
  # 観測値の追加
  if(!is.null(obs_vec)){
    result_df$obs <- obs_vec
  }
  
  # 図示
  p <- ggplot(data = result_df, aes(x = time)) + 
    labs(title = graph_title) +
    ylab(y_label) +
    geom_line(aes(y = fit), size = 1.2) +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) + 
    scale_x_datetime(date_labels = date_labels)
  
  # 観測値をグラフに追加
  if(!is.null(obs_vec)){
    p <- p + geom_point(alpha = 0.6, size = 0.9, 
                        data = result_df, aes(x = time, y = obs))
  }
  
  # グラフを返す
  return(p)
}

# プロット作成
plotSSM(mcmc_sample = mcmc_sample,
        time_vec = sales_df$date,
        obs_vec = sales_df$sales,
        state_name = "mu",
        graph_title = "Estimation Result of Local Level Model",
        y_label = "sales") 
