# 問い: SDGsのインデックススコアが優秀な国はなにゆえ優秀なのか

# SDGsのインデックススコアが高い国とそのほかの数値の相関を見ることで、SDGsのインデックススコアが高い国の特徴を探る。

# ######################################
# まずはデータの準備
# ######################################
data <- read.csv("2023-sdgs-data.csv")

# データフレームのカラム名を出力
column_names <- colnames(data)
write.table(column_names, file = "column_names.txt", row.names = FALSE, col.names = FALSE, quote = FALSE) # nolint

# 2023 SDGs Index Scoreを抽出。
(sdgs_index_score <- data$`X2023.SDG.Index.Score`[1:193])

# 変動係数を取得
(sdgs_index_score_cv <- sd(sdgs_index_score, na.rm = TRUE) / mean(sdgs_index_score, na.rm = TRUE)) # nolint
# -> 0.1515398 (15.15%)

# ######################################
# いくつかの変数との相関を見る
# ######################################

# 1. 人口との相関を見る
(population_data <- data$`Population.in.2022`[1:193])
plot(sdgs_index_score, population_data, xlab="SDGs Index Score", ylab="Population") # nolint
# -> 相関なさそう。

# 2. データの欠損率との相関を見る
(percentage_missing_values <- data$`Percentage.missing.values`[1:193])
plot(sdgs_index_score, percentage_missing_values, xlab="SDGs Index Score", ylab="Percentage missing values") # nolint
# -> 相関なさそう。

(international_spillovers_score <- data$`International.Spillovers.Score..0.100.`[1:193]) # nolint
plot(sdgs_index_score, international_spillovers_score, xlab="SDGs Index Score", ylab="International Spillovers Score") # nolint
# -> 若干相関がありそう。

# 3. 1~17のゴールごとのスコアとの相関を見る（散布図を出力）
for (i in 1:17) {
  # ファイル名を設定
  file_name <- paste0("output/SDG_Goal_", i, "_Score_Plot.png")

  # PNGファイルを開始
  png(file_name)

  # カラム名を動的に生成
  column_name <- paste0("Goal.", i, ".Score")

  # 対応するSDGスコアを抽出
  goal_score <- data[[column_name]][1:193]

  # SDGs Index Scoreと特定のSDGスコアのプロット
  plot(sdgs_index_score, goal_score, xlab="SDGs Index Score", ylab=paste0("SDG ", i, " Score")) # nolint

  # デバイスを閉じる（ファイルを保存）
  dev.off()
}

# 4. 1~17のゴールごとのスコアとの相関を見る（相関係数を出力）

correlation_coefficients <- c()

for (i in 1:17) {
  # カラム名を動的に生成
  column_name <- paste0("Goal.", i, ".Score")

  # 対応するSDGスコアを抽出
  goal_score <- data[[column_name]][1:193]

  # 相関係数を出力
  correlation_coefficient <- cor(sdgs_index_score, goal_score, use = "complete.obs") # nolint

  # 相関係数を配列に追加
  correlation_coefficients <- c(correlation_coefficients, correlation_coefficient) # nolint
}

# 相関係数を出力
correlation_coefficients

# 各ゴールごとの相関係数を棒グラフで出力し、棒グラフをPNGファイルとして出力
png("output/SDG_Goal_Correlation_Coefficient_Plot.png")
barplot(correlation_coefficients, names.arg = 1:17, xlab = "SDG Goal", ylab = "Correlation coefficient") # nolint
dev.off()
