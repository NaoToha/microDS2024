library(tidyverse)

##
pitching2023 <- read.csv("baseball_data/npb_Pitching2023.csv")

df <- pitching2023 %>% 
  mutate(Team = str_replace_all(Team, c("Yokohama"= "DeNA", "Daiei" = "SoftBank"))) %>% 
  filter(Year >= 2005) %>% 
  dplyr::filter(ERA < 20)

team_win <- pitching2023 %>% 
  group_by(Team) %>% 
  summarise(
    total_win = sum(W)
    )

w_graph <- ggplot(team_win, aes(x = Team, y = total_win)) +
  geom_col()

w_graph

summary(pitching2023)



theater_rate<- ggplot(df, aes(x = W, y = ERA)) +
  geom_point(alpha=0.1)+
  geom_smooth()

relate_to_win <-ggplot(df, aes(x = W, y = ERA)) +
  geom_point(alpha=0.25)+
  geom_smooth(method = "lm")

theater_rate
relate_to_win


install.packages("corrr") # 必要に応じて一度だけインストール 
library(corrr)

cor_matrix <- correlate(df)
print(cor_matrix)

df_new <- df[,-c(1:12)] 


cor_matrix <- round(cor(df_new), 2)
cor_data <- as.data.frame(as.table(cor_matrix))

ggplot(data = cor_data, aes(Var1, Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1), space = "Lab", name="Correlation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)) +
  coord_fixed()

df_2023 <-  df_new %>% 
  filter(IP>0) %>% 
  mutate(
    IP = case_when(
      floor(IP) == IP~IP,
      round(IP %% 1, 1) == 0.1 ~floor(IP)+0.333,
      round(IP %% 1, 1) == 0.2 ~floor(IP)+0.667
    )
  )

#IPが1/3が.1、2/3が.2となっており、数値計算に問題が生じるため、
#以上のコードにて問題を解決した。

# クラスタリング用のデータを選択
# 例として、登板数 (G), 奪三振数 (SO), 防御率 (ERA) を使用
clustering_data <- pitching2023 %>%
  select(G, SO, ERA) %>%
  na.omit() # 欠損値を除去

# データの正規化（スケーリング）
scaled_data <- scale(clustering_data)

# k-means クラスタリング
set.seed(123) # 再現性のためにシードを設定
k <- 7 # クラスタ数を設定（ここでは3を例として使用）
kmeans_result <- kmeans(scaled_data, centers = k)

# クラスタリング結果をデータに追加
clustering_data <- clustering_data %>%
  mutate(Cluster = as.factor(kmeans_result$cluster))

# 結果をプロット
ggplot(clustering_data, aes(x = G, y = ERA, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "クラスタリング結果", x = "登板数 (G)", y = "防御率 (ERA)") +
  theme_minimal()

#なんの変数をカーネル関数に入れる。
#カーネル密度関数の視覚的表示
df_mitudo <- density(df_new$ERA)
plot(df_mitudo)

ggplot(df_new, aes(x = ERA, y = W)) +
  geom_point() +
  geom_density2d()

ggplot(df_new, aes(x = ERA)) +
  geom_density()


qmodel <- lm(ERA ~ Weight + Height, data = df)
qmodel

# 残差の取得
residuals <- resid(qmodel)

# 残差の散布図
plot(qmodel$fitted.values, residuals, 
     xlab = "Fitted values", 
     ylab = "Residuals", 
     main = "Residuals vs Fitted Values")
abline(h = 0, col = "red")  # y = 0の基準線を追加

df <- df |>
  mutate(transfer = 0) %>% 
  mutate(omit = 0)

for (i in 2:1951){
  if (df$Player.ID[i] == df$Player.ID[i-1]){
    df$transfer[i]=1
  }
}

for (i in 2:1951){
  if (df$Player.ID[i] == df$Player.ID[i-1] & 
      df$Year[i] != df$Year[i-1]+1){
    df$omit[i] <- 1
  }
  
}


for (i in 2:1951){
  if (df$Player.ID[i] == df$Player.ID[i-1] & 
      df$Team[i] != df$Team[i-1]){
    df$transfer[i] <- 1
  }
  
}
