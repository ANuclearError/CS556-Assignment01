# This code includes all the functions used for creating the models and using
# them to evaluate how good they are at predicting score rank.
library(ggplot2)
library(rpart)

# Root mean square error
rmse <- function(y, f) sqrt(mean((y-f) ^ 2))

# Reading the data
steam <- read.csv("data/top100in2weeks.csv")
steam <- steam[1:100,]
# steam$ScoreGrade <- cut(steam$ScoreRank, seq(0, 100, by=10))
# steam$PriceBand <- cut(steam$Price, seq(0, 6000, by=500))

print_clusters <- function(labels, k) {
    for (i in 1:k) {
        print(paste("cluster", i))
        print(steam[labels==i, c("Name", "Publisher", "ScoreRank")])
    }
}

# Basic linear regression ignoring tags.
lr <- sort(sample(nrow(steam), nrow(steam) * 0.7))
train_b <- steam[lr, 4:13]
test_b <- steam[-lr, 4:13]

mdl_b <- lm(formula = ScoreRank ~., data = train_b)

test_b$PredLR_b <- predict(mdl_b, test_b)
pred_b <- ggplot(test_b) + geom_point(aes(x=PredLR_b, y=ScoreRank)) +
    geom_abline(intercept=0, slope=1, color="blue")
res_b <- ggplot(test_b) + geom_point(aes(x=(ScoreRank-PredLR_b), y=ScoreRank)) +
    geom_abline(intercept=0, slope=0, color="blue")
rmse_b <- rmse(test_b$ScoreRank, test_b$PredLR_b)


# Linear regression with tags
train <- steam[lr, -(1:3)]
test <- steam[-lr, -(1:3)]

mdl <- lm(formula = ScoreRank ~., data = train)

test$PredLR <- predict(mdl, test)
pred <- ggplot(test) + geom_point(aes(x=PredLR, y=ScoreRank)) +
    geom_abline(intercept=0, slope=1, color="blue")
res <- ggplot(test) + geom_point(aes(x=(ScoreRank-PredLR), y=ScoreRank)) +
    geom_abline(intercept=0, slope=0, color="blue")
rmse_t <- rmse(test$ScoreRank, test$PredLR)


steam_vars <- colnames(steam)[-(1:3)]
steam_matrix <- scale(steam[,steam_vars])
steam_center <- attr(steam_matrix, "scaled:center")
steam_scale <- attr(steam_matrix, "scaled:scale")

d <- dist(steam_matrix, method="euclidean")
steam_fit <- hclust(d, method="ward.D")
steam_plot <- plot(steam_fit, labels=steam$Name)
rect.hclust(steam_fit, k=8)

groups <- cutree(steam_fit, k=8)

princ <- prcomp(steam_matrix)
project <- predict(princ, newdata=steam_matrix)[,1:2]
project.plus <- cbind(as.data.frame(project, cluster=as.factor(groups),
    name=steam$Name))

clust_plot <- ggplot(project.plus, aes(x=PC1, y=PC2)) +
    geom_point(aes(shape=cluster, color=cluster)) +
    geom_text(aes(label=name), hjust=0, vjust=1)

save_all <- function()
    ggsave("doc/report/img/pred_b.png", pred_b) +
    ggsave("doc/report/img/pred.png", pred) +
    ggsave("doc/report/img/res_b.png", res_b) +
    ggsave("doc/report/img/res.png", res)