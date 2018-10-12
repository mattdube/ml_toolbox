library(caret)
library(here)
library(tictoc)

data(mtcars)
set.seed(42)

# generate some missing data
mtcars[sample(1:nrow(mtcars), 10), "hp"] <- NA
Y <- mtcars$mpg
X <- mtcars[, 2:4]

# use linear model "recipe"
set.seed(42)
model <- train(
    X, Y, method = "glm",
    preProcess = c("medianImpute", "center", "scale")
)

print(min(model$results$RMSE))

# working with constant and near constant variables
# identify near zero variance predictors
remove_cols <- nearZeroVar(bloodbrain_x, names = TRUE,
                            freqCut = 2, uniqueCut = 20)

all_cols <- names(bloodbrain_x)

# remove from data
bloodbrain_x_small <- bloodbrain_x[, setdiff(all_cols, remove_cols)]

# fit model on reduced data
model_bb <- train(x = bloodbrain_x_small, y = bloodbrain_y, method = "glm")

model_bb

# load blood-brain dataset, and fit a basic glm model
data(BloodBrain)
names(bbbDescr)[nearZeroVar(bbbDescr)]

set.seed(42)
model <- train(
    bbbDescr, logBBB, method = "glm",
    trControl = trainControl(method = "cv", number = 10, verbose = TRUE),
    preProcess = c("zv", "center", "scale")
)

min(model$results$RMSE)

# fit a glm model with low-variance predictors removed
set.seed(42)
model_2 <- train(
    bbbDescr, logBBB, method = "glm",
    trControl = trainControl(method = "cv", number = 10, verbose = TRUE),
    preProcess = c("nzv", "center", "scale")
)

min(model_2$results$RMSE)

# add PCA
set.seed(42)
model_3 <- train(
    bbbDescr, logBBB, method = "glm",
    trControl = trainControl(method = "cv", number = 10, verbose = TRUE),
    preProcess = c("zv", "center", "scale", "pca")
)

min(model_3$results$RMSE)
