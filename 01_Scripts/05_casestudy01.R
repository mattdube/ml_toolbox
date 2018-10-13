library(caret)
library(C50)
library(tictoc)
library(here)

# load dataset from C50 library
data(churn)

# summarize the targete variable
table(churn_y) / length(churn_y)

# create train/test indexes
set.seed(42)
myFolds <- createFolds(churn_y, k = 5)

# compare class distribution
i <- myFolds$Fold1
table(churn_y[i]) / length(i)

# use folds to create trainControl object
# this will give us exact same cv folds for each model
myControl <- trainControl(
    summaryFunction = twoClassSummary,
    classProbs = TRUE,
    verboseIter = TRUE,
    savePredictions = TRUE,
    index = myFolds
)

# fit a glmnet model to churn dataset
set.seed(42)
model_glmnet <- train(
    churn_x, churn_y,
    metric = "ROC",
    method = "glmnet",
    tuneGrid = expand.grid(
        alpha = 0:1,
        lambda = 0:10/10
    ),
    trControl = myControl
)

# plot the glmnet model
plot(model_glmnet)

# plot the glmnet coefficients
plot(model_glmnet$finalModel)

# Fit a random forest model to the churn dataset
set.seed(42)
model_rf <- train(
    churn_x, churn_y,
    metric = "ROC",
    method = "ranger",
    trControl = myControl
)

# plot the rf model
plot(model_rf)

######## Compare Models #########
# Make a list
model_list <- list(
    glmnet = model_glmnet,
    rf = model_rf
)

# Collect resamples from the CV folds
resamps <- resamples(model_list)

# Summarize the results
summary(resamps)

# box and whisker plot comparison
bwplot(resamps, metric = "ROC")

# dotplot comparison
dotplot(resamps, metric = "ROC")

# density plot comparison
densityplot(resamps, metric = "ROC")

# scatterplot comparison
xyplot(resamps, metric = "ROC")

# fit a stacked ensemble model
library(caretEnsemble)

model_list <- caretList(churn_x, churn_y,
          methodList = c("glmnet", "ranger"),
          trControl = myControl
    )

stack <- caretStack(model_list, method = "glm")
summary(stack)
