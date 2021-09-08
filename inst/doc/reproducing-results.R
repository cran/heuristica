## -----------------------------------------------------------------------------
library(heuristica)

## -----------------------------------------------------------------------------
data_set <- city_population_original
criterion_col <- 3    # Population
cols_to_fit <- 4:ncol(data_set) # The 9 cues

reg <- regInterceptModel(data_set, criterion_col, cols_to_fit)
ttb <- ttbModel(data_set, criterion_col, cols_to_fit)
unit <- unitWeightModel(data_set, criterion_col, cols_to_fit)
min <- minModel(data_set, criterion_col, cols_to_fit)

## -----------------------------------------------------------------------------
results <- percentCorrectList(data_set, list(reg, ttb, unit, min))
# Round values to make comparison easier.
round(results)

## -----------------------------------------------------------------------------
# Cross-validate the data over the vector of models, suing training-proportion
# of the data to train and the rest as a test set.  Outputs the mean percent
# correct for each model in fitting and in prediction.
crossV <- function(vec_of_models, criterion_col, cols_to_fit, data, reps, training_proportion){
  fitting <- vector()
  prediction <- vector()
  for(i in 1:reps){

    #randomly sample training and test row indexes
    train <- sample(1:nrow(data), nrow(data)*training_proportion)
    test <- setdiff(1:nrow(data), train)

    #create training and test set
    training_set <- data[train,]
    test_set <- data[test,]

    # If a regression is overdetermined (e.g. has too many columns(), it will
    # drop the right-most columns.  To instead make it drop random columns,
    # we shuffle the column order.
    shuffled_cols_to_fit <- sample(cols_to_fit)

    models<-list()
    y <- 0
    for (mod in vec_of_models) { #fit the models to the training_set
      y <- y+1
      models[[y]] <- mod(training_set, criterion_col, shuffled_cols_to_fit)
    }

    #calculate percentage of correct predictions
    fittingAccuracy <- percentCorrectList(training_set, models)
    predictionAccuracy <- percentCorrectList(test_set, models)
    fitting <- rbind(fitting,fittingAccuracy)
    prediction <- rbind(prediction,predictionAccuracy)
  }

  results <- (rbind(colMeans(fitting),colMeans(prediction)))
  rownames(results) <- c("Fitting","Prediction")
  results
}

## -----------------------------------------------------------------------------
set.seed(0) # Use the same seed if you want to reproduce same results.
reps <- 200 # The book used 10,000 but that takes minutes.
training_proportion <- 0.5

results <- crossV(c(regInterceptModel, ttbModel, unitWeightModel, minModel), criterion_col, cols_to_fit, data_set, reps,training_proportion)
# Round values to make comparison easier.
round(results, 1)

## ----fig.width=7, fig.height=5------------------------------------------------
library(ggplot2)
library(reshape)
p <- melt(results)
colnames(p) <- c("condition","model","value")
ggplot(p, aes(x=condition, y=value, colour=model,group=model)) + geom_point() + geom_line() + ylab("Proportion correct")

