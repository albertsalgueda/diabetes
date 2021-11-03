#Modeling file

#IMPORT LIBRARIES
library(readr)
library( tidyverse )
library(tidymodels)
library (mosaic)
library(ggformula)
library( lubridate )
library ( modelr)
library (discrim)
library (klaR)
library(kernlab)
library( remotes )
library(NeuralNetTools)
library( parsnipExtra )
add_neuralnet_engine()
library(plumber)

df <- read_csv("diabetes.csv")

df <- df %>% 
  mutate(Gender=factor(Gender),
         Polyuria = factor(Polyuria),
         Polydipsia=factor(Polydipsia),
         SuddenWeightLoss=factor(SuddenWeightLoss),
         Weakness=factor(Weakness),
         Polyphagia=factor(Polyphagia),
         GenitalThrush=factor(GenitalThrush),
         VisualBlurring=factor(VisualBlurring),
         Itching=factor(Itching),
         Irritability=factor(Irritability),
         DelayedHealing=factor(DelayedHealing),
         PartialParesis=factor(PartialParesis),
         Diagnosis=factor(Diagnosis),
         MuscleStiffness=factor(MuscleStiffness),
         Alopecia=factor(Alopecia),
         Obesity=factor(Obesity))
df <- as.data.frame(df)
summary(df)

set.seed( 28 )
split <- initial_split( df, prop = 0.5 )
dfTrain <- training( split )
dfTest <- testing( split )

write.csv(dfTrain, "training.csv")
write.csv(dfTest, "testing.csv")
rec <- 
  recipe(df) %>% 
  update_role(Diagnosis, new_role = "outcome") %>% 
  update_role(Age,Gender,Polyuria,Polydipsia,SuddenWeightLoss,Weakness,Polyphagia,
              GenitalThrush,VisualBlurring,Itching,
              Irritability,DelayedHealing,PartialParesis,MuscleStiffness,Alopecia,
              Obesity,new_role = "predictor") 

mdlNeural1 <- mlp( hidden_units = 1 ) %>% 
  set_engine("nnet")%>%
  set_mode("classification")

wflow <-
  workflow() %>% 
  add_model(mdlNeural1) %>% 
  add_recipe(rec)

fit <- fit(wflow,dfTrain)

predict(fit,dfTrain)

dfTrain <- dfTrain %>%
  add_predictions(fit,var="predNeural1",type="class")%>%
  mutate(predNeural1_pred_class=predNeural1$.pred_class,predNeural1 = NULL)

conf_mat( dfTrain, Diagnosis, predNeural1_pred_class )
metrics( dfTrain, Diagnosis, predNeural1_pred_class )

cvFolds <- vfold_cv( dfTrain, v=2 )


gridNeuralTune <- expand.grid( hidden_units = 1:10, penalty = seq(0,1,0.25) )
mdlNeural1Tune <- mdlNeural1 %>% 
  set_args(hidden_units=tune(),penalty=tune())
wflowNeural1Tune <- wflow %>% 
  update_model(mdlNeural1Tune)

resultsNeuralTune1 <- wflowNeural1Tune %>% 
  tune_grid(resamples=cvFolds,
            grid = gridNeuralTune,
            metrics = metric_set( accuracy, f_meas ))
show_best( resultsNeuralTune1, metric = "accuracy")
show_best( resultsNeuralTune1, metric = "f_meas")

bestNeuralParams <- select_best( resultsNeuralTune1, metric = "f_meas" )
wflowNeural1Best <- finalize_workflow( wflowNeural1Tune, bestNeuralParams )
fitNeural1TrainBest <- fit( wflowNeural1Best, dfTrain )
dfTrain <- dfTrain %>%
  add_predictions(fitNeural1TrainBest,var="predNeural1Best",type="class")%>% 
  mutate(predNeural1Best_pred_class=predNeural1Best$.pred_class,
         predNeural1Best = NULL)
metrics( dfTrain, Diagnosis, predNeural1Best_pred_class )
f_meas( dfTest, Diagnosis, predNeural1Best_pred_class )


save(fitNeural1TrainBest,file="diabetes.RData")

saveRDS(fitNeural1TrainBest, "model.rds")


