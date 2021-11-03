#r <- plumb("API_7.R")
#r$run(port=9000)
#curl --data "x=5&y=3" http://localhost:9000/function 

#* @apiTitle Diabetes Prediction
#* @apiDescription AI-based prediction using neural networks. With an accuracy of 0.962 , kap of 0.864 and, F-score of 0.905

#* Insert the variables 
#* @get /connect
function(age,gender){
  age <- strtoi(age)
  test <- data.frame( Age = c(age), Gender = c(gender) )
  test <- test %>%
    mutate(Gender=factor(Gender))
  test
}
#* Insert the variables 
#* @get /diabetes
function(Age,Gender,Polyuria,Polydipsia,SuddenWeightLoss,Weakness,Polyphagia,
         GenitalThrush,VisualBlurring,Itching,
         Irritability,DelayedHealing,PartialParesis,MuscleStiffness,Alopecia,
         Obesity){
  Age <- strtoi(Age)
  load("diabetes.RData")
  Diagnosis = "NA"
  test <- data.frame(Age = c(Age),
                     Gender = c(Gender),
                     Polyuria=c(Polyuria),
                     Polydipsia=c(Polydipsia),
                     SuddenWeightLoss=c(SuddenWeightLoss),
                     Weakness=c(Weakness),
                     Polyphagia=c(Polyphagia),
                     GenitalThrush=c(GenitalThrush),
                     VisualBlurring=c(VisualBlurring),
                     Itching=c(Itching),
                     Irritability=c(Irritability),
                     DelayedHealing=c(DelayedHealing),
                     PartialParesis=c(PartialParesis),
                     MuscleStiffness=c(MuscleStiffness),
                     Alopecia=c(Alopecia),
                     Obesity=c(Obesity),
                     Diagnosis=c(Diagnosis))
  test <- test %>% 
    mutate(
      Gender=factor(Gender),
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
      MuscleStiffness=factor(MuscleStiffness),
      Alopecia=factor(Alopecia),
      Obesity=factor(Obesity))
  predict(fitNeural1TrainBest,test)
}
