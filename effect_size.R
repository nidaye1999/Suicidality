library(boot)
logit_pred=function(model, data){
  return(logit(stackPred(data, "suicidality_T4", 5, 5, "result/stack/rNCV/combine/", "combine")[["suicidal"]]))
}

# single independent variable
library(progress)
partial_dependence=function(model, data, interested_input, predict_function){
  result=c()
  data_temp=data
  pb=progress_bar$new(format = "[:bar] :percent eta: :eta", total = length(data[[interested_input]]), clear = FALSE)
  for (value in data[[interested_input]]){
    data_temp[[interested_input]]=value
    result=c(result, mean(predict_function(model, data_temp)))
    pb$tick()
  }
  return(data.frame(x=data[[interested_input]], y_partial=result))
}

effect_size=function(model, data, interested_input, predict_function=predict){
  pd=partial_dependence(model, data, interested_input, predict_function)
  linear_regression=lm(y_partial~x, pd)
  print(coefficients(linear_regression)[["x"]])
}
