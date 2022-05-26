library(ggplot2)
library(broom)

reality.check = function(var.list, y, df, decimals=6){
  
  f = reformulate(var.list, response = y)
  model.full = glm(f, data = df, family = "binomial")
  
  tidy.full = tidy(model.full)
  colnames(tidy.full) = c("term","JEstimate", "JSE", "JZ", "JPVal")
  
  results = NULL
  for(i in seq(1,length(var.list))){
    
    f = reformulate(var.list[i], response = y)
    model.simple = glm(f, data = df, family = "binomial")
    
    tidy.simple = tidy(model.simple)
    results = rbind(results, tidy.simple)
  }
  
  results = results[results$term != "(Intercept)",]
  colnames(results) = c("term","MEstimate", "MSE", "MZ", "MPVal")
  
  final = merge(tidy.full, results, by.x = "term", by.y="term")
  final[,2:9] = round(final[,2:9], decimals)
  final$CHECK = ifelse(sign(final$JEstimate) == sign(final$MEstimate), "PASS","FAIL")
  return(final)
}

# Example usage
mydata = read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
var.list = c("gre", "gpa", "rank")
reality.check(var.list=var.list, y="admit", df=mydata)
reality.check(var.list=var.list, y="admit", df=mydata, decimals=2)
