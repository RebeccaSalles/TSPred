#Run method
run <- function(obj,...){
  UseMethod("run")
}

#Updating parameters
updt <- function(obj,...){
  UseMethod("updt")
}

objs <- function(obj,...){
  UseMethod("objs")
}

res <- function(obj,...){
  UseMethod("res")
}

preprocess <- function(obj,...){
  UseMethod("preprocess")
}

subset <- function(obj,...){
  UseMethod("subset")
}

train <- function(obj,...){
  UseMethod("train")
}

#predict is already a generic method
#predict <- function(obj,...){
#  UseMethod("predict")
#}

postprocess <- function(obj,...){
  UseMethod("postprocess")
}

evaluate <- function(obj,...){
  UseMethod("evaluate")
}