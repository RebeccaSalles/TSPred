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