fmongo_find <- function(nm_db, nm_collec){
  mongo_credentials <- config::get(file = "conf/globalresources.yml")
  mongo_set <- mongo(db = nm_db, collection = nm_collec, url = mongo_credentials$mongoURL, verbose = TRUE)
  return(mongo_set$find())
}
