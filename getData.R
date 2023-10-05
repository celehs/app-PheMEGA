
con <- function(){
  DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = as.numeric(Sys.getenv("DB_PORT")),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PSWD")
  )
}

readDB <- function(sql, tname){
  on.exit(RPostgres::dbDisconnect(con()), add = TRUE)
  conn <- con()
  if(tname %in% RPostgres::dbListTables(conn)){
    RPostgres::dbGetQuery(conn = conn, sql)
  } else {
    print(paste0(tname, " is't in the database"))
    NULL
  }
}

getData <- function(id, tname){
  field <- ifelse(grepl("^\\d.+", id, perl = TRUE), "snp", "trait")
  sql <- paste0("SELECT * FROM ", tname, " WHERE ", field, " = '", id, "';")
  df <- readDB(sql, tname)
  if(field == "trait"){
    colnames(df)[1:2] <- c("from", "to")
  } else {
    colnames(df)[1:2] <- c("to", "from")
  }
  df
}


getTable <- function(tname){
  sql <- paste0('SELECT * FROM "', tname, '";')
  readDB(sql, tname)
}


searchDict <- function(text, tname){
  sql = paste0("select * from ", tname, " where id ilike '%", text, "%' or \"desc\"  ilike '%", text, "%' or \"group\"  ilike '%", text, "%';")
  readDB(sql, tname)
}

