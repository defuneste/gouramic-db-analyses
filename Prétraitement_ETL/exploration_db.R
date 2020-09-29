#### test de connection de le base via R + test dm


source("code.R") # ici j'ai mis codes/pwd/port/adresse c'est en .gitignore

pkgs <-  c("DBI","RPostgreSQL", "dm",  "microbenchmark")
inst <- lapply(pkgs, library, character.only = TRUE)

drv <- dbDriver("PostgreSQL")
usr <- "postgres"

con <- dbConnect(
    drv,
    user = usr,
    password = pwd,
    dbname = db,
    host = adresse,
    port = port
)

dbListTables(con)

dbExistsTable(con, "t_sujet") 


list_table <- c("t_sujet", "t_adresse", "p_t_adresse_interval", "t_interval_date")

my_dm <- dm_from_src(con, list_table = c("t_sujet"), schema = c("gou"), learn_keys = F)
my_dm



tbl(con, from = "gou.t_adresse")

dbDisconnect(con)