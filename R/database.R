# =====================================================
# R/database.R
# =====================================================

library(DBI)
library(RJDBC)

conectar_banco <- function(){

    drv <- JDBC(
        driverClass = Sys.getenv("IRIS_DRIVER"),
        classPath = Sys.getenv("IRIS_JAR")
    )

    dbConnect(
        drv,
        Sys.getenv("IRIS_URL"),
        user = Sys.getenv("IRIS_USER"),
        password = Sys.getenv("IRIS_PASSWORD")
    )

}
