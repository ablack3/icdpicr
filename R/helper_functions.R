
#insert columns after named column
.insert_columns <- function(df, colname, df_to_insert){
    stopifnot(nrow(df) == nrow(df_to_insert))
    stopifnot(is.data.frame(df) & is.data.frame(df_to_insert) & is.character(colname))
    stopifnot(colname %in% names(df))

    target <- which(names(df) == colname)[1]

    if(target == ncol(df)){
        df_out <- cbind(df[,1:target,drop=F], df_to_insert)
    } else {
        df_out <- cbind(df[,1:target,drop=F], df_to_insert, df[,(target+1):ncol(df),drop=F])
    }
    df_out
}

# select columns from one of the icd 10 lookup tables
# prefix = "NIS"
# i10_type = "cm"
.select_i10_data <- function(prefix, i10_type) {
    stopifnot(i10_type %in% c("cm", "base"))
    stopifnot(prefix %in% c("NIS", "TQIP", "NIS_only", "TQIP_only"))

    if(i10_type == "cm") {
        df <- i10cm_map_roc
    } else if(i10_type == "base") {
        df <- i10base_map_roc
    }
    df <- df[ , c("dx", paste(prefix, c("severity", "issbr"), sep = "_"))]
    colnames(df) <- c("dx", "severity", "issbr")
    df
}

# select coefficient columns from one of the icd 10 lookup tables
# prefix = "NIS"
# i10_type = "cm"
.select_i10_coef <- function(prefix, i10_type) {
    stopifnot(i10_type %in% c("cm", "base"))
    stopifnot(prefix %in% c("NIS", "TQIP", "NIS_only", "TQIP_only"))

    if(i10_type == "cm") {
        df <- i10cm_map_roc
    } else if(i10_type == "base") {
        df <- i10base_map_roc
    }
    df <- df[ , c("dx", paste(prefix, c("effect", "intercept"), sep = "_"))]
    colnames(df) <- c("dx", "effect", "intercept")
    df
}

#' The default ICD 10 ISS method
#'
#' This function looks up the i10_iss_method environment variable which may be set by adding `i10_iss_method=roc_max_NIS` to
#' the .Rprofile file. The function also informs the user of the difference between the methods so an informed choice can be made.
#'
#' @return The default ICD 10 ISS method to be used as a character string.
#' @export
default_i10_iss_method <- function() {

    i10_methods <- c("roc_max_NIS", "roc_max_TQIP", "roc_max_NIS_only", "roc_max_TQIP_only" ,"gem_max", "gem_min")
    if(Sys.getenv("i10_iss_method") != "") {
        if(Sys.getenv("i10_iss_method") %in% i10_methods) {
            return(Sys.getenv("i10_iss_method"))
        } else {
            stop(paste0("The environment variable i10_iss_method is set to `",
                        Sys.getenv("i10_iss_method"),
                        "` but must be unset or one of",
                        paste(i10_methods, collapse = ", ")))
        }
    }

    rlang::inform(paste("Consider setting reference database for the ICD 10 injury mapping as it can make a difference",
                        "Use TQIP if your data is more similar to a registry and NIS if your data is administrative.",
                        "You can also set the environment variable i10_iss_method to override the default method.",
                        collapse = "/n"),
                  .frequency = "once",
                  .frequency_id = "i10_method_message")

    return("roc_max_NIS") # the default method
}
