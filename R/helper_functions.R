
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

