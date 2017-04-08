
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
