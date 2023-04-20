swap_columns_order <- function(df, col_origen, col_destino) {
  n_cols = dim(df)[2]
  if(col_origen > n_cols || col_origen < 1 || col_destino < 1 || col_destino > n_cols || col_origen == col_destino) {
    cat("Wrong indexes\n")
    return (-1)
  }
  
 #Se cambia el contenido de las columnas
 ret_df <- df
 back <- ret_df[col_origen]
 ret_df[col_origen] <- ret_df[col_destino]
 ret_df[col_destino] <- back
 
 #Se cambia el nombre de las columnas
 back_name <- colnames(ret_df)[col_origen]
 colnames(ret_df)[col_origen] <-colnames(ret_df)[col_destino]
 colnames(ret_df)[col_destino] <- back_name
 
 #Se ordenan las columnas según la primera de estas
 
 ret_df <- ret_df[order(ret_df[[1]]),]
  
  
  return(ret_df)
}