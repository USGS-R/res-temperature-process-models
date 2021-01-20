#' Login to ScienceBase using the "cidamanager" user credentials from dssecrets package
#' 
#' @details To use `dssecrets`, ensure you have file system access to the cidamanager
#' secret among the files where `dssecrets` is installed.
#' If you prefer, you can bypass `dssecrets` by manually calling
#' sbtools::authenticate_sb('[my_sb_user]'), 
#' provided your account has permission to access the SB item(s).
sb_secret_login <- function(){
  if (!sbtools::is_logged_in()){
    sb_secret <- dssecrets::get_dssecret("cidamanager-sb-srvc-acct")
    sbtools::authenticate_sb(username = sb_secret$username, password = sb_secret$password)
  }
}
