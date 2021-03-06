#' @title Get Dynamo DB Item
#' @description Get an item from a Dynamo DB tables
#' @param table A character string specifying the table name, or an object of class \dQuote{aws_dynamodb_table}.
#' @param item A list of key-value pairs. If the table only has a primary key, this should specify the primary key attribute name and value for the desired item. If a composite primary key is used, then both attribute names and values must be specified.
#' @param \dots Additional arguments passed to \code{\link{dynamoHTTP}}.
#' @return A list.
#' @references
#'   \href{https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_GetItem.html}{API Guide: GetItem}
#'   \href{https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_BatchGetItem.html}{API Guide: BatchGetItem}
#' @examples
#' \dontrun{
#'   tab <- create_table(
#'     table = "Music",
#'     attributes = list(Artist = "S"),
#'     primary_key = "Artist"
#'   )
#'   
#'   # put item
#'   put_item("Music", list(Artist = "No One You Know", SongTitle = "Call Me Today"))
#' 
#'   # get item
#'   get_item("Music", list(Artist = "No One You Know"))
#' 
#'   # cleanup
#'   delete_table(tab)
#' }
#' @export
get_item <- function(table, item, ...) {
    bod <- list(TableName = get_tablename(table),
                Key = map_attributes(item))
    out <- dynamoHTTP(verb = "POST", body = bod, target = "DynamoDB_20120810.GetItem", ...)
    if (length(out$Item)) {
        return(out$Item)
    } else {
        return(NULL)
    }
}
