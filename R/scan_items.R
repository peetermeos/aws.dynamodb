#' @title Scan returns one or more items and item attributes by accessing every item in a Dynamo DB table or a secondary index.
#' @description Get one or more items and item attributes from a Dynamo DB table
#' @param table A character string specifying the table name, or an object of class \dQuote{aws_dynamodb_table}.
#' @param filterExpression A string that contains conditions that DynamoDB applies after the Scan operation, but before the data is returned to you. Items that do not satisfy the FilterExpression criteria are not returned.
#' @param expressionAttributeValues One or more values that can be substituted in an expression. Use the : (colon) character in an expression to dereference an attribute value.
#' @param \dots Additional arguments passed to \code{\link{dynamoHTTP}}.
#' @return A list.
#' @references
#'   \href{https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_Scan.html}{API Guide: Scan}
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
#'   # scan items
#'   scan_items("Music", "Artist IN (:artist)", list(":artist" = "No One You Know"))
#' 
#'   # cleanup
#'   delete_table(tab)
#' }
#' @export
scan_items <- function(table, filterExpression, expressionAttributeValues, ...) {
    bod <- list(TableName = get_tablename(table),
                FilterExpression = filterExpression,
                ExpressionAttributeValues = map_attributes(expressionAttributeValues))
    out <- dynamoHTTP(verb = "POST", body = bod, target = "DynamoDB_20120810.Scan", ...)
    if (length(out$Items)) {
        return(out$Items)
    } else {
        return(NULL)
    }
}
