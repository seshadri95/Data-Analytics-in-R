#' The constructor for the qfifo
#' @return An S3 object of class qfifo
#' @examples
#' q <- qfifo()
#' @export
qfifo <- function(){
  structure(list(data=list()), class ="qfifo")
}


#' Pushes a value to the queue
#'
#' @param q is the current fifo queue object
#' @param val is the value to be added to the queue
#'
#' @return The updated queue object
#' @export
#'
#' @examples
#' q <- qfifo()
#' q <- add(q,1234)
add <- function(q, val){
  UseMethod("add")
}

#' @export
add.qfifo <- function(q, val){
  q$data[length(q$data)+1] <-val
  q
}

#' Return the top value in the queue
#'
#' @param q is the current fifo queue object
#'
#' @return The top of the queue
#' @export
#'
#' @examples
#' q <- qfifo()
#' q <- add(q,1234)
#' v <- top(q)
#' @export
top <- function(q){
  UseMethod("top")
}

#' @export
top.qfifo <- function(q){
  # get the top element
  if(length(q$data) == 0)
    stop("No elements on the queue")
  q$data[[1]]
}

#' Delete the top element from the queue
#'
#' @param q is the current fifo queue object
#'
#' @return The modified queue
#' @export
#'
#' @examples
#' q <- qfifo()
#' q <- add(q,1234)
#' q <- add(q,5678)
#' q <- process(q)
#' @export
process <- function(q){
  UseMethod("process")
}

#' @export
process.qfifo <- function(q){
  # get the top element
  if(length(q$data) == 0)
    stop("No elements on the queue to pop")
  q$data[[1]] <- NULL
  q
}
