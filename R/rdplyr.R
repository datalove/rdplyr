`%>%` <- function(x,y) {
  cl <- match.call()
  lhs <- cl[[2]] ; rhs <- cl[[3]]
  if(length(rhs) == 1) {
    y(x)
  } else {
    rhsl <- as.list(rhs)
    rhsl <- c(rhsl[1], lhs, rhsl[2:length(rhsl)]) # swap in the lhs as the first arg in rhs
    eval(as.call(rhsl))
  }
}

dots <- function(...) {
  eval(substitute(alist(...)))
}

filter <- function(.data, ...) {
  arg <- dots(...)

  for(i in seq_along(arg)) {
    r <- arg[[i]]
    .data <- .data[eval(r,.data), ]
  }
  .data
}

mutate <- function(.data, ...) {
  arg <- dots(...)
  nms <- names(arg)

  for(i in seq_along(arg)) {
    r <- arg[[i]] ; n <- nms[i]
    .data[,n] <- eval(r, .data)
  }
  .data
}

group_by <- function(.data, ...) {
  arg <- dots(...)
  class(.data) <- c("grouped_df",class(.data))
  attr(.data, "grouping") <- arg
  .data
}

summarise <- function(.data, ...) {

  arg <- dots(...)
  nms <- names(arg)

  if(inherits(.data,"grouped_df")) {

    grp <- attr(.data, "grouping")
    pieces <- split(.data, lapply(grp, eval, .data), drop = TRUE)

    .data <- lapply(pieces, function(p) {
      dat <- p[1,as.character(grp)]
      for(i in seq_along(arg)) {
        dat[,nms[[i]]] <- eval(arg[[i]], p)
      }
      dat
    })
    .data <- do.call(rbind, .data)

    rownames(.data) <- NULL
    attr(.data, "grouping") <- grp # preserve grouping

    .data
  }
}