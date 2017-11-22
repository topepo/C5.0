#' Plot a decision tree
#' 
#' Plot a decision tree. 
#' 
#' 
#' @param x an object of class `C5.0`
#' @param trial an integer for how many boosting iterations are
#'  used for prediction. NOTE: the internals of `C5.0` are
#'  zero-based so to get the initial decision tree you must use
#'  `trial = 0`. If `trial` is set too large, it is reset
#'  to the largest value and a warning is given.
#' @param subtree an optional integer that can be used to isolate
#'  nodes below the specified split. See
#'  [partykit::party()] for more details.
#' @param ...  options passed to [partykit::plot.party()]
#' @return No value is returned; a plot is rendered.
#' @author Mark Culp, Max Kuhn
#' @seealso [C5.0()], [partykit::party()]
#' @references Quinlan R (1993). C4.5: Programs for Machine
#'  Learning. Morgan Kaufmann Publishers,
#'  \url{http://www.rulequest.com/see5-unix.html}
#' @keywords models
#' @examples
#' 
#' mod1 <- C5.0(Species ~ ., data = iris)
#' plot(mod1)
#' plot(mod1, subtree = 3)
#' 
#' 
#' mod2 <- C5.0(Species ~ ., data = iris, trials = 10)
#' plot(mod2) ## should be the same as above
#' 
#' ## plot first weighted tree
#' plot(mod2, trial = 1)
#' 
#' @export
#' @method plot C5.0
#' @importFrom stats model.frame model.weights as.formula na.omit
#' @importFrom stats delete.response
#' @importFrom partykit as.partynode partynode partysplit as.party
#' @importFrom partykit fitted_node party 
#' @importFrom graphics plot
plot.C5.0 <- function(x,
                      trial = 0,
                      subtree = NULL,
                      ...) {
  if (x$rules != "")
    stop("tree models only", call. = FALSE)
  if (trial > x$trials["Actual"] - 1) {
    warning(
      paste(
        "Only",
        x$trials["Actual"],
        "trials are in the model.",
        "Setting 'trial' to",
        x$trials["Actual"] - 1,
        "(the plot code is zero-based)."
      ),
      call. = FALSE
    )
    trial <- x$trials["Actual"] - 1
  }
  x <- as.party(x, trial = trial)
  if (!is.null(subtree)) {
    if (subtree < 0 || subtree > length(x))
      stop("For this model, 'subtree' should be between zero and ",
           length(x),
           call. = FALSE)
    else
      x <- x[subtree]
  }
  if (any(names(list(...)) == "trials")) {
    warning(
      "The option 'trials' was passed and will be ignored. ", "
      Did you mean to use 'trial'?",
      call. = FALSE
    )
  }
  plot(x, ...)
}

model.frame.C5.0 <- function (formula, ...) {
  if (!is.null(formula$model))
    return(formula$model)
  mf <- formula$call
  mf <- mf[c(1L, match(
    c("formula", "data", "subset", "na.action",
      "weights"),
    names(mf),
    0L
  ))]
  if (is.null(mf$na.action))
    mf$na.action <- na.omit
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- as.name("model.frame")
  env <- if (!is.null(environment(formula$Terms)))
    environment(formula$Terms)
  else
    parent.frame()
  mf <- eval(mf, env)
  
  rsp <- strsplit(paste(formula$call[2]), " ")[[1]][1]
  tmp <- mf[rsp]
  mf[, 1:length(formula$pred)] <- mf[formula$pred]
  mf[, length(formula$pred) + 1] <- tmp
  names(mf) <- c(formula$pred, rsp)
  if (any(is.na(names(mf)))) {
    i1 <- as.vector(which(is.na(names(mf))))
    if (is.na(match("(weights)", names(mf)))) {
      names(mf)[i1] <- "(weights)"
    }
  }
  
  return(mf)
}

as.party.C5.0 <- function(obj, trial = 0, ...) {
  out <- strsplit(obj$output, "\n")[[1]]
  out <- out[out != ""]
  out <- out[grep("^\t", out, invert = TRUE)]
  out <- out[grep("\\*\\*\\*", out, invert = TRUE)]
  tr <- as.vector(obj$trials)[2]
  if (tr > 1) {
    if (trial > (tr - 1)) {
      trial = tr - 1
    }
    
    iv1 <- match(paste("-----  Trial ", trial, ":  -----", sep = ""), out)
    out <- out[grep("^\t", out, invert = TRUE)]
    iv2 <- match(paste("-----  Trial ", trial + 1, ":  -----", sep = ""), out)
    if (is.na(iv2)) {
      iv2 <- grep("Evaluation on training data", out)
    }
    out <- out[(iv1 + 2):(iv2 - 1)]
  } else{
    indx <- 1:which(out == "Decision tree:")
    out <- out[-indx]
    l1 = length(out)
    out <- out[1:(l1 - 2)]
  }
  check1 <- cbind(grep("\\{", out), grep("\\}", out))
  indv1 <- which(check1[, 1] != check1[, 2])
  if (length(indv1) > 0) {
    a17 <- check1[indv1, , drop = FALSE]
    rml = NULL
    for (j in 1:dim(a17)[1]) {
      nterms = diff(a17[j, ])
      vlaps = NULL
      for (i in 1:nterms) {
        iv37 <- out[a17[j, 1]:a17[j, 2]][-1][i]
        if (i == nterms) {
          vlap <- strsplit(iv37, ":")[[1]]
          arv12 = gsub(" ", "", vlap[(length(vlap) - 1)][1])
          if (is.na(match("", arv12))) {
            vlap <- paste(arv12, vlap[length(vlap)], sep = ":")
          } else{
            vlap = vlap[length(vlap)]
            vlap = gsub(" ", "", vlap)
            vlap <- paste(vlap, ":", sep = "")
          }
        } else{
          v1 <- strsplit(iv37, " ")[[1]]
          vlap <- v1[length(v1)]
        }
        vlaps = paste(vlaps, vlap, sep = "")
      }
      vlap <- paste(out[a17[j, 1]], vlaps, sep = "")
      out[a17[j, 1]] = vlap
      pts <- unique(sort((a17[j, 1] + 1):a17[j, 2]))
      rml = c(rml, pts)
    }
    out = out[-rml]
  }
  indtrees <- grep("SubTree", out)
  
  if (length(indtrees) > 0) {
    while (length(indtrees) > 0) {
      xval <-
        t(sapply(1:length(indtrees), function(i)
          grep(paste("[S", i, "]", sep = ""), out, fixed = TRUE)))
      end1 = length(out)
      j = length(indtrees)
      ind.x = xval[j, 1]
      torb <- sapply(1:length(obj$predictors), function(i) {
        v <- grep(obj$predictors[i], out[ind.x], fixed = TRUE)
        if (length(v) < 1)
          v = 0
        v
      })
      adj <- strsplit(out[ind.x], obj$pred[which(torb > 0)])[[1]][1]
      ind.y <- xval[j, 2] + 1
      
      stree <- paste(adj, out[ind.y:end1], sep = "    ")
      
      out <- c(out[1:ind.x], stree, out[-c(1:ind.x, ind.y:end1)])
      out <- out[-length(out)]
      out[ind.x] = gsub(paste(" \\[S", length(indtrees), "\\]", sep = ""), "", out[ind.x])
      indtrees <- grep("SubTree", out)
    }
  }
  is.default <- length(grep("default", paste(obj$call[1]))) > 0
  if (!is.default) {
    mf <- model.frame(obj)
  } else{
    xspot <- match("x", names(obj$call))[1]
    yspot <- match("y", names(obj$call))[1]
    wspot <- match("weights", names(obj$call))[1]
    if (is.na(wspot)) {
      mf <-
        data.frame(x = eval(parse(text = paste(obj$call)[xspot])), y = eval(parse(text =
                                                                                    paste(obj$call)[yspot])))
      names(mf) <- c(obj$pred, "y")
    } else{
      mf <-
        data.frame(eval(parse(text = paste(obj$call)[xspot])), eval(parse(text =
                                                                            paste(obj$call)[yspot])), eval(parse(text = paste(obj$call)[wspot])))
      ind1 <- length(names(mf)) - 1
      ind2 <- length(names(mf))
      names(mf) <- c(obj$pred, "y", "(weights)")
    }
  }
  if (length(out) == 1) {
    pn <- as.partynode(partynode(1L), from = 1L)
  } else{
    f.mat <- lapply(1:length(out), function(i) {
      a1 <- strsplit(out[i], " ")[[1]]
      a1 <- gsub(":", "", a1)
      a1 <- gsub("\\.\\.\\.", "", a1)
      a1 <- a1[a1 != ""]
      a1
    })
    indvars <- sapply(1:length(f.mat), function(i) {
      v = match(obj$predictors, f.mat[[i]][1])
      a1 <- which(!is.na(v))
      if (length(a1) == 0)
        a1 = 0
      a1
    })
    treestr <- sapply(1:length(out), function(i) {
      avec = obj$predictors[indvars[i]]
      avec = nchar(strsplit(out[[i]], avec)[[1]][1])
      avec
    })
    treestr <- as.numeric(as.factor(treestr))
    
    indclass <- sapply(1:length(f.mat), function(i) {
      v <- match(obj$lev, f.mat[[i]][4])
      if (any(!is.na(v))) {
        which(!is.na(v))
      } else{
        NA
      }
    })
    cuts <- sapply(1:length(f.mat), function(i)
      f.mat[[i]][3])
    vars <-
      sapply(1:length(f.mat), function(i)
        strsplit(f.mat[[i]][2], "=")[[1]][1])
    n.cat <-
      sapply(1:length(obj$pred), function(i)
        is.factor(mf[, obj$pred[i]]))
    xlevels <- list()
    if (sum(n.cat) > 0) {
      r1 = 1
      for (i in 1:length(n.cat)) {
        if (n.cat[i]) {
          xlevels[[r1]] <- list(varid = obj$pred[i], lev = levels(mf[, obj$pred[i]]))
          r1 = r1 + 1
        }
      }
    }
    
    c5.split <-
      function(i, j, r, k = NULL) {
        ##i=variable, j=cuts, r=TRUE,xlevs
        if (!n.cat[i]) {
          partysplit(
            varid = as.integer(i),
            breaks = as.numeric(j[1]),
            right = r,
            info = k,
            prob = NULL
          )
        } else{
          ind1 <-
            match(obj$pred[i],
                  sapply(1:length(xlevels), function(i)
                    xlevels[[i]]$varid))
          xlev <- xlevels[[ind1]]$lev
          lj = length(j)
          a1s = sapply(j, function(i)
            strsplit(i, ","))
          
          a1s <- sapply(j, function(i1) {
            a1 = strsplit(i1, ",")[[1]]
            if (length(a1) > 1) {
              a1[1] <- strsplit(a1[1], "\\{")[[1]][2]
              a1[length(a1)] <- strsplit(a1[length(a1)], "\\}")[[1]]
            }
            a1
          })
          index = rep(NA, length(xlev))
          for (i1 in 1:lj) {
            index[match(a1s[[i1]], xlev)] = as.integer(i1)
          }
          partysplit(
            varid = as.integer(i),
            index = index,
            info = k,
            prob = NULL
          )
        }
      }
    
    c5.node <- function(tvec, vvec, bvec, vvars) {
      if (length(tvec) == 1 | any(tvec < 0)) {
        return(partynode(1L))
      }
      l <- list()
      ind <- which(tvec == 1)
      lind <- length(ind)
      ind2 <- !vvars[ind[1]] == ">"
      
      split1 <- c5.split(vvec[ind[1]], bvec[ind], TRUE)
      
      
      for (i in 1:lind) {
        str = ind[i]
        if (i == lind) {
          term = length(tvec)
        } else{
          term = ind[i + 1] - 1
        }
        val = ind[i]:term
        
        l[[i]] = list(
          tvec = tvec[val] - 1,
          vec = vvec[val],
          bvec = bvec[val],
          vvars = vvars[val]
        )
      }
      if (!ind2) {
        tmp = l[[1]]
        l[[1]] = l[[2]]
        l[[2]] = tmp
      }
      partynode(1L,
                split = split1,
                kids = lapply(1:length(l), function(i) {
                  i = l[[i]]
                  c5.node(i$tvec, i$vec, i$bvec, i$vvars)
                }))
    }
    pn <- as.partynode(c5.node(treestr, indvars, cuts, vars), from = 1L)
  }
  if (is.default) {
    if (is.na(wspot)) {
      p <- dim(mf)[2]
      dat1 <- data.frame(
        "(fitted)" = fitted_node(pn, data = mf),
        "(response)" = mf[, p],
        check.names = FALSE
      )
    } else{
      p <- dim(mf)[2] - 1
      dat1 <- data.frame(
        "(fitted)" = fitted_node(pn, data = mf),
        "(response)" = mf[, p],
        "(weights)" = model.weights(mf),
        check.names = FALSE
      )
    }
    fn = as.formula(paste("y ~ ", paste(obj$pred, collapse = " + "), sep =
                            ""))
    g7 <- party(pn,
                data = mf[0L, ],
                fitted = dat1 ,
                terms = terms(fn))
  } else{
    p1 <- match(strsplit(paste(obj$call[2]), " ")[[1]][1], names(mf))
    if (is.na(p1)) {
      stop("Error in Response")
    }
    
    C5.0_fitted <- function(p1) {
      ret <- as.data.frame(matrix(nrow = NROW(mf), ncol = 0))
      ret[["(fitted)"]] <- fitted_node(pn, data = mf)
      ret[["(response)"]] <- mf[, p1]
      ret[["(weights)"]] <- model.weights(mf)
      ret
    }
    fitted <- C5.0_fitted(p1)
    g7 <-
      party(
        pn,
        data = mf[0L, , drop = FALSE],
        fitted = fitted,
        terms = terms(mf),
        info = list(method = "C5.0")
      )
  }
  
  class(g7) <- c("constparty", class(g7))
  g7
}


