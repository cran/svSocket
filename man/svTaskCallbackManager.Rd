\name{svTaskCallbackManager}
\alias{svTaskCallbackManager}

\title{ Create task callbacks that are evaluated both from R and the socket server }

\description{
  \code{svTaskCallbackManager()} is a copy of \code{taskCallbackManager()} in
  base package, as of version 2.9.2 of R. Two important differences: (1) the top
  task created is named \code{SV-taskCallbackManager} instead of
  \code{R-taskCallbackManager}, and its tasks are executed after each top-level
  task in R console, or after execution of non-hidden R code from the socket
  server (take care: only once per set of code, no mather the number of
  top-level task in the R code send by the client in the second case). All
  taskCallbacks defined by \code{addTaskCallback()} or
  \code{taskCallbackManager$add()} from base package are not executed when code
  is invoked from the R socket server!
}

\usage{
svTaskCallbackManager(handlers = list(), registered = FALSE, verbose = FALSE)
}

\arguments{
  \item{handlers}{ this can be a list of callbacks in which each element is a
    list with an element named \code{f} which is a callback function, and an
	optional element named \code{data} which is the 5-th argument to be supplied
	to the callback when it is invoked. Typically this argument is not
	specified, and one uses add to register callbacks after the manager is
	created. }
  \item{registered}{ a logical value indicating whether the \code{evaluate}
    function has already been registered with the internal task callback
	mechanism. This is usually \code{FALSE} and the first time a callback is
	added via the add function, the evaluate function is automatically
	registered. One can control when the function is registered by specifying
	\code{TRUE} for this argument and calling \code{addTaskCallback()} manually. }
  \item{verbose}{ a logical value, which if \code{TRUE}, causes information to
    be printed to the console about certain activities this dispatch manager
	performs. This is useful for debugging callbacks and the handler itself. }
}

\value{
  See \code{?taskCallbackManager} for both the returned object and how to use it.
}

\author{Slightly modified from the original R core team's function by
  Ph. Grosjean <phgrosjean@sciviews.org>}

\seealso{ \code{\link[base]{taskCallbackManager}}, \code{\link{processSocket}} }

\keyword{ IO }

\concept{ task callback }
