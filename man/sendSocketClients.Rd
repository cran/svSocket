\name{sendSocketClients}
\alias{sendSocketClients}

\title{ Send data to one or more clients through a socket }

\description{
  The text is send to one or more clients of the R socket server currently
  connected.
}

\usage{
sendSocketClients(text, sockets = "all", serverport = 8888)
}

\arguments{
  \item{text}{ the text to send to the client(s). }
  \item{sockets}{ the Tcl name of the client(s) socket(s) currently connected
    (sockXXX), or "all" (by default) to send the same text to all connected
    clients. }
  \item{serverport}{ the port of the server considered. }
}

\author{Philippe Grosjean (\email{phgrosjean@sciviews.org})}

\seealso{ \code{\link{closeSocketClients}}, \code{\link{processSocket}} }

\keyword{ IO }

\concept{ stateful socket server interprocess communication }
