\name{evalServer}
\alias{evalServer}

\title{ Evaluate R code in a server process }
\description{
  This function is designed to connect two R processes together using
  the socket server. This function allows for piloting the server R process from
  a client R process, to evaluate R code in the server and return its results
  to the client.
}

\usage{
evalServer(con, expr, send = NULL)
}

\arguments{
  \item{con}{ a socket connection with the server (see examples). }
  \item{expr}{ an R expression to evaluate in the server. }
  \item{send}{ optional data to send to the server. }
}

\value{
  The object returned by the last evaluation in the server.
}

\author{Matthew Dowle (\email{m.dowle@wintoncapital.com})}

\seealso{ \code{\link{sendSocketClients}} }

\examples{
\dontrun{
## Start an R process and make it a server
require(svSocket)
startSocketServer()

## Start a second R process and run this code in it (the R client):
require(svSocket)

## Connect with the R socket server
con <- socketConnection(host = "localhost", port = 8888, blocking = FALSE)

L <- 10:20
L
evalServer(con, L)             # L is not an the server, hence the error
evalServer(con, L, L)          # Send it to the server
evalServer(con,L)              # Now it is there
evalServer(con, L, L + 2)
L
evalServer(con, L)

## More examples
evalServer(con,"x = 42")       # Set x
evalServer(con,"y = 10")
evalServer(con, x + y)         # Don't need quotes
evalServer(con, "x + y")       # but you can put quotes if you like
evalServer(con, x)             # Same as get x
evalServer(con, "x + Y")       # Return server side-error to the client
evalServer(con, x)             # Keep working after an error
evalServer(con, "x = 'a'")     # Embedded quotes are OK

## Examples of sending data
evalServer(con, X, -42)        # Alternative way to assign to X
evalServer(con, Y, 1:10)
evalServer(con, X + Y)
X  # Generates an error, X is not here in the client, only on the server
evalServer(con, X)
evalServer(con, "Z <- X + 3")  # Send an assignment to execute remotely
evalServer(con, X + Z)
evalServer(con, "Z <- X + 1:1000; NULL")   # Same but prevents Y being returned
evalServer(con, length(Z))
Z <- evalServer(con, Z)        # Bring it back to client
Z

## Close connection with the R socket server
close(con)

## Now, switch back to the R server process and check
## that the created variables are there
L
x
y
X
Y
Z
}
}

\keyword{ IO }
\keyword{ utilities }

\concept{ stateful socket server interprocess communication }
