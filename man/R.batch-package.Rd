%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  999.package.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{R.batch-package}
\alias{R.batch-package}
\alias{R.batch}
\docType{package}

\title{Package R.batch}


\description{
  The package provides methods to batch run R scripts in a straightforward manner. A job batch is defined by a root directory containing subdirectories todo/, running/, finished/, failed/, src/ among others. A job to be processed is a directory placed in todo/. A job is processed by moving it to running/, then sourcing the R files in src/ (containing common code) and then the ones in the job directory (contain job specific code), in which a function onRun() has to be defined, which is then called. Succesfully jobs end up in finished/ whereas failed jobs in failed/.

  This package should be considered to be in "alpha" version.
}

\section{Installation}{
  To install this package, see \url{http://www.braju.com/R/}.
  Required packages are installed in the same way.
}

\section{Dependancies and other requirements}{
  This package depends on the \pkg{R.oo}, \pkg{R.io}, and
  \pkg{R.lang} packages.
  It is a cross-platform package, because it is written in pure \R.
}

\section{To get started}{
  To get started, see:
  \enumerate{
    \item \code{\link{JobBatch}}.
    \item \code{\link{main.JobBatch}}() (to start a job from the command line).
  }
}

\section{How to cite this package}{
  Currently no information.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}
\keyword{package}
