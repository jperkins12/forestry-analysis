# latex_export

require(stargazer)

# read clipboard data
read.excel <- function(header=FALSE,...) {
  read.table("clipboard",sep="\t",header=header,...)
}

dat = read.excel()

stargazer(dat, summary = FALSE, header = FALSE, rownames = FALSE, colnames = FALSE)