# parse a CNF formula file
CnfParse = function(path)
{
  HEADER_LINE = 4
  CLAUSE_LINE = 5
  
  stream = file(path, open = "r")
  fileLines = readLines(stream)
  close(stream)
  
  header = strsplit(fileLines[HEADER_LINE], " ")[[1]]
  variables = as.integer(header[3])
  clauses = as.integer(header[4])
  
  cnf = list()
  for (i in CLAUSE_LINE:length(fileLines))
  {
    clause = as.integer(strsplit(fileLines[i], "  ")[[1]])
    cnf[[i + 1 - CLAUSE_LINE]] = head(clause, -1)
  }
  
  return (cnf)
}
