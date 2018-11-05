# measure chromosome fitness for given CNF formula
CnfFitness = function(cnf, chromosome)
{
  clausesToSatisfy = length(cnf)
  
  for (clauseNumber in 1:length(cnf))
  {
    clauseSatisfied = FALSE
    clause = cnf[[clauseNumber]]
    
    for (variableNumber in 1:length(clause))
    {
      variable = clause[variableNumber]
      
      if (variable > 0)
      {
        if (chromosome[variable] == 1) { clauseSatisfied = TRUE }
      }
      else
      {
        if (chromosome[-variable] == 0) { clauseSatisfied = TRUE }
      }
    }
    
    if (clauseSatisfied == TRUE)
    {
      clausesToSatisfy = clausesToSatisfy - 1
    }
  }
  
  return (clausesToSatisfy)
}
