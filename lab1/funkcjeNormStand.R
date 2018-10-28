fnorm = function(x)
{
  return (x-min(x))/(max(x)-min(x))
}


fstand = function(x)
{
  return ((x-mean(x))/sd(x))
}