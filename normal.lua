local spi = math.sqrt(2*math.pi)
function normal(x,sig,mean)
  if sig == nil then 
    mean = x.mean
    sig = x.sig
    x = x.val
  end
  return 1.0/(sig*spi) * math.exp(-0.5*((x - mean)/sig)^2)
end
