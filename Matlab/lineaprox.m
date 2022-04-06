function [m,b]=lineaprox(x,y)
  try
    x;
    y;
  catch
    x=input("x = ");
    y=input("y = ");
  end_try_catch
  
  if size(x,1)==1
    x=x';
  endif
  
  if size(y,1)==1
    y=y';
  endif
  
  assert(size(y,1)==size(x,1),"The size of inputs are not the same");
  
  t=0;
  
  for n=1:16384
    
    a=abs(sum(y-x*tan(t)));
    b=abs(sum(y-x*tan(t+pi/(4*n))));
    c=abs(sum(y-x*tan(t-pi/(4*n))));
    
    %assert(or(a==b,a==c,b==c),"error in linearity");
    if and(b<a,b<c)
      t=t+pi/(4*n);
    elseif and(c<a,c<b)
      t=t-pi/(4*n);
    endif
    
  endfor
m=tan(t);
a=x*m;
b=sum(y)/size(x,1)-sum(a)/size(x,1);
endfunction
