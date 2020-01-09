n = 5;

A = 1:n;
count = 0;
while (1)

   count = count+1;
   disp([sprintf('%d:\t', count) sprintf('%d ', A)]);

   isNext = 0;
   
   for s = 1:n
      if A(s) ~= n + 1 - s
         isNext = 1;
         break;
      end
   end

   if isNext == 0;
      break;
   end

   p = n + 1 - s;
   if s > 1
      A = [A(s:n) (p+1):n];
   end

   for q = 1:n
      if A(q) == p
         break;
      end
   end

   % swap
   A(q)   = A(q-1);
   A(q-1) = p;

end