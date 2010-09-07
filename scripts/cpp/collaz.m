function main()

   maxChainStart = 0;
   maxChainVal   = 0;
   for i =1:999999
      val = chain(i);
      
      if rem(i, 1000) == 0
         disp(sprintf('ind %d val %d', i, val));
      end
      
      if val > maxChainVal
         maxChainVal = val;
         maxChainStart = i;
      end
      
   end

   disp(sprintf('max chain start is %d', maxChainStart));
   
function count = chain(n)

   count = 1;
   
   while (1)

      if rem(n, 2) == 0
         n = n/2;
      else
         n = 3*n + 1;
      end

      count = count + 1;
      
      if n == 1
         break
      end
   end