function factorize(n)

   while (1)
      
      for k=[2:floor(sqrt(n)) n]
         if rem(n, k) == 0
            disp(sprintf('%d', k));
            n = n/k;
            break;
         end
      end

      if n == 1
         break
      end
   end