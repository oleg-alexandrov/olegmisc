function val = is_prime(n)

   if n <= 1
      val = 0;
      return;
   end
   
   for k=2:sqrt(n)
      if rem(n, k) == 0;
         val = 0;
         return;
      end
   end

   val = 1;