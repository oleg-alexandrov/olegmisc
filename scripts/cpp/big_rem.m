function main()

   a = 1777;
   n = 10^8; 

   val = 1;
   
   for b = 1:10
      val2 = do_big_rem(a, val, n);
      disp(sprintf('Remainder of dividing %d^%d by %d is %d', ...
                   a, val, n, val2));
      val = val2;
   end

   val = do_big_rem(a, a, n)
   
function val = do_big_rem(a, b, n)

   % Find the remainder of dividing a^b by n.

   a = rem(a, n);

   if b == 0
      val = 1;
      return;
   end

   if b == 1
      val = a;
      return;
   end
   
   r = rem(b, 2);

   q = (b-r)/2; % b = 2*q+r
   
   val = rem(a^r, n) * do_big_rem( rem(a^2, n), q, n );

   val = rem(val, n);