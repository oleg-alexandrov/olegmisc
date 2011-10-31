a=600851475143;
a  = 12;
%a = 1345*337;

factors = [];
while (1)

   success = 0;
   for p = [2:sqrt(a) a]
      if rem (a, p) == 0
         disp(sprintf('A factor is %d', p));
         factors = [factors, p];
         a = a/p;
         success = 1;
         break;
      end
      
   end

   if a == 1
      break;
   end
   
end

factors