function main()

   n = 10^10;

   sum = 0;
   for i=1:1000
      sum = rem(sum + last_ten(i, n), n);
   end

   disp(sprintf('sum is %0.14g', sum));
   
function prod = last_ten(k, n)
   
   prod = 1;
   for l = 1:k
      prod = rem(prod*k, n);
   end

   disp(sprintf('number is %0.14g, rem is %0.14g', k^k, prod));

   