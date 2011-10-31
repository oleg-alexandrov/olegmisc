% Find the sum of all numbers which are equal to the sum of the
% factorial of their digits.

sum = 0; 
r = 8;
last = 7*factorial(9);
for n = 10:last

   if rem(n, 10000) == 0
      disp(sprintf('%d', n));
      disp(sprintf('%d', last));
      disp(sprintf('sum is %d', sum));
   end
   
   str = sprintf('%d', n);
   k = length(str);
   digits = zeros(1, k);

   sum_factorial = 0;
   for l = 1:k
      digits(l) = str(l) - '0';
      sum_factorial = sum_factorial + factorial(digits(l));
   end
   
   %disp([sprintf('%d: ', n), sprintf(' %d', digits)]);
   %disp(sprintf('sum_factorial is %d', sum_factorial));
   if n == sum_factorial
      disp(sprintf('%d equals to the sum of factorial of its digits', n));
      sum = sum + n;
      disp(sprintf('sum is %d', sum));
   end
   
end

disp(sprintf('sum is %d', sum));