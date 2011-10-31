function main()
   A=[3 2 1
      2 3 4
      2 3 5
      2 3 4];
   
   sortrows(A);

   n = 100;
   Primes = getPrimesUpTo(n);
   disp([sprintf('Primes are: ') sprintf(' %d', Primes)]);

   total = (n - 1)^2;
   Vals = zeros(total, length(Primes));

   k = 1;
   for a = 2:n

      V = factorize(a, Primes);
      disp([sprintf('Factors of %d:\t', a) sprintf(' %d', V)]);
      
      for b = 2:n
         Vals(k, :) = V*b;
         %disp(sprintf(' %d', Vals(k, :)));
         k = k + 1;
      end
   end

   numDistinct = 1;
   Vals = sortrows(Vals);
   for k = 1:(total-1)
      %disp(sprintf('%d ', Vals(k, :)));
      if ~ isequal(Vals(k, :), Vals(k+1, :))
         numDistinct = numDistinct + 1;
      end
   end
   
   disp(sprintf('Number of distinct numbers: %d', numDistinct));
   
function V = factorize(n, Primes)

% Given a number n all of whose prime factors are in Primes,
% find the vector V such that
% n = Primes(1)^V(1) * ... * Primes(np)^V(np)
% where np = length(Primes).

   np = length(Primes);
   V = zeros(1, np);

   for s = 1:np
      p = Primes(s);
      while rem(n, p) == 0
         V(s) = V(s) + 1;
         n = n/p;
      end
   end

function Primes = getPrimesUpTo(n)
   Primes = [];
   for n=2:100
      if isPrime(n)
         Primes = [Primes, n];
      end
   end
   
function val = isPrime(n)

   if n <= 1
      val = 0;
      return;
   end

   for k=2:sqrt(n)
      if rem(n, k) == 0
         val = 0;
         return;
      end
   end


   val = 1;
   
