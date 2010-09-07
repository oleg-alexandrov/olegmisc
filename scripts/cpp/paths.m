M = 21;
N = 21;

A = zeros(M, N) + 1;

for n = 2:N
   for m = 2:M
      A(m, n) = 0;
      for k = 1:m
         A(m, n) = A(m, n) + A(k, n - 1);
      end
   end
end

A