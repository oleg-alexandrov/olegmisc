n = 4;

nf = factorial(n);

for i = 0:(nf-1)

   lnf = nf;

   pos = zeros(1, n);
   
   li = i;
   for j = n:-1:1
      % Find the position of j in the permutation
      lnf = lnf/j;       % lnf = (j-1)!
      r = rem(li, lnf);  
      q = (li-r)/lnf;    % 0<= q <= j - 1
      pos(j) = j - q;
      li = r;
   end

   % Form the permutation
   perm = zeros(1, n);
   for j = n:-1:1
      count = 0;
      for l = 1:n
         if perm(l) == 0
            count = count + 1;
         end
         if count == pos(j)
            perm(l) = j;
            break;
         end
      end
   end

   disp(sprintf('%d ', perm));
end