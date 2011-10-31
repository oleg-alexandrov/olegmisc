function main()

   sum = 0;
   maxN = 200;
   Table = zeros(1, maxN);

   fid = fopen('out.txt', 'w');
   
   for n=1:maxN
      [val, Table] = do_min_powers(n, Table);
      sum = sum + val;
      fprintf(fid, '%d %d\n', n, val);
   end

   fprintf(fid, 'sum is %d\n', sum);

   fclose(fid);
   
function [val, Table] = do_min_powers(n, Table)

   if n == 1
      val = 0;
      Table(n) = val;
      return;
   end
   
   if n == 2
      val = 1;
      Table(n) = val;
      return;
   end

   if Table(n) ~= 0
      val = Table(n);
      return;
   end
   
   val = 10^20;

   for d=2:sqrt(n)
      if rem(n, d) == 0
         [val1, Table] = do_min_powers(d, Table);
         [val2, Table] = do_min_powers(n/d, Table);
         val = min(val, val1 + val2);
      end
   end

   for r=1:(n-1)
      [val1, Table] = do_min_powers(r, Table);
      [val2, Table] = do_min_powers(n-r, Table);
      val = min(val1 + val2 + 1, val);

%      k = r;
%      l = n - r;
%      if k < l & rem(l, k) == 0
%         [val1, Table] = do_min_powers(k, Table);
%         [val2, Table] = do_min_powers(l/k, Table);
%         val = min(val1 + val2 + 1, val);
%      end
   end

   Table(n) = val;

