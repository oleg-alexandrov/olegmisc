q = 0;
for l=1:9
   for n=1:21
      if l^n >= 10^(n-1)
         disp(sprintf('%0.16g = %d^%d has %d digits', l^n, l, n, n));
         q = q+1;
      end
   end
end

disp(sprintf('total is %d', q));