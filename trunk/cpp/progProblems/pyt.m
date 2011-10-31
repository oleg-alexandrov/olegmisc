for a=1:500
   %disp(sprintf('a = %d', a));
   for b = 1:(1000-a)
      c = 1000 - a - b;
      if a > 0 & b > 0 & c > 0 & a^2 + b^2 - c^2 == 0
         disp(sprintf('triplet is %d %d %d', a, b, c));
      end
   end
end