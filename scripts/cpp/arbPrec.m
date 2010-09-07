function main(pow)

   k = 200;
   v = zeros(1, k);
   v(k) = 1;

   for l = 1:pow
      v = timesNum(v, l);
      showNum(v);
   end

   disp(sprintf('sum of digits of %d! is %d', pow, sum(v)));

function dummy = showNum(v)

   dummy = 0;
   
   k = length(v);
   s = [];
   for i=1:k
      d = sprintf('%d', v(i));
      s = [s d];
   end

   disp(sprintf('%s', s));
   
   
function v = timesNum(a, f)

   k = length(a);

   v = zeros(1, k);
   
   for j=1:k

      w = zeros(1, k);
      num = f*a(j);
      d0 = rem(num, 10); num = (num - d0)/10;
      d1 = rem(num, 10); num = (num - d1)/10;
      d2 = rem(num, 10); num = (num - d2)/10;

      %[d0 d1 d2]
      w(j) = d0;
      if j > 1
         w(j - 1) = d1;
      end
      if j > 2
         w(j - 2) = d2;
      end

      v = v + w;

      for l=k:-1:2
         if v(l) >= 10
            v(l-1) = v(l-1) + (v(l) - rem(v(l), 10))/10;
            v(l) = rem(v(l), 10);
         end
      end
      
   end

   