mx = 4e6;

fpp = 0;
fp  = 1;

sum = 0;

while (1)

   fc = fpp + fp;
   if (fc > mx)
      break
   end

   if rem(fc, 2) == 0
      sum = sum + fc;
   end

   disp(sprintf('%d\t%d', fc, sum));
   
   fpp = fp;
   fp = fc;

end
   