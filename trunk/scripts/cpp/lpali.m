function main()

   a = 131;

   val = is_palindrome(a);
   disp(sprintf('is palindrome: %d %d', a, val));

   maxPali = 0;
   for a=100:999
      for b=100:999
         c = a*b;
         if is_palindrome(c) & c > maxPali
            disp(sprintf('Max pali is %d x %d = %d', a, b, c));
            maxPali = c;
         end
      end
   end
   
function val = is_palindrome(a)

   if ( a == find_num(reverse(find_digits(a))) )
      val = 1;
   else
      val = 0;
   end
   
function B = reverse(A)

   B = [];
   
   for a = A
      B = [a B];
   end
   
function num = find_num(digits)

   num = 0;
   for i = 1:length(digits)
      dig = digits(i);
      num = 10*num + dig;
   end
   
function digits = find_digits(a)
   
   digits = [];
   while (a ~= 0)
      d = rem(a, 10);
      a = (a - d)/10;
      digits = [d digits];
   end