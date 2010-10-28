import Fruit2.*;

class Prog4{

    public static void main(String args[]){

        System.out.println("first program");

        System.out.println("Number of args is " + args.length );
        for (int i = 0; i < args.length; i++){
            System.out.println(args[i]);
            int s = Integer.parseInt(args[i]);
            System.out.println("Number is: " + s);
        }


        String S = "abvca";
        System.out.println("The length of " + S + " is " + S.length());
        System.out.println("Before replacement is: " + S);
        S = S.replaceAll("[av]", "x");
        System.out.println("After replacemnt is:   " + S);
        
        System.out.println("sqrt(4) is " + Math.sqrt(4.0));

        String a = "hi", b = "hi";
        if (a == b){
            System.out.println("Variables " + a + " and " + b + " are equal");
        }else{
            System.out.println("Vars " + a + " and " + b + " are not equal");
        }
        double x = 2.5;
        System.out.println("square of " + x + " is " + doSquare(x));

        Pear A = new Pear(7);
        System.out.println("The action 1 value is " + A.action1());

        Orange B = new Orange();
    }
    
    private static double doSquare(double x){
        return x*x;
    }
    
}
