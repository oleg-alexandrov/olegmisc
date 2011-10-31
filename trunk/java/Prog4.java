import Fruit2.Orange;
import java.util.ArrayList; 

class Grape{
    private double m_x;
    Grape(double x){
        System.out.println("Now in the constructor of Grape");
        m_x = x;
    }
    void setVal(double x){
        m_x = x;
    }
    double getVal(){
        return m_x;
    }
    
    public String toString(){ // Overwrite the default
        return "Grape: " + m_x;
    }
}


abstract class Stone{
    Stone(double x, int z){
        System.out.println("Now in the Stone constructor");
    }
    void action1(){
        System.out.println("Doing Stone action 1");
    }
}

class Diamond extends Stone{

    Diamond(double x, int z){
        super(x, z); // Call the constructor of the base class
        System.out.println("Now in the Diamond constructor");
    }
    void action1(){
        System.out.println("Doing Diamond action 1");
        super.action1(); // Call the member function in the base class
    }
}

class Kiwi{

    public class Kiwi3{ // Note: nested class
    }
    
}

interface Apple4{

}

interface Apple5{

}

class Apple6 extends Kiwi implements Apple4, Apple5{ // Note: Using interfaces

}

class Prog4{

    final static double INTEREST_RATE = 0.05;
    
    private static double doSquare(double x){
        return x*x;
    }

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

        System.out.println("The interest rate is " + INTEREST_RATE);

        String a1 = new String(); a1 = "hi";
        String b1 = "h" +"i";
        if (a1 == b1){
            System.out.println("variables are equal: " + a1 + " " + b1);
        }else{
            System.out.println("variables are not equal: " + a1 + " " + b1);
        }
        

        String a3 = "hi";
        String a4 = a3;
        System.out.println("Strings are " + a3 + " " + a4);
        a3 += "c";
        System.out.println("Strings are " + a3 + " " + a4);

        // References to the same object
        Grape g1 = new Grape(3);
        Grape g2 = g1;
        System.out.println("Values are " + g1.getVal() + " " + g2.getVal());
        g1.setVal(8);
        System.out.println("Values are " + g1.getVal() + " " + g2.getVal());

        // Object equality compares pointers not values
        Grape g3 = new Grape(7);
        Grape g4 = new Grape(7);
        Grape g5 = g3;
        System.out.println("Are equal: " + (g3 == g4) );
        System.out.println("Are equal: " + (g3 == g5) );

        System.out.println("The object is " + g3);

        Diamond d = new Diamond(5, 3);
        d.action1();

        // Arrays
        Diamond [] dArr = new Diamond[7]; // Array of diamonds
        Grape [] g8 = null;
        int [] list = {4, 5, 6};
        System.out.println("The number of elems is " + list.length);
        

        double Q[] = new double[2];
        for (int i = 0; i < Q.length; i++){
            Q[i] = i + 5;
        }
        for (double q : Q){
            System.out.println("The value is " + q);
            q  = q - 1;
        }
        System.out.println("Now modifying the elements");
        for (double q : Q){
            System.out.println("The value is " + q);
        }

        // Book b2 = new Book(){}; // Anonymous class -- does not compile

        double[] v = new double[0]; 
        //double[] v = null; // -- will throw exception when checking length
        System.out.println("the size of v is " + v.length);

        // Dynamic arrays
        ArrayList<String> al = new ArrayList<String>();
        System.out.println("The size of the array is " + al.size());
        al.add("hi");
        al.add("there");
        for (int q = 0; q < al.size(); q++){
            System.out.println("Elem is: " + al.get(q));
        }
        System.out.println("--------");
        al.set(1, "x");
        for (int q = 0; q < al.size(); q++){
            System.out.println("Elem is: " + al.get(q));
        }
        System.out.println("--------");
        al.remove(0);
        al.add("hi2");
        for (int q = 0; q < al.size(); q++){
            System.out.println("Elem is: " + al.get(q));
        }
        System.out.println("--------");

        // Array of int does not work, need object
        //ArrayList<int> il = new ArrayList<int>();
        
        //Array of Integer works
        ArrayList<Integer> il = new ArrayList<Integer>(3);
        il.add(3); il.add(5); // Note that we use autoboxing: cast to Ingeger
        for (int s = 0; s < il.size(); s++){
            System.out.println("The value is " + il.get(s));
        }

        // Multidim arrays
        int [][] a2 = new int[3][4];
        System.out.println("The length is " + a2.length + " " + a2[0].length);

        // Exceptions
        try {
            System.out.println("Now having an exception");
            throw new Exception("Division by zero");
        }catch(Exception e){
            System.out.println("Now catching the exception");
        }finally{
            System.out.println("Now in the finally block");
        }
        
    } // End of main

}
