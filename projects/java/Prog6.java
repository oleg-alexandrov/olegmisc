import java.util.*;
import java.io.*;

class AppleBase{
    double m_x;
    int    m_k;
    AppleBase(double x, int k){
        System.out.println("Now in the AppleBase constructor");
        m_x = x;
        m_k = k;
    }
}

interface AppleShape1{
    void Shape1();
}

interface AppleShape2{
    void Shape2();
}


class Apple extends AppleBase implements AppleShape1, AppleShape2{
    Apple(double x, int k){
        super(x, k);
        System.out.println("Now in the Apple constructor");
    }

    static void ApplePie(){
        System.out.println("Now in ApplePie static function");
    }

    public void Shape1(){
        System.out.println("A re-implementation of Shape1");
    }

    public void Shape2(){
        System.out.println("A re-implementation of Shape2");
    }

}

class Prog6{

    private static class Pear{
        Pear(){
            System.out.println("Now in the pear constructor");
        }
        void doSomething(){
            System.out.println("Now in doSomething of Pear");
        }
    }
    
    public static void main(String[] args) throws Exception{

        System.out.println("hi there");

        Apple U = new Apple(3.0, 5);

        // Calling a re-implementation of an interface
        U.Shape1(); 
        U.Shape2(); 
        
        // Calling a static function
        Apple.ApplePie(); 

        // Static class
        Pear P = new Pear();
        P.doSomething();
                
        double[] a = {4.0, 3.0, 4.5};
        System.out.println("Size of a is " + a.length);

        double [] b = new double[7];
        System.out.println("Size of b is " + b.length);

        ArrayList<Integer> Q = new ArrayList<Integer>();
        
        System.out.println("size of Q is " + Q.size());
        Q.add(6);
        Q.add(8);
        System.out.println("size of Q is " + Q.size());
        Q.remove(0);
        System.out.println("size of Q is " + Q.size());
        System.out.println("First elem is " + Q.get(0));

        Integer [] S = new Integer[0];
        S = Q.toArray(S);
        System.out.println("First elem of S is " + S[0]);

        // Reading text
        BufferedReader F = new BufferedReader(new FileReader("in.txt"));
        String Str = F.readLine();
        System.out.println("Read the line: " + Str);

        // Booleans
        boolean z = true;
        System.out.println("boolean z is " + z);
        System.out.println("Its negation is " + !z);
    }

}