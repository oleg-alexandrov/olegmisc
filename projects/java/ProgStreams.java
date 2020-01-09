import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.*;
import java.util.*; 

class ProgStreams{
    
    static public void main(String args[]) throws Exception{
        
        System.out.println("The number of args is " + args.length);

        BufferedReader br = new BufferedReader( new FileReader("in.txt") );
        String s;
        while( (s = br.readLine()) != null ){
            System.out.println("Reading " + s);
        }

        BufferedWriter bw = new BufferedWriter(new FileWriter("out.txt"));
        bw.write("sample write\n");
        bw.close();

        String n1 = "in1.txt"; File f1 = new File(n1);
        String n2 = "in2.txt"; File f2 = new File(n2);

        if (f1.exists()){
            System.out.println("File " + n1 + " exists");
        }else{
            System.out.println("File " + n1 + " does not exist");
        }
        
        if (f2.exists()){
            System.out.println("File " + n2 + " exists");
        }else{
            System.out.println("File " + n2 + " does not exist");
        }

    }

}