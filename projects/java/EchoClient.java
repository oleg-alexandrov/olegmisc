import java.net.*;
import java.io.*;

// A client for our multithreaded EchoServer.
public class EchoClient{

    
    public static void main(String[] args){
        // First parameter has to be machine name
        if(args.length == 0){
            System.out.println("Usage : EchoClient <serverName>");
            return;
        }
        
        int maxNum = 15;
        System.out.println("Max num is " + maxNum);

        Socket s = null;
        
        // Create the socket connection to the EchoServer.
        try{
            s = new Socket(args[0], 12111);
        }catch(UnknownHostException uhe){
            // Host unreachable
            System.out.println("Unknown Host :" + args[0]);
            s = null;
        }catch(IOException ioe){
            // Cannot connect to port on given host
            System.out.println("Cant connect to server at 12111. Make sure it is running.");
            s = null;
        }
        
        if(s == null) System.exit(-1);
        
        BufferedReader inStream = null;
        PrintWriter outStream = null;
        
        try{
            // Create the streams to send and receive information
            inStream = new BufferedReader(new InputStreamReader(s.getInputStream()));
            outStream = new PrintWriter(new OutputStreamWriter(s.getOutputStream()));
            
            // Since this is the client, we will initiate the talking.
            // Send a string.
            String maxNumStr = Integer.toString(maxNum);
            System.out.println("Will ask a number >= 0 and less than " + maxNumStr);
            outStream.println(maxNumStr);
            outStream.flush();
            // receive the reply.
            System.out.println("Server says : " + inStream.readLine());
            
            outStream.flush();
        }catch(IOException ioe){
            System.out.println("Exception during communication. Server probably closed connection.");
        }finally{
            try{
                // Close the streams
                outStream.close();
                inStream.close();
                // Close the socket before quitting
                s.close();
            }
            catch(Exception e){
                e.printStackTrace();
            }                
        }        
    } // end main
    
} // end class