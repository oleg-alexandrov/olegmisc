class Prog5{

    public static void main(String[] args){
        System.out.println("Now in main, the number of args is "
                           + args.length);

        // To enable assertions, use:
        // java -enableassertions Prog5
        assert (1 == 0): "My assertion failed!";
    }

}