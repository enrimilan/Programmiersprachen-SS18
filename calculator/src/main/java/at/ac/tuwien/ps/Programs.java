package at.ac.tuwien.ps;

/**
 * A set of predefined programs
 */
public class Programs {

    public static final String RUN_PROGRAM = "((Welcome!)1w((Please write your program or enter the name of a predefined one (conditional|factorial|prime):)1w1ra s3+ (3c3< (1w1d1d)(3cc: 1 4c-3c3c4d4d4d 2ca)(3c4d1+da)a) () 2ca 0ra)0w0ra)";
    public static final String CONDITIONAL = "(0 (9~)(8)(3c4d1+da)a1w)";
    public static final String FACTORIAL = "((Enter number:)1w1ra(2c1 3c-1c1=3c()(3c4d1+da)a2d*)2c3d2ca2d1w)";

    /*
    Prime factor program explained:

    ((Enter number:)1w1ra           // Input a number
    (1c1c*3c<                       // if p*p < N then execute outer second list, else first list
        (1c3c%0=                    // if (N mod p) = 0 then execute inner second list, else first list
            (1+ 3ca)                // Not divisible, increase p by one and execute function list
            (1c1w1c3c/2c3d3d 3ca)   // Divisible, divide N by p (reorder needed) and execute function list
            (3c4d1+da)a)            // Executes inner if statement
        (1d1w1d)                    // Second list of outer if, p*p < N, outputs last factor and cleans up
        (3c4d1+da)a)                // Executes outer if statement
    2c3d 2 3ca)                     // Bootstrap, copies code to first place, sets p to 2 and execute function list
    */
    public static final String PRIME = "((Enter number:)1w1ra (1c1c*3c< (1c3c%0= (1+ 3ca)(1c1w1c3c/2c3d3d 3ca)(3c4d1+da)a)(1d1w1d)(3c4d1+da)a) 2c3d 2 3ca)";

}
