package at.ac.tuwien.ps.test;

import at.ac.tuwien.ps.Calculator;

/**
 * Tests for the calculator written in the calculator language
 */
public class CalculatorTest {


    private static final String IS_EXPECTED = "=(FAIL\n)(OK\n)(3c4d1+d)a1w";

    // the tests
    private static final String ADDITION_TEST = "('Addition' test: )1w 1 1+" + "2"+IS_EXPECTED;
    private static final String SUBSTRACTION_TEST = "('Substraction' test: )1w 1 1-" + "0"+IS_EXPECTED;
    private static final String DIVISION_TEST = "('Division' test: )1w 1 3/" + "3"+IS_EXPECTED;
    private static final String MULTIPLICATION_TEST = "('Multiplication' test: )1w 2 3*" + "6"+IS_EXPECTED;
    private static final String MODULO_TEST1 = "('Modulo' test 1: )1w 1 2%" + "0"+IS_EXPECTED;
    private static final String MODULO_TEST2 = "('Modulo' test 2: )1w 2 3%" + "1"+IS_EXPECTED;
    private static final String EQUALS_TEST1 = "('Equals' test 1: )1w 2 2=" + "1"+IS_EXPECTED;
    private static final String EQUALS_TEST2 = "('Equals' test 2: )1w 3 2=" + "0"+IS_EXPECTED;
    private static final String EQUALS_TEST3 = "('Equals' test 3: )1w ()2=" + "0"+IS_EXPECTED;
    private static final String EQUALS_TEST4 = "('Equals' test 4: )1w ()()=" + "1"+IS_EXPECTED;
    private static final String EQUALS_TEST5 = "('Equals' test 5: )1w (1 1+)(1 1+)=" + "1"+IS_EXPECTED;
    private static final String GREATER_TEST1 = "('GreaterThan' test 1: )1w 1 1>" + "0"+IS_EXPECTED;
    private static final String GREATER_TEST2 = "('GreaterThan' test 2: )1w 1 2>" + "1"+IS_EXPECTED;
    private static final String GREATER_TEST3 = "('GreaterThan' test 3: )1w 2 1>" + "0"+IS_EXPECTED;
    private static final String LESS_TEST1 = "('LessThan' test 1: )1w 1 1<" + "0"+IS_EXPECTED;
    private static final String LESS_TEST2 = "('LessThan' test 2: )1w 1 2<" + "0"+IS_EXPECTED;
    private static final String LESS_TEST3 = "('LessThan' test 3: )1w 2 1<" + "1"+IS_EXPECTED;
    private static final String LOGICAL_AND_TEST1 = "('LogicalAnd' test 1: )1w 0 0&" + "0"+IS_EXPECTED;
    private static final String LOGICAL_AND_TEST2 = "('LogicalAnd' test 2: )1w 0 1&" + "0"+IS_EXPECTED;
    private static final String LOGICAL_AND_TEST3 = "('LogicalAnd' test 3: )1w 1 0&" + "0"+IS_EXPECTED;
    private static final String LOGICAL_AND_TEST4 = "('LogicalAnd' test 4: )1w 1 1&" + "1"+IS_EXPECTED;
    private static final String LOGICAL_OR_TEST1 = "('LogicalOr' test 1: )1w 0 0|" + "0"+IS_EXPECTED;
    private static final String LOGICAL_OR_TEST2 = "('LogicalOr' test 2: )1w 0 1|" + "1"+IS_EXPECTED;
    private static final String LOGICAL_OR_TEST3 = "('LogicalOr' test 3: )1w 1 0|" + "1"+IS_EXPECTED;
    private static final String LOGICAL_OR_TEST4 = "('LogicalOr' test 4: )1w 1 1|" + "1"+IS_EXPECTED;

    // the test suite
    private static final String TEST_SUITE =
                    ADDITION_TEST +
                    SUBSTRACTION_TEST +
                    DIVISION_TEST +
                    MULTIPLICATION_TEST +
                    MODULO_TEST1 +
                    MODULO_TEST2 +
                    EQUALS_TEST1 +
                    EQUALS_TEST2 +
                    EQUALS_TEST3 +
                    EQUALS_TEST4 +
                    EQUALS_TEST5 +
                    GREATER_TEST1 +
                    GREATER_TEST2 +
                    GREATER_TEST3 +
                    LESS_TEST1 +
                    LESS_TEST2 +
                    LESS_TEST3 +
                    LOGICAL_AND_TEST1 +
                    LOGICAL_AND_TEST2 +
                    LOGICAL_AND_TEST3 +
                    LOGICAL_AND_TEST4 +
                    LOGICAL_OR_TEST1 +
                    LOGICAL_OR_TEST2 +
                    LOGICAL_OR_TEST3 +
                    LOGICAL_OR_TEST4;

    public static void main(String[] args) {
        Calculator calculator = new Calculator();
        calculator.initialize("(" + TEST_SUITE + ")");
        calculator.run(false);
    }
}
