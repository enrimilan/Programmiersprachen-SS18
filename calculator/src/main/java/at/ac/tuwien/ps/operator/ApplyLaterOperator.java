package at.ac.tuwien.ps.operator;

import at.ac.tuwien.ps.Context;

/**
 * An argument (which is a list) is taken from the data stack, and the list contents (without parentheses) are
 * inserted at the end of the command stream to be executed after everything else currently being in the command stream.
 * An error is reported if the argument is no list.
 */
public class ApplyLaterOperator implements Operator {

    @Override
    public void execute(Context context) {
        //TODO
    }

}