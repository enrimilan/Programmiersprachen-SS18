package at.ac.tuwien.ps.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.stream.CommandStream;

import java.util.Stack;

/**
 * An argument (which is a list) is taken from the data stack, and the list contents (without parentheses) are
 * inserted at the end of the command stream to be executed after everything else currently being in the command stream.
 * An error is reported if the argument is no list.
 */
public class ApplyLaterOperator implements Operator {

    @Override
    public void execute(Context context) {
        Stack<Element> stack = context.getDataStack();
        CommandStream commandStream = context.getCommandStream();
        Element list = stack.pop();
        commandStream.writeToEnd(list);
    }

}