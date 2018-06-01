package at.ac.tuwien.ps.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.stream.CommandStream;

import java.util.Stack;

/**
 * Stops the execution of the calculator.
 */
public class ExitOperator implements Operator {

    @Override
    public void execute(Context context) {
        CommandStream commandStream = context.getCommandStream();
        Stack<Element> stack = context.getDataStack();
        commandStream.clear();
        stack.clear();
    }

}