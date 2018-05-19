package at.ac.tuwien.ps.operator;

import at.ac.tuwien.ps.stream.CommandStream;
import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;

import java.util.Stack;

/**
 * An argument (which is a list) is taken from the data stack, and the list contents (without parentheses) are inserted
 * at the begin of the command stream to be executed next. An error is reported if the argument is no list.
 */
public class ApplyImmediatelyOperator implements Operator {

    @Override
    public void execute(Context context) {
        Stack<Element> stack = context.getDataStack();
        CommandStream commandStream = context.getCommandStream();
        Element list = stack.pop();
        commandStream.write(list);
    }

}