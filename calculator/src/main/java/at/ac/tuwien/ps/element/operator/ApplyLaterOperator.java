package at.ac.tuwien.ps.element.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.CommandStream;

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
        if(stack.size() < 1)
            throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> Stack needs to contain at least 1 element but has " + stack.size());

        CommandStream commandStream = context.getCommandStream();
        Element list = stack.pop();
        if(list.getElementType() != ElementType.LIST)
            throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> Element is not a list.");

        commandStream.appendList(list);
    }

}