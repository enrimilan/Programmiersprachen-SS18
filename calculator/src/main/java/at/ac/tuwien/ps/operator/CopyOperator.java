package at.ac.tuwien.ps.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;

import java.util.Stack;

/**
 * Replaces the top element n on the data stack with a copy of the nth element on the data stack (counted from the top of
 * the stack). An error is reported if n is not a positive number.
 */
public class CopyOperator implements Operator {

    @Override
    public void execute(Context context) {
        Stack<Element> stack = context.getDataStack();
        Element top = stack.peek();
        int index = stack.size() - Integer.parseInt(top.getValue()) - 1;
        stack.pop();
        stack.push(stack.get(index));
    }

}
