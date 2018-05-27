package at.ac.tuwien.ps.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;

import java.util.Stack;

/**
 * Takes the top element n from the data stack and removes the nth element from the data stack (counted from the top of
 * the stack). An error is reported if n is not a positive number.
 */
public class DeleteOperator implements Operator {

    @Override
    public void execute(Context context) {
        Stack<Element> stack = context.getDataStack();
        Element top = stack.peek();
        int index = stack.size() - Integer.parseInt(top.getValue()) - 1;
        stack.remove(index);
        stack.pop();
    }
}
