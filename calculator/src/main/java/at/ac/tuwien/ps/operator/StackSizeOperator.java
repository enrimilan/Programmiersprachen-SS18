package at.ac.tuwien.ps.operator;

import java.util.Stack;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;

/**
 * Pushes the number of stack entries onto the stack.
 */
public class StackSizeOperator implements Operator {

    @Override
    public void execute(Context context) {
    	Stack<Element> stack = context.getDataStack();
    	stack.push(new Element(String.valueOf(stack.size()), ElementType.INTEGER));
    }
}