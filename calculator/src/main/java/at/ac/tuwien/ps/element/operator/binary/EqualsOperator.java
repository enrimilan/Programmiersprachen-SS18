package at.ac.tuwien.ps.element.operator.binary;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.element.operator.Operator;
import at.ac.tuwien.ps.element.operator.OperatorException;

import java.util.Stack;

/**
 * If applied to two equal lists or two equal integers, then the result is 1, otherwise the result is 0.
 */
public class EqualsOperator implements Operator {

    @Override
    public void execute(Context context) {
        Stack<Element> stack = context.getDataStack();
        if(stack.size() < 2)
            throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> Stack needs to contain at least 2 elements but has " + stack.size());

        String first = stack.pop().getValue();
        String second = stack.pop().getValue();
        String result = first.equals(second)? "1" : "0";
        stack.push(new Element(result, ElementType.INTEGER));
    }

}
