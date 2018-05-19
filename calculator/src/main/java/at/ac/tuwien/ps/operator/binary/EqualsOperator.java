package at.ac.tuwien.ps.operator.binary;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.operator.Operator;

import java.util.Stack;

/**
 * If applied to two equal lists or two equal integers, then the result is 1, otherwise the result is 0.
 */
public class EqualsOperator implements Operator {

    @Override
    public void execute(Context context) {
        Stack<Element> stack = context.getDataStack();
        String first = stack.pop().getValue();
        String second = stack.pop().getValue();
        String result = first.equals(second)? "1" : "0";
        stack.push(new Element(result, ElementType.INTEGER));
    }

}
