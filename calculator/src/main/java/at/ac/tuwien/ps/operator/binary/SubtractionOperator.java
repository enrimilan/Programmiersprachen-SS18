package at.ac.tuwien.ps.operator.binary;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.operator.Operator;

import java.util.Stack;

public class SubtractionOperator implements Operator {

    @Override
    public void execute(Context context) {
        Stack<Element> stack = context.getDataStack();
        int first = Integer.parseInt(stack.pop().getValue());
        int second = Integer.parseInt(stack.pop().getValue());
        int result = first - second;
        stack.push(new Element(String.valueOf(result), ElementType.INTEGER));
    }

}