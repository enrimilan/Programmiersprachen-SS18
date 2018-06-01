package at.ac.tuwien.ps.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;

import java.util.Stack;

/**
 * Takes the top element h and the second element t (which is a list) from the data stack, creates a new list by
 * adding h as new head element to t, and pushes the new list onto the data stack. An error is reported if t is not a list.
 */
public class CombineOperator implements Operator {

    @Override
    public void execute(Context context) {
        //TODO create real implementation: this is only a dirty implementation
        Stack<Element> stack = context.getDataStack();
        Element e = stack.pop();
        Element l = stack.pop();
        l = l.deepCopy();
        l.setValue(l.getValue().substring(0, l.getValue().length()-1) + (l.getValue().length()==2?"":" ") + e.getValue() + ")");

        stack.push(l);
    }

}