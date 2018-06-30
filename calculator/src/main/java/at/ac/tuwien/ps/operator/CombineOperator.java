package at.ac.tuwien.ps.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.parsing.ParsingTools;

import java.util.List;
import java.util.Stack;

/**
 * Takes the top element h and the second element t (which is a list) from the data stack, creates a new list by
 * adding h as new head element to t, and pushes the new list onto the data stack. An error is reported if t is not a list.
 */
public class CombineOperator implements Operator {

    private ParsingTools parsingTools = new ParsingTools();

    @Override
    public void execute(Context context) {
        Stack<Element> stack = context.getDataStack();

        if(stack.size() < 2)
            throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> Stack needs to contain at least 2 elements but has " + stack.size());

        Element h = stack.pop();
        Element t = stack.pop();

        if(t.getElementType() != ElementType.LIST)
            throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> The element is not a list");

        String list = t.getValue();
        String listContent = list.substring(1, list.length() - 1);

        List<Element> elements = parsingTools.parseElements(listContent);

        if(!elements.isEmpty()) {
            Element head = elements.get(elements.size()-1);
            if(head.getElementType() == ElementType.INTEGER && h.getElementType() == ElementType.INTEGER) {
                listContent = listContent + " ";
            }
        }
        listContent = listContent + h.getValue();
        stack.push(new Element("("+listContent+")", ElementType.LIST));
    }

}