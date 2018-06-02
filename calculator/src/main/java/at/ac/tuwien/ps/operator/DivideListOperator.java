package at.ac.tuwien.ps.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.parsing.ParsingTools;

import java.util.Stack;

/**
 * Takes an argument (which is a nonempty list) from the data stack and pushes first the tail of this list and then
 * the list head onto the data stack. An error is reported if the argument is not a nonempty list.
 */
public class DivideListOperator implements Operator {

    private ParsingTools parsingTools = new ParsingTools();

    @Override
    public void execute(Context context) {
        Stack<Element> stack = context.getDataStack();
        if(stack.size() < 1)
            throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> Stack needs to contain at least 1 element but has " + stack.size());

        Element element = stack.pop();
        if(element.getElementType() == ElementType.INTEGER)
            throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> The element is not a list");

        String list = element.getValue();
        if(list.length() == 2 && list.charAt(0) == '(' && list.charAt(1) == ')')
            throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> The element is an empty list");

        String listContent = list.substring(1, list.length() - 1);

        Element head = parsingTools.parseElement(listContent);

        listContent = listContent.substring(head.getValue().length());

        if(listContent.startsWith(" ")) {
            listContent = listContent.substring(1);
        }

        Element tail = new Element("("+listContent+")", ElementType.LIST);

        stack.push(tail);
        stack.push(head);

    }

}