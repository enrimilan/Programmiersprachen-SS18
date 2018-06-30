package at.ac.tuwien.ps.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.parsing.ParsingTools;

import java.util.ArrayList;
import java.util.List;
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

        List<Element> elements = new ArrayList<>();

        String nextContent = listContent;

        while(!nextContent.isEmpty()){
            if(nextContent.charAt(0) == ' ') {
                nextContent = nextContent.substring(1);
            }
            Element el = parsingTools.parseElement(nextContent);
            elements.add(el);
            nextContent = nextContent.substring(el.getValue().length());
        }

        Element head = elements.get(elements.size()-1);

        int headIndex = listContent.length() - head.getValue().length();
        String tailContent = listContent.substring(0, headIndex);
        Element tail = new Element("("+tailContent+")", ElementType.LIST);

        stack.push(tail);
        stack.push(head);

    }

}