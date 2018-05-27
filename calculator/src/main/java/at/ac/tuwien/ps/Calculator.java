package at.ac.tuwien.ps;


import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.operator.Operator;
import at.ac.tuwien.ps.parsing.ParsingTools;
import at.ac.tuwien.ps.stream.CommandStream;

import java.util.Stack;

public class Calculator
{
    public static void main( String[] args ) {

        example1();
        example2();

    }

    private static void example1() {
        System.out.println("A simple example: ");
        ParsingTools parsingTools = new ParsingTools();

        Context context = new Context(new CommandStream("(9~)(8)(3c4d1+da)a"));
        Stack<Element> stack = context.getDataStack();
        CommandStream commandStream = context.getCommandStream();
        stack.push(new Element("0", ElementType.INTEGER));

        Element element = null;
        while(commandStream.hasNext()) {
            element = commandStream.readNext();
            if(element.getElementType() == ElementType.OPERATOR) {
                Operator operator = parsingTools.match(element.getValue());
                operator.execute(context);
            }
            else {
                stack.push(element);
            }
            System.out.println("--> " + stack + " ^ " + commandStream.getContent());
        }

    }

    private static void example2() {
        System.out.println("\n\n\nA more complex example: ");

        ParsingTools parsingTools = new ParsingTools();

        Context context = new Context(new CommandStream("(2c1 3c-1c1=3c()(3c4d1+da)a2d*)2c3d2ca2d"));
        Stack<Element> stack = context.getDataStack();
        CommandStream commandStream = context.getCommandStream();
        stack.push(new Element("3", ElementType.INTEGER));

        Element element = null;
        while(commandStream.hasNext()) {
            element = commandStream.readNext();
            if(element.getElementType() == ElementType.OPERATOR) {
                Operator operator = parsingTools.match(element.getValue());
                operator.execute(context);
            }
            else {
                stack.push(element);
            }
            String output = ("--> " + stack + " ^ " + commandStream.getContent())
                    .replace("(3c4d1+da)", "C")
                    .replace("(2c1 3c-1c1=3c()Ca2d*)", "A");
            System.out.println(output);
        }
    }
}
