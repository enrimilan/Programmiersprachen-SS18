package at.ac.tuwien.ps.element;

import at.ac.tuwien.ps.element.operator.*;
import at.ac.tuwien.ps.element.operator.binary.*;

import java.util.ArrayList;
import java.util.List;

public class ParsingTools {

    public Operator match(String sign) {

        switch (sign) {
            case "+": return new AdditionOperator();
            case "-": return new SubtractionOperator();
            case "*": return new MultiplicationOperator();
            case "/": return new DivisionOperator();
            case "%": return new ModuloOperator();
            case "&": return new LogicalAndOperator();
            case "|": return new LogicalOrOperator();
            case "=": return new EqualsOperator();
            case "<": return new LessThanOperator();
            case ">": return new GreaterThanOperator();
            case "~": return new NegationOperator();
            case "c": return new CopyOperator();
            case "d": return new DeleteOperator();
            case "a": return new ApplyImmediatelyOperator();
            case "z": return new ApplyLaterOperator();
            case "r": return new ReadRegisterOperator();
            case "w": return new WriteRegisterOperator();
            case "i": return new IntegerCheckOperator();
            case "l": return new NonEmptyListCheckOperator();
            case "s": return new StackSizeOperator();
            case ":": return new CombineOperator();
            case "!": return new DivideListOperator();
            case "x": return new ExitOperator();
        }

        throw new OperatorException("Unknown operator: "  + sign);
    }

    public Element parseElement(String content) {
        String nextContent = content;

        if(nextContent.charAt(0) == ' ') {
            // empty space means the next element will be a number
            nextContent = nextContent.substring(1);
            StringBuilder integer = new StringBuilder();
            if(nextContent.charAt(0) == '-') {
                integer.append("-");
                nextContent = nextContent.substring(1);
            }
            int i = 0;
            while(i<nextContent.length() && Character.isDigit(nextContent.charAt(i))) {
                integer.append(nextContent.charAt(i));
                i++;
            }
            return new Element(integer.toString(), ElementType.INTEGER);
        }

        // try to parse a list
        if(content.startsWith("(")) {
            int depth = -1;
            for(int i=0; i<content.length(); i++) {
                if(content.charAt(i) == '(') {
                    depth++;
                }
                if(content.charAt(i) == ')' && depth == 0) {
                    String list = content.substring(0, i+1);
                    return new Element(list, ElementType.LIST);
                }
                if(content.charAt(i) == ')') {
                    depth--;
                }
            }
        }

        // try to parse a negative number
        if(nextContent.length() >=2 && nextContent.charAt(0) == '-' && Character.isDigit(nextContent.charAt(1))) {
            StringBuilder integer = new StringBuilder();
            integer.append("-");
            nextContent = nextContent.substring(1);
            int i = 0;
            while(i<nextContent.length() && Character.isDigit(nextContent.charAt(i))) {
                integer.append(nextContent.charAt(i));
                i++;
            }
            return new Element(integer.toString(), ElementType.INTEGER);
        }

        // try to parse a positive number
        if(Character.isDigit(content.charAt(0))) {
            int i = 0;
            StringBuilder integer = new StringBuilder();
            while(i<content.length() && Character.isDigit(content.charAt(i))) {
                integer.append(content.charAt(i));
                i++;
            }
            return new Element(integer.toString(), ElementType.INTEGER);
        }

        // last possibility: operator
        String operation = String.valueOf(content.charAt(0));
        return new Element(operation, ElementType.OPERATOR);
    }

    public List<Element> parseElements(String content) {
        String nextContent = content;
        List<Element> elements = new ArrayList<>();
        while(!nextContent.isEmpty()){
            int consumedLength = 0;
            if(nextContent.charAt(0) == ' ') {
                consumedLength++;
            }
            Element el = parseElement(nextContent);
            consumedLength = consumedLength + el.getValue().length();
            elements.add(el);
            nextContent = nextContent.substring(consumedLength);
        }
        return elements;
    }
}