package at.ac.tuwien.ps.register;

import at.ac.tuwien.ps.Programs;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.parsing.ParsingTools;

import java.util.Scanner;

public class StreamingRegisterContent implements RegisterContent {

    private Scanner scanner = new Scanner(System.in);
    private ParsingTools parsingTools = new ParsingTools();

    @Override
    public Element read() {
        String line = scanner.nextLine();
        if(line.equals("conditional")) {
            return new Element(Programs.CONDITIONAL, ElementType.LIST);
        }
        if(line.equals("factorial")){
            return new Element(Programs.FACTORIAL, ElementType.LIST);
        }
        if(line.equals("prime")){
            return new Element(Programs.PRIME, ElementType.LIST);
        }
        return parsingTools.parseElement(line);
    }

    @Override
    public void write(Element element) {
        String output = element.getValue();
        if(element.getElementType() == ElementType.LIST) {
            output = output.substring(1, output.length() - 1);
        }
        System.out.println(output);
    }

}