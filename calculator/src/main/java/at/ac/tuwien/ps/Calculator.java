package at.ac.tuwien.ps;


import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.operator.Operator;
import at.ac.tuwien.ps.parsing.ParsingTools;
import at.ac.tuwien.ps.register.NormalRegisterContent;
import at.ac.tuwien.ps.register.Register;
import at.ac.tuwien.ps.register.StreamingRegisterContent;
import at.ac.tuwien.ps.stream.CommandStream;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class Calculator {

    private Context context;

    private void initialize() {

        // command stream
        CommandStream commandStream = new CommandStream("0ra");

        // data stack
        Stack<Element> stack = new Stack<>();

        // registers
        List<Register> registers = new ArrayList<>(32);
        for(int i=0; i<32; i++) {
            if(i==1) {
                registers.add(new Register(new StreamingRegisterContent()));
            }
            else {
                registers.add(new Register(new NormalRegisterContent()));
            }
        }
        Register register0 = registers.get(0);
        //String program = "(((Please enter your program:)1w1ra1d1ca)1ca)";
        register0.writeElement(new Element(Programs.RUN_PROGRAM, ElementType.LIST));
        this.context = new Context(commandStream, stack, registers);
    }

    private void run(boolean verbose) {
        ParsingTools parsingTools = new ParsingTools();
        CommandStream commandStream = context.getCommandStream();
        Stack<Element> stack = context.getDataStack();

        if(verbose) System.out.println("--> " + stack + " ^ " + commandStream.getContent());
        Element element;
        while(commandStream.hasNext()) {
            element = commandStream.readNext();
            if(element.getElementType() == ElementType.OPERATOR) {
                Operator operator = parsingTools.match(element.getValue());
                operator.execute(context);
            }
            else {
                stack.push(element);
            }
            if (verbose) System.out.println("--> " + stack + " ^ " + commandStream.getContent());
        }
    }

    public static void main( String[] args ) {
        Calculator calculator = new Calculator();
        calculator.initialize();
        calculator.run(false);
    }

}
