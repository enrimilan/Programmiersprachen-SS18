package at.ac.tuwien.ps;


import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.element.ParsingTools;
import at.ac.tuwien.ps.element.operator.Operator;
import at.ac.tuwien.ps.element.operator.OperatorException;
import at.ac.tuwien.ps.register.NormalRegisterContent;
import at.ac.tuwien.ps.register.Register;
import at.ac.tuwien.ps.register.StreamingRegisterContent;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class Calculator {

    private Context context;

    public void initialize(String program) {

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
        register0.writeElement(new Element(program, ElementType.LIST));
        this.context = new Context(commandStream, stack, registers);
    }

    public void run(boolean verbose) {
        ParsingTools parsingTools = new ParsingTools();
        CommandStream commandStream = context.getCommandStream();
        Stack<Element> stack = context.getDataStack();

        if(verbose) System.out.println("--> " + stack.toString().replaceAll("\n", "\\\\n") + " ^ " + commandStream.getContent().replaceAll("\n", "\\\\n"));
        Element element;

        try {
            while(commandStream.hasNext()) {
                element = commandStream.readNext();
                if(element.getElementType() == ElementType.OPERATOR) {
                    Operator operator = parsingTools.match(element.getValue());
                    operator.execute(context);
                }
                else {
                    stack.push(element);
                }
                if (verbose) System.out.println("--> " + stack.toString().replaceAll("\n", "\\\\n") + " ^ " + commandStream.getContent().replaceAll("\n", "\\\\n"));
            }
        }
        catch (OperatorException e) {
            System.out.println(e.getMessage());
        }

    }

    public static void main( String[] args ) {
        Calculator calculator = new Calculator();
        calculator.initialize(Programs.RUN_PROGRAM);
        calculator.run(false);
    }

}
