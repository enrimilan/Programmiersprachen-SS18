package at.ac.tuwien.ps.element.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.register.NormalRegisterContent;
import at.ac.tuwien.ps.register.Register;
import at.ac.tuwien.ps.register.StreamingRegisterContent;
import at.ac.tuwien.ps.CommandStream;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class ReadRegisterOperatorTest {

    private ReadRegisterOperator readRegisterOperator = new ReadRegisterOperator();
    private Context context;

    @Before
    public void setUp() {
        ByteArrayInputStream in = new ByteArrayInputStream("1 1+\n".getBytes());
        System.setIn(in);

        // command stream
        CommandStream commandStream = new CommandStream("");

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

        this.context = new Context(commandStream, stack, registers);
    }

    @Test
    public void readRegisterTest1() {
        Stack<Element> stack = context.getDataStack();
        List<Register> registers = context.getRegisters();
        Register register0 = registers.get(0);
        register0.writeElement(new Element("(1 1+)", ElementType.LIST));
        stack.push(new Element("0", ElementType.INTEGER));
        readRegisterOperator.execute(context);
        Assert.assertEquals(1, stack.size());
        Assert.assertEquals(new Element("(1 1+)", ElementType.LIST), stack.peek());
    }

    @Test
    public void readRegisterTest2() {
        Stack<Element> stack = context.getDataStack();
        stack.push(new Element("1", ElementType.INTEGER));
        readRegisterOperator.execute(context);
        Assert.assertEquals(1, stack.size());
        Assert.assertEquals(new Element("(1 1+)", ElementType.LIST), stack.peek());
    }

    @Test(expected = OperatorException.class)
    public void readRegisterNoValueTest() {
        Stack<Element> stack = context.getDataStack();
        stack.push(new Element("0", ElementType.INTEGER));
        readRegisterOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void readRegisterEmptyStackTest() {
        readRegisterOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void readRegisterStackElementNotIntegerTest() {
        Stack<Element> stack = context.getDataStack();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        readRegisterOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void readRegisterOutOfRangeTest() {
        Stack<Element> stack = context.getDataStack();
        stack.push(new Element("32", ElementType.INTEGER));
        readRegisterOperator.execute(context);
    }

    @After
    public void tearDown() {
        System.setIn(System.in);
    }
}
