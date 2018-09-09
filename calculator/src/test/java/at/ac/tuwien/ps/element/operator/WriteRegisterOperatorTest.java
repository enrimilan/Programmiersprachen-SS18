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

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class WriteRegisterOperatorTest {

    private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
    private WriteRegisterOperator writeRegisterOperator = new WriteRegisterOperator();
    private Context context;

    @Before
    public void setUp() {
        System.setOut(new PrintStream(outContent));

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
    public void writeRegisterTest1() {
        Stack<Element> stack = context.getDataStack();
        List<Register> registers = context.getRegisters();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        stack.push(new Element("0", ElementType.INTEGER));
        writeRegisterOperator.execute(context);
        Assert.assertEquals(0, stack.size());
        Assert.assertEquals(new Element("(1 1+)", ElementType.LIST), registers.get(0).readElement());
    }

    @Test
    public void writeRegisterTest2() {
        Stack<Element> stack = context.getDataStack();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        stack.push(new Element("1", ElementType.INTEGER));
        writeRegisterOperator.execute(context);
        Assert.assertEquals(0, stack.size());
        Assert.assertEquals("1 1+\n", outContent.toString());
    }

    @Test
    public void writeRegisterTest3() {
        Stack<Element> stack = context.getDataStack();
        stack.push(new Element("1337", ElementType.INTEGER));
        stack.push(new Element("1", ElementType.INTEGER));
        writeRegisterOperator.execute(context);
        Assert.assertEquals(0, stack.size());
        Assert.assertEquals("1337\n", outContent.toString());
    }

    @Test(expected = OperatorException.class)
    public void writeRegisterEmptyStackTest() {
        writeRegisterOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void writeRegisterStackElementNotIntegerTest() {
        Stack<Element> stack = context.getDataStack();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        stack.push(new Element("(1 1+)", ElementType.LIST));
        writeRegisterOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void writeRegisterOutOfRangeTest() {
        Stack<Element> stack = context.getDataStack();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        stack.push(new Element("32", ElementType.INTEGER));
        writeRegisterOperator.execute(context);
    }

    @After
    public void tearDown() {
        System.setOut(System.out);
    }
}
