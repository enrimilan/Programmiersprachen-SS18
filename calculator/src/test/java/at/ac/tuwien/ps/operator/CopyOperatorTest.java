package at.ac.tuwien.ps.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.register.Register;
import at.ac.tuwien.ps.stream.CommandStream;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Stack;

public class CopyOperatorTest {

    private CopyOperator copyOperator = new CopyOperator();

    @Test
    public void copyTest1() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("2", ElementType.INTEGER));
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        copyOperator.execute(context);
        Assert.assertEquals(new Element("2", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(2, stack.size());
    }

    @Test
    public void copyTest2() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        copyOperator.execute(context);
        Assert.assertEquals(new Element("(1 1+)", ElementType.LIST), stack.peek());
        Assert.assertEquals(2, stack.size());
    }

    @Test(expected = OperatorException.class)
    public void copyEmptyStackTest() {
        Context context = new Context(new CommandStream(""), new Stack<Element>(), new ArrayList<Register>());
        copyOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void copyStackOnlyOneElementTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        copyOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void copyStackElementNotIntegerTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        stack.push(new Element("(1 1+)", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        copyOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void copyNegativeTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("2", ElementType.INTEGER));
        stack.push(new Element("-1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        copyOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void copyZeroTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("2", ElementType.INTEGER));
        stack.push(new Element("0", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        copyOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void copyOutOfRangeTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("2", ElementType.INTEGER));
        stack.push(new Element("2", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        copyOperator.execute(context);
    }

}
