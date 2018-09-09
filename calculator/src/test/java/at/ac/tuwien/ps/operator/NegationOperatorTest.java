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

public class NegationOperatorTest {

    private NegationOperator negationOperator = new NegationOperator();

    @Test
    public void negationTest1() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        negationOperator.execute(context);
        Assert.assertEquals(new Element("-1", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(1, stack.size());
    }

    @Test
    public void negationTest2() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("-1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        negationOperator.execute(context);
        Assert.assertEquals(new Element("1", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(1, stack.size());
    }

    @Test
    public void negationTest3() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("0", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        negationOperator.execute(context);
        Assert.assertEquals(new Element("0", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(1, stack.size());
    }

    @Test(expected = OperatorException.class)
    public void negationEmptyStackTest() {
        Context context = new Context(new CommandStream(""), new Stack<Element>(), new ArrayList<Register>());
        negationOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void negationStackElementNotIntegerTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        negationOperator.execute(context);
    }

}
