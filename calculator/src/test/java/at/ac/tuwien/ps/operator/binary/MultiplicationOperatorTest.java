package at.ac.tuwien.ps.operator.binary;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.operator.OperatorException;
import at.ac.tuwien.ps.register.Register;
import at.ac.tuwien.ps.stream.CommandStream;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Stack;

public class MultiplicationOperatorTest {

    private MultiplicationOperator multiplicationOperator = new MultiplicationOperator();

    @Test
    public void multiplicationTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        multiplicationOperator.execute(context);
        Assert.assertEquals(new Element("1", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(1, stack.size());
    }

    @Test
    public void multiplicationNegativeNumbersTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("-1", ElementType.INTEGER));
        stack.push(new Element("-1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        multiplicationOperator.execute(context);
        Assert.assertEquals(new Element("1", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(1, stack.size());
    }

    @Test
    public void multiplicationWithZeroNumbersTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("-1", ElementType.INTEGER));
        stack.push(new Element("0", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        multiplicationOperator.execute(context);
        Assert.assertEquals(new Element("0", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(1, stack.size());
    }

    @Test(expected = OperatorException.class)
    public void multiplicationEmptyStackTest() {
        Context context = new Context(new CommandStream(""), new Stack<Element>(), new ArrayList<Register>());
        multiplicationOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void multiplicationStackWithOneElementTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        multiplicationOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void multiplicationStackElementsNotIntegersTest1() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        stack.push(new Element("(1 1+)", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        multiplicationOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void multiplicationStackElementsNotIntegersTest2() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        multiplicationOperator.execute(context);
    }
}
