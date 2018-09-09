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

public class DivisionOperatorTest {

    private DivisionOperator divisionOperator = new DivisionOperator();

    @Test
    public void divisionTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        stack.push(new Element("0", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divisionOperator.execute(context);
        Assert.assertEquals(new Element("0", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(1, stack.size());
    }

    @Test
    public void divisionTest2() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("2", ElementType.INTEGER));
        stack.push(new Element("2", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divisionOperator.execute(context);
        Assert.assertEquals(new Element("1", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(1, stack.size());
    }

    @Test
    public void divisionTest3() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("2", ElementType.INTEGER));
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divisionOperator.execute(context);
        Assert.assertEquals(new Element("0", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(1, stack.size());
    }

    @Test
    public void divisionTest4() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("2", ElementType.INTEGER));
        stack.push(new Element("5", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divisionOperator.execute(context);
        Assert.assertEquals(new Element("2", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(1, stack.size());
    }

    @Test
    public void divisionTest5() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("2", ElementType.INTEGER));
        stack.push(new Element("-5", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divisionOperator.execute(context);
        Assert.assertEquals(new Element("-2", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(1, stack.size());
    }

    @Test(expected = OperatorException.class)
    public void divisionByZeroTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("0", ElementType.INTEGER));
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divisionOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void divisionEmptyStackTest() {
        Context context = new Context(new CommandStream(""), new Stack<Element>(), new ArrayList<Register>());
        divisionOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void divisionStackWithOneElementTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divisionOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void divisionStackElementsNotIntegersTest1() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        stack.push(new Element("(1 1+)", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divisionOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void divisionStackElementsNotIntegersTest2() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divisionOperator.execute(context);
    }

}
