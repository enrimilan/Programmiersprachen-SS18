package at.ac.tuwien.ps.element.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.register.Register;
import at.ac.tuwien.ps.CommandStream;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Stack;

public class DivideListOperatorTest {

    private DivideListOperator divideListOperator = new DivideListOperator();

    @Test
    public void divideListTest1() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divideListOperator.execute(context);
        Assert.assertEquals(2, stack.size());
        Assert.assertEquals(new Element("+", ElementType.OPERATOR), stack.get(1));
        Assert.assertEquals(new Element("(1 1)", ElementType.LIST), stack.get(0));
    }

    @Test
    public void divideListTest2() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(1)", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divideListOperator.execute(context);
        Assert.assertEquals(2, stack.size());
        Assert.assertEquals(new Element("1", ElementType.INTEGER), stack.get(1));
        Assert.assertEquals(new Element("()", ElementType.LIST), stack.get(0));
    }

    @Test
    public void divideListTest3() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(1+)", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divideListOperator.execute(context);
        Assert.assertEquals(2, stack.size());
        Assert.assertEquals(new Element("+", ElementType.OPERATOR), stack.get(1));
        Assert.assertEquals(new Element("(1)", ElementType.LIST), stack.get(0));
    }

    @Test
    public void divideListTest4() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("((1 1+)(2 2*))", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divideListOperator.execute(context);
        Assert.assertEquals(2, stack.size());
        Assert.assertEquals(new Element("(2 2*)", ElementType.LIST), stack.get(1));
        Assert.assertEquals(new Element("((1 1+))", ElementType.LIST), stack.get(0));
    }

    @Test
    public void divideListTest5() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(1 1+(2 2*))", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divideListOperator.execute(context);
        Assert.assertEquals(2, stack.size());
        Assert.assertEquals(new Element("(2 2*)", ElementType.LIST), stack.get(1));
        Assert.assertEquals(new Element("(1 1+)", ElementType.LIST), stack.get(0));
    }

    @Test(expected = OperatorException.class)
    public void divideListEmptyListTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("()", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divideListOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void divideListNoListTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        divideListOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void divideListEmptyStackTest() {
        Context context = new Context(new CommandStream(""), new Stack<Element>(), new ArrayList<Register>());
        divideListOperator.execute(context);
    }

}
