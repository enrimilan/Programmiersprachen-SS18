package at.ac.tuwien.ps;

import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import org.junit.Assert;
import org.junit.Test;

public class CommandStreamTest {

    @Test
    public void appendListTest1() {
        CommandStream commandStream = new CommandStream("1 2 3");
        commandStream.appendList(new Element("()", ElementType.LIST));
        Assert.assertEquals("1 2 3", commandStream.getContent());
    }

    @Test
    public void appendListTest2() {
        CommandStream commandStream = new CommandStream("1 2 3");
        commandStream.appendList(new Element("(4)", ElementType.LIST));
        Assert.assertEquals("1 2 3 4", commandStream.getContent());
    }

    @Test
    public void appendListTest3() {
        CommandStream commandStream = new CommandStream("1 2 3");
        commandStream.appendList(new Element("((4))", ElementType.LIST));
        Assert.assertEquals("1 2 3(4)", commandStream.getContent());
    }

    @Test
    public void appendListTest4() {
        CommandStream commandStream = new CommandStream("(1 2 3)");
        commandStream.appendList(new Element("((4))", ElementType.LIST));
        Assert.assertEquals("(1 2 3)(4)", commandStream.getContent());
    }

    @Test
    public void appendListTest5() {
        CommandStream commandStream = new CommandStream("(1 2 3)");
        commandStream.appendList(new Element("(4)", ElementType.LIST));
        Assert.assertEquals("(1 2 3)4", commandStream.getContent());
    }

    @Test
    public void appendListTest6() {
        CommandStream commandStream = new CommandStream("(1 2 3)");
        commandStream.appendList(new Element("(a)", ElementType.LIST));
        Assert.assertEquals("(1 2 3)a", commandStream.getContent());
    }


    @Test
    public void prependListTest1() {
        CommandStream commandStream = new CommandStream("1 2 3");
        commandStream.prependList(new Element("()", ElementType.LIST));
        Assert.assertEquals("1 2 3", commandStream.getContent());
    }

    @Test
    public void prependListTest2() {
        CommandStream commandStream = new CommandStream("1 2 3");
        commandStream.prependList(new Element("(4)", ElementType.LIST));
        Assert.assertEquals("4 1 2 3", commandStream.getContent());
    }

    @Test
    public void prependListTest3() {
        CommandStream commandStream = new CommandStream("1 2 3");
        commandStream.prependList(new Element("((4))", ElementType.LIST));
        Assert.assertEquals("(4)1 2 3", commandStream.getContent());
    }

    @Test
    public void prependListTest4() {
        CommandStream commandStream = new CommandStream("(1 2 3)");
        commandStream.prependList(new Element("((4))", ElementType.LIST));
        Assert.assertEquals("(4)(1 2 3)", commandStream.getContent());
    }

    @Test
    public void prependListTest5() {
        CommandStream commandStream = new CommandStream("(1 2 3)");
        commandStream.prependList(new Element("(4)", ElementType.LIST));
        Assert.assertEquals("4(1 2 3)", commandStream.getContent());
    }

    @Test
    public void prependListTest6() {
        CommandStream commandStream = new CommandStream("(1 2 3)");
        commandStream.prependList(new Element("(a)", ElementType.LIST));
        Assert.assertEquals("a(1 2 3)", commandStream.getContent());
    }
}
