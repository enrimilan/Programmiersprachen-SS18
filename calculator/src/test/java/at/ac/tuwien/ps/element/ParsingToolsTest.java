package at.ac.tuwien.ps.element;

import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.element.ParsingTools;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

public class ParsingToolsTest {

    private ParsingTools parsingTools = new ParsingTools();

    @Test
    public void testParseElementInteger() {
        Element actual = parsingTools.parseElement("1");
        Assert.assertEquals(ElementType.INTEGER, actual.getElementType());
        Assert.assertEquals("1", actual.getValue());
    }

    @Test
    public void testParseElementList() {
        Element actual = parsingTools.parseElement("(11 1)");
        Assert.assertEquals(ElementType.LIST, actual.getElementType());
        Assert.assertEquals("(11 1)", actual.getValue());
    }

    @Test
    public void testParseElementOperator() {
        Element actual = parsingTools.parseElement("a");
        Assert.assertEquals(ElementType.OPERATOR, actual.getElementType());
        Assert.assertEquals("a", actual.getValue());
    }

    @Test
    public void testParseElement1() {
        List<Element> parsedElements = parsingTools.parseElements("2c1 3c-1c1=3c()");

        Assert.assertEquals(ElementType.INTEGER, parsedElements.get(0).getElementType());
        Assert.assertEquals("2", parsedElements.get(0).getValue());

        Assert.assertEquals(ElementType.OPERATOR, parsedElements.get(1).getElementType());
        Assert.assertEquals("c", parsedElements.get(1).getValue());

        Assert.assertEquals(ElementType.INTEGER, parsedElements.get(2).getElementType());
        Assert.assertEquals("1", parsedElements.get(2).getValue());

        Assert.assertEquals(ElementType.INTEGER, parsedElements.get(3).getElementType());
        Assert.assertEquals("3", parsedElements.get(3).getValue());

        Assert.assertEquals(ElementType.OPERATOR, parsedElements.get(4).getElementType());
        Assert.assertEquals("c", parsedElements.get(4).getValue());

        Assert.assertEquals(ElementType.OPERATOR, parsedElements.get(5).getElementType());
        Assert.assertEquals("-", parsedElements.get(5).getValue());

        Assert.assertEquals(ElementType.INTEGER, parsedElements.get(6).getElementType());
        Assert.assertEquals("1", parsedElements.get(6).getValue());

        Assert.assertEquals(ElementType.OPERATOR, parsedElements.get(7).getElementType());
        Assert.assertEquals("c", parsedElements.get(7).getValue());

        Assert.assertEquals(ElementType.INTEGER, parsedElements.get(8).getElementType());
        Assert.assertEquals("1", parsedElements.get(8).getValue());

        Assert.assertEquals(ElementType.OPERATOR, parsedElements.get(9).getElementType());
        Assert.assertEquals("=", parsedElements.get(9).getValue());

        Assert.assertEquals(ElementType.INTEGER, parsedElements.get(10).getElementType());
        Assert.assertEquals("3", parsedElements.get(10).getValue());

        Assert.assertEquals(ElementType.OPERATOR, parsedElements.get(11).getElementType());
        Assert.assertEquals("c", parsedElements.get(11).getValue());

        Assert.assertEquals(ElementType.LIST, parsedElements.get(12).getElementType());
        Assert.assertEquals("()", parsedElements.get(12).getValue());

        Assert.assertEquals(13, parsedElements.size());
    }

    @Test
    public void testParseElement2() {
        List<Element> parsedElements = parsingTools.parseElements("");
        Assert.assertEquals(0, parsedElements.size());
    }

}

