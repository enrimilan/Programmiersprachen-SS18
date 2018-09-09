package at.ac.tuwien.ps.stream;

import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.parsing.ParsingTools;

import java.util.List;

public class CommandStream {

    private String content;
    private ParsingTools parsingTools = new ParsingTools();

    public CommandStream(String content) {
        this.content = content;
    }

    /** Consumes the next element from the command stream
     *  element = Integer | List | Operation
     *  @return the next element
     */
    public Element readNext() {

        if(content.startsWith(" ")) {
            content = content.substring(1);
        }

        Element element = parsingTools.parseElement(content);
        content = content.substring(element.getValue().length());
        return element;
    }

    /**
     * Prepend the contents of a list to the current command stream
     * @param element the list
     */
    public void prependList(Element element) {
        String listContent = element.getListContent();
        List<Element> elements = parsingTools.parseElements(listContent);
        if(!elements.isEmpty() && elements.get(elements.size() - 1).getElementType() == ElementType.INTEGER) {
            listContent = listContent + " ";
        }
        content = listContent  + content;
    }

    /**
     * Append the contents of a list to the current command stream
     * @param element the list
     */
    public void appendList(Element element) {
        String listContent = element.getListContent();
        List<Element> elements = parsingTools.parseElements(content);
        if(!elements.isEmpty() && elements.get(elements.size() - 1).getElementType() != ElementType.LIST) {
            listContent = " " + listContent;
        }
        content = content + listContent;
    }

    public boolean hasNext() {
        return !content.isEmpty();
    }

    public String getContent() {
        return content;
    }

    public void clear() {
        content = "";
    }
}
