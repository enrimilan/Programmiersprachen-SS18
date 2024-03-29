package at.ac.tuwien.ps;

import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.element.ParsingTools;

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
        int consumedLength = 0;
        if(content.charAt(0) == ' ') {
            consumedLength++;
        }
        Element element = parsingTools.parseElement(content);
        consumedLength = consumedLength + element.getValue().length();
        content = content.substring(consumedLength);
        return element;
    }

    /**
     * Prepend the contents of a list to the current command stream
     * @param element the list
     */
    public void prependList(Element element) {
        String listContent = element.getListContent();
        List<Element> commandStreamContentElements = parsingTools.parseElements(content);
        List<Element> listContentElements = parsingTools.parseElements(listContent);

        // add a white space if the first element of the stream and the last of the list are integers
        if(!listContent.isEmpty()
                && !commandStreamContentElements.isEmpty()
                && !listContentElements.isEmpty()
                && commandStreamContentElements.get(0).getElementType() == ElementType.INTEGER
                && listContentElements.get(listContentElements.size() - 1).getElementType() == ElementType.INTEGER) {
            listContent =  listContent + " ";
        }
        content = listContent  + content;
    }

    /**
     * Append the contents of a list to the current command stream
     * @param element the list
     */
    public void appendList(Element element) {
        String listContent = element.getListContent();
        List<Element> commandStreamContentElements = parsingTools.parseElements(content);
        List<Element> listContentElements = parsingTools.parseElements(listContent);

        // add a white space if the last element of the stream and the first of the list are integers
        if(!listContent.isEmpty()
                && !commandStreamContentElements.isEmpty()
                && !listContentElements.isEmpty()
                && commandStreamContentElements.get(commandStreamContentElements.size() - 1).getElementType() == ElementType.INTEGER
                && listContentElements.get(0).getElementType() == ElementType.INTEGER) {
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
