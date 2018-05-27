package at.ac.tuwien.ps.stream;

import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.parsing.ParsingTools;

public class CommandStream {

    private String content;
    private ParsingTools parsingTools = new ParsingTools();

    public CommandStream(String content) {
        this.content = content;
    }

    /** Reads the next element from the command stream
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

    public void write(Element element) {
        if(element.getElementType() == ElementType.LIST) {
            String value = element.getValue();
            content = value.substring(1, value.length() - 1) + content;
        }
    }

    public boolean hasNext() {
        return !content.isEmpty();
    }

    public String getContent() {
        return content;
    }
}
