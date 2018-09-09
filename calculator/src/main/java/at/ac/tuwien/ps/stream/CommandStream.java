package at.ac.tuwien.ps.stream;

import at.ac.tuwien.ps.element.Element;
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

    public void prependList(Element element) {
        String value = element.getValue();
        String listContent = value.substring(1, value.length() - 1);
        String[] elements = listContent.split(" ");
        if(elements[elements.length-1].matches("-?\\d+") && content.length()>0 && Character.isDigit(content.charAt(0))) {
            listContent = listContent + " ";
        }
        content = listContent  + content;
    }

    public void appendList(Element element) {
        String value = element.getValue();
        String listContent = value.substring(1, value.length() - 1);
        if(listContent.matches("-?\\d+") && content.length()>0 && Character.isDigit(content.charAt(0))) {
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
