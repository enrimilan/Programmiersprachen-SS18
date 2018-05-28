package at.ac.tuwien.ps.element;

public class RegisterElement {
	 private String value;
	 private RegisterElementType elementType;

	 public RegisterElement(String value, RegisterElementType elementType) {
        this.value = value;
        this.elementType = elementType;
    }
	 
	public RegisterElement(Element element){
		this.value = element.getValue();
		this.elementType = RegisterElementType.valueOf(element.getElementType().name());
	}

    public String getValue() {
        return value;
    }
    
    public int parseToInt(){
    	return Integer.parseInt(this.getValue());
    }

    public void setValue(String value) {
        this.value = value;
    }

    public RegisterElementType getElementType() {
        return elementType;
    }

    public void setElementType(RegisterElementType elementType) {
        this.elementType = elementType;
    }

    @Override
    public String toString() {
        return value;
    }
    
    public RegisterElement deepCopy(){
    	return new RegisterElement(this.getValue(),this.getElementType());
    }
    
}
