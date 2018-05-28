package at.ac.tuwien.ps;

import at.ac.tuwien.ps.element.RegisterElement;
import at.ac.tuwien.ps.operator.OperatorException;

public class Register {
	private RegisterElement[] register = new RegisterElement[32];
	
	public Register(){
		
	}
	
	public RegisterElement[] readRegister(){
		return this.register;
	}
	
	public RegisterElement readRegister(int n){
		if(n>=0 && n <= 31)
			return register[n];
		else
			throw new OperatorException("");
	}
	
	public void writeRegister(int n, RegisterElement element){
		register[n] = element;
	}
}
