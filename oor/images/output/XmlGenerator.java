/**
 * 
 */
package oor.images.output;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

import oor.images.metadata.Field;

/**
 * @author nikhillo
 *
 */
public class XmlGenerator implements ChartOutput {
	private StringBuilder bldr;
	
	public XmlGenerator() {
		bldr = new StringBuilder();
		bldr.append("<?xml version = '1.0' encoding='UTF-8' ?>");
		bldr.append("<chart>");
	}
	
	
	public void print() {
		bldr.append("</chart>");
		System.out.println(bldr.toString());
	}

	@Override
	public void visitStart(Field fld) {
		if (fld != null && (fld.getNumChildren() > 0 || fld.hasValue())) {
			bldr.append("<" + fld.toString() +">\n");
		}
		
	}

	@Override
	public void visitEnd(Field fld) {
		if (fld != null && (fld.getNumChildren() > 0 || fld.hasValue())) {
			bldr.append("</" + fld.getName() + ">\n");
		}
		
	}


	@Override
	public void writeToFile(String filename) throws IOException {
		bldr.append("</chart>");
		BufferedWriter writer = new BufferedWriter(new FileWriter(filename));
		writer.write(bldr.toString());
		writer.flush();
		writer.close();
		
	}

}
