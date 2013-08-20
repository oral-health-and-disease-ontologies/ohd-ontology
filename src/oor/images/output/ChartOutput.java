/**
 * Interface that defines any output generator for the chart
 */
package oor.images.output;

import java.io.IOException;

import oor.images.metadata.Field;

/**
 * @author nikhillo
 *
 */
public interface ChartOutput {
	public static final String __REV__ = "$Date : Rev$";
	
	public abstract void visitStart (Field fld);
	public abstract void visitEnd (Field fld);
	public abstract void writeToFile(String filename) throws IOException;
}
