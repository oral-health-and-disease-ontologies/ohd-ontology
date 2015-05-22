/**
 * Class that represents the current chart being processed
 */
package oor.images.metadata;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Stack;

import oor.images.detectors.OptionDetector;
import oor.images.geometry.ImageArea;
import oor.images.geometry.Shape;
import oor.images.metadata.Field.FieldTypes;
import oor.images.metadata.fields.FieldFactory;
import oor.images.output.ChartOutput;
import oor.util.BufferedFileReader;

/**
 * @author nikhillo
 *
 */
public class ChartMetadata {
	@SuppressWarnings("unused")
	private static final String __REV__  ="$Rev$ : $Date$";
	
	private static ChartMetadata chart;
	
	private Collection<Field> topFields;
	
	/*
	 * Default private constructor
	 */
	private ChartMetadata() {
		topFields = new ArrayList<Field>();
	}
	
	/**
	 * Static method to load the metadata given a resource location
	 * @param resourceLocation The fully qualified path from where to load the
	 * definition
	 * @return The loaded metadata as a ChartMetadata object
	 */
	public static ChartMetadata load (String resourceLocation) {
		if (chart == null) {
			chart = new ChartMetadata();
			BufferedFileReader flrdr = new BufferedFileReader(resourceLocation, "[\t]");
			String[] fields;
			Field currField = null, prevField = null, temp = null;
			int prevLvl = -1, currLvl = -1;
			Stack<Field> tempStack = new Stack<Field>();
			while ((fields = flrdr.read()) != null) {
				prevField = currField;
				prevLvl = currLvl;
				
				currField = convert(fields);
				currLvl = currField.getLevel();
				
				if (prevField != null) {
					if (prevLvl == currLvl) { //siblings
						if (currLvl == 1) {
							chart.topFields.add(prevField); //goes to top level
						} else {
							tempStack.peek().add(prevField); //else to current top of stack
						}
					} else if (prevLvl < currLvl) { //this is a child of prev
						tempStack.push(prevField);
					} else { //we finished all children of whoever's on top of stack
						temp = tempStack.pop(); //one pop is needed
						temp.add(prevField);
						prevField = temp;
						
						while (true) {
							if (prevField.getLevel() == currLvl) {
								if (currLvl == 1) {
									chart.topFields.add(prevField);
								} else {
									tempStack.peek().add(prevField);
								}
								break;
							} else {
								temp = tempStack.pop();
								temp.add(prevField);
								prevField = temp;
							}
						}
					}
				}
			}
			
			while (!tempStack.isEmpty()) {
				temp = tempStack.pop();
				temp.add(currField);
				currField = temp;
			}
			
			temp = currField;
			chart.topFields.add(temp);
		}
		return chart;
	}
	
	/**
	 * Method to trigger traversal of a given chart metadata using the given
	 * ChartOutput interface
	 * @param output The current chart output object
	 */
	public void accept (ChartOutput output) {
		if (output != null && topFields != null) {
			for (Field f : topFields) {
				f.accept(output);
			}
		}
	}
	
	
	/**
	 * Method to trigger the traversal of a given chart metadata using the 
	 * given output detector
	 * @param detector The current detector
	 */
	public void accept (OptionDetector detector) {
		if (topFields != null &&  detector != null) {
			for (Field f : topFields) {
				f.accept(detector);
			}
		}
	}
	
	public void clearValues() {
		if (topFields != null) {
			for (Field f : topFields) {
				f.clearValues();
			}
		}
	}
	
	public ArrayList<Field> find(String fldType) {
		final ArrayList<Field> retlist = new ArrayList<Field>();
		final FieldTypes ft = Field.convert(fldType);
		this.accept(new OptionDetector() {
			
			@Override
			public List<Shape> visit(Field field) {
				retlist.add(field);
				return null;
			}
			
			@Override
			public boolean isSupported(FieldTypes type) {
				return (ft.equals(type));
			}
		});
		return retlist;
	}
	
	/**
	 * Method to convert a given array of strings into the corresponding Field
	 * @param flds An array of Strings as loaded from the resource file
	 * @return The corresponding Field instance
	 */
	private static Field convert(String[] flds) {
		String name = flds[0];
		ImageArea area = new ImageArea(Float.parseFloat(flds[1]),
				Float.parseFloat(flds[2]), Float.parseFloat(flds[3]), Float.parseFloat(flds[4]));
		FieldTypes type = Field.convert(flds[5]);
		int lvl = Integer.valueOf(flds[6]);
		return FieldFactory.initField(type, name, area, lvl);
	}
	
}
