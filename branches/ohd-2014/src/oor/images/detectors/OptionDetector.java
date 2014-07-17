/**
 * Interface that defines any detector that can detect a selected option on the chart
 */
package oor.images.detectors;

import java.util.List;

import oor.images.geometry.Shape;
import oor.images.metadata.Field;
import oor.images.metadata.Field.FieldTypes;

/**
 * @author nikhillo
 *
 */
public interface OptionDetector {
	public static final String __REV__ = "$Date : Rev$";
	
	/**
	 * Method that is used to visit a given field with the current detector
	 * @param field : The field being visited
	 * @return A list of Shapes detected by the detector within the Field if
	 * it finds anything, null otherwise
	 */
	public abstract List<Shape> visit (Field field);
	
	/**
	 * Method to define if the given detector supports the given field type
	 * A detector will be applied iff the field type is supported by the detector
	 * @param type : The current field type
	 * @return true iff the detector supports the given field type
	 */
	public abstract boolean isSupported(FieldTypes type);
}
