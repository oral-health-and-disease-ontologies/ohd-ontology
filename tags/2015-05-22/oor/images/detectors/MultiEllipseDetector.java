/**
 * Class that is used to detect ellipses in a given image segment. It differs
 * from its parent in the sense that it tracks multiple ellipses as against
 * the parent that tracks a single ellipse
 */
package oor.images.detectors;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

import oor.images.geometry.Shape;
import oor.images.metadata.Field;
import oor.images.metadata.Field.FieldTypes;

/**
 * @author nikhillo
 *
 */
public class MultiEllipseDetector extends EllipseDetector implements OptionDetector{
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date : Rev$";
	
	/**
	 * Default constructor
	 * @param img : The iamge being processed
	 */
	public MultiEllipseDetector(BufferedImage img) {
		super(img);
	}
	
	/*
	 * (non-Javadoc)
	 * @see oor.images.detectors.EllipseDetector#convert(oor.images.metadata.Field, int[])
	 */
	@Override
	protected List<Shape> convert(Field f, int[] processed) {
		int numSegs = processed.length;
		List<Shape> retColl = new ArrayList<Shape>(numSegs);
		
		boolean found = false;
		for (int i = 0; i < numSegs; i++) {
			if (processed[i] > 0) {
				found = true;
				retColl.add(f.getChild(i).getPosition());
			} else {
				retColl.add(null);
			}
		}
		
		return found ? retColl : null;
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see oor.images.detectors.OptionDetector#isSupported(oor.images.metadata.Field.FieldTypes)
	 */
	@Override
	public boolean isSupported(FieldTypes type) {
		return (type.equals(FieldTypes.MULTI_SELECT));
	}
}
