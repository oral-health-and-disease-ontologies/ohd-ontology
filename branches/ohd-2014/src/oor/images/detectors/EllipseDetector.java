/**
 * Class that is used to detect ellipses based on their inherent symmetry
 */
package oor.images.detectors;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import oor.images.geometry.ImageArea;
import oor.images.geometry.Point;
import oor.images.geometry.Shape;
import oor.images.metadata.Field;
import oor.images.metadata.Field.FieldTypes;

/**
 * @author nikhillo
 *
 */
public class EllipseDetector implements OptionDetector {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date$ : $Rev$";
	
	private final BufferedImage image;
	
	/**
	 * Default constructor
	 * @param img : Image on which the detector is to be run
	 */
	public EllipseDetector(BufferedImage img) {
		this.image = img;
	}
	

	/* (non-Javadoc)
	 * @see oor.images.detectors.OptionDetector#visit(oor.images.metadata.Field)
	 */
	@Override
	public List<Shape> visit(Field field) {
		ImageArea segmentArea = field.getPosition();
		BufferedImage img = segmentArea.getSubImage(image);
		int numSegments = field.getNumChildren();
		int[] processed = process(img, numSegments == 0 ? 1 : numSegments);
		return this.convert(field, processed);
	}


	/**
	 * Method to convert the array of ints representing the number of found pixels
	 * per segment into a list of shapes.
	 * @param f  :The current field on which the detection is being run
	 * @param processed : The array of ints representing the processed segment pixel values
	 * @return A list containing a Shape instance (specifically ImageArea) at a given index
	 * if the child at that index is selected, or null otherwise.
	 */
	protected List<Shape> convert(Field f, int[] processed) {
		int numSegs = processed.length;
		List<Shape> retColl = null;
		
		int currMax = -1, currIdx = -1;
		for (int i = 0; i < numSegs; i++) {
			if (processed[i] > currMax) {
				currMax = processed[i];
				currIdx = i;
			}
		}
		
		Shape temp;
		if (currMax > 0 && currIdx > -1) {
			retColl = new ArrayList<Shape>(numSegs);
			for (int i = 0; i < numSegs; i++) {
				if (i == currIdx) {
					temp = f.hasChildren() ? f.getChild(i).getPosition() : f.getPosition();
				} else
					temp = null;
				retColl.add(temp);
			}
		}
		
		return retColl;
	}

	/**
	 * Method to count the number of eligible pixels per segment
	 * @param img : The image on which the processing is to be run
	 * @param numChildren : The number of segments being processed
	 * @return : AN array containing the number of eligible pixels
	 * found per segment iff they are more than a threshold. The length
	 * of the array is always equal to numChildren
	 */
	private int[] process(BufferedImage img, int numChildren) {
		/*
		 * Pseudo-code:
		 * 	1. Create a local hash of points
		 *  2. One by one, sample across each axis
		 *  	On a slice, get two points
		 *  	Validate if those points can determine an ellipse
		 *  	If yes, add to local hash
		 *  3. If local hash has more than a thresholded points
		 *  	Add all to global hash
		 *  4. Return array
		 */
		
		int segHeight = img.getHeight();
		int segWidth = img.getWidth() / numChildren;
		
		int[] retarray = new int[numChildren];
		for (int i = 0; i < numChildren; i++) {
			retarray[i] = processSegment(img, i * segWidth, (i+1) * segWidth, 0, segHeight);
		}
		return retarray;
	}

	/**
	 * Method to process an individual segment and count the nuber of eligible pixels
	 * @param img : The image being processed
	 * @param xstart : The x co-ordinate of the upper left corner
	 * @param xend : The x co-ordinate of the lower right corner
	 * @param ystart : The y co-ordinate of the upper left corner
	 * @param yend : The y co-ordinate of the lower right corner
	 * @return The number of pixels found iff they are more than a threshold, -1 otherwise
	 */
	private int processSegment(BufferedImage img, int xstart, int xend, int ystart,
			int yend) {
		HashSet<Point> segPoints = new HashSet<Point>();
		
		int numSlices = 20;
		int width = xend - xstart;
		int height = yend - ystart;
		
		Point p1 =null, p2 = null, temp;
		//Sample across x axis
		for (int x = xstart; x < xend; x+= width/numSlices) {
			p1 = null;
			p2 = null;
			for (int y = ystart; y < yend; y++) {
				if (isPixelSelected(img, x, y)) {
					temp = new Point(x, y);
					if (p1 == null) {
						p1 = temp;
					} else if (p2 == null || temp.greaterThan(p2)) {
						p2 = temp;
					}
				}
			}
			
			if (validatePoints(p1, p2) && p1.midPoint(p2).distance(new Point(x, ystart)) >= 10) {
				if (!segPoints.contains(p1))
					segPoints.add(p1);
				
				if (!segPoints.contains(p2))
					segPoints.add(p2);
			}
		}
		
		//Sample across y axis
		for (int y = ystart; y < yend; y+= height/numSlices) {
			p1 = null;
			p2 = null;
			for (int x = xstart; x < xend; x++) {
				if (isPixelSelected(img, x, y)) {
					temp = new Point(x, y);
					if (p1 == null) {
						p1 = temp;
					} else if (p2 == null || temp.greaterThan(p2)) {
						p2 = temp;
					}
				}
			}
			
			if (validatePoints(p1, p2) && p1.midPoint(p2).distance(new Point(xstart, y)) >= 10) {
				if (!segPoints.contains(p1))
					segPoints.add(p1);
				
				if (!segPoints.contains(p2))
					segPoints.add(p2);
			}
		}
		
		int size = segPoints.size();
		return (size >= 40) ? size : -1;
	}
	
	/**
	 * Utility method to determine if a given pixel has been selected
	 * @param img : The image being processed
	 * @param x : The x coordinate of the pixel
	 * @param y : The y coordinate of the pixel
	 * @return : True if the pixel is selected, false otherwise. Selected here refers to
	 * its rgb value being equal to 0xFFFFFF
	 */
	private boolean isPixelSelected(BufferedImage img, int x, int y) {
		int rgb = img.getRGB(x, y);
		rgb = 0x00FFFFFF & rgb;
		
		return (rgb == 0);
	}
	
	/**
	 * Utility method to validate if the two given points could lie on the
	 * edge of an ellipse
	 * @param p1 : The first point
	 * @param p2 : The second point
	 * @return : True, if the points could lie on the edge, false othrwise
	 */
	private boolean validatePoints (Point p1, Point p2) {
		return (p1 != null && p2 != null && p1.distance(p2) >= 10);
	}

	
	/*
	 * (non-Javadoc)
	 * 
	 * @see oor.images.detectors.OptionDetector#isSupported(oor.images.metadata.Field.FieldTypes)
	 */
	@Override
	public boolean isSupported(FieldTypes type) {
		return (type.equals(FieldTypes.SINGLE_SELECT) || type.equals(FieldTypes.SINGLE_OPTION));
	}

}
