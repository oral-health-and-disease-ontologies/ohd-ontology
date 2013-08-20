/**
 * Class that detects line segments above a given threshold
 */
package oor.images.detectors;

import georegression.struct.line.LineSegment2D_F32;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

import oor.images.geometry.ImageArea;
import oor.images.geometry.LineSegment;
import oor.images.geometry.Point;
import oor.images.geometry.Shape;
import oor.images.metadata.Field;
import oor.images.metadata.Field.FieldTypes;
import boofcv.abst.feature.detect.line.DetectLineSegmentsGridRansac;
import boofcv.core.image.ConvertBufferedImage;
import boofcv.factory.feature.detect.line.FactoryDetectLineAlgs;
import boofcv.struct.image.ImageFloat32;

/**
 * @author nikhillo
 * 
 */
public class LineSegmentDetector implements OptionDetector {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date$ : $Rev$";
	
	private final BufferedImage img;
	private final DetectLineSegmentsGridRansac<ImageFloat32, ImageFloat32> detector;
	
	/**
	 * Default constructor
	 * @param image : The image that is being processed
	 */
	public LineSegmentDetector(BufferedImage image) {
		img = image;
		detector = FactoryDetectLineAlgs.lineRansac(140, 140, 2.36, true,
				ImageFloat32.class, ImageFloat32.class);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see oor.images.detectors.OptionDetector#visit(oor.images.metadata.Field)
	 */
	@Override
	public List<Shape> visit(Field field) {
		ImageArea ia = field.getPosition();
		int xoffset = ia.getX(), yoffset = ia.getY();
		BufferedImage image = ia.getSubImage(img);
		ImageFloat32 fltImg = ConvertBufferedImage.convertFromSingle(image, null,
				ImageFloat32.class);
		List<LineSegment2D_F32> found = detector.detect(fltImg);
		
		if (found != null) {
			//LineImageOps.mergeSimilar(found, (float) (2 * Math.PI * 0.1), 50f);
			ArrayList<Shape> retList = new ArrayList<Shape>(found.size());
			LineSegment temp;
			for (LineSegment2D_F32 segment : found) {
				temp = new LineSegment(new Point(segment.a.x + xoffset, segment.a.y + yoffset), new Point(segment.b.x + xoffset, segment.b.y + yoffset));
				retList.add(temp);
			}
			
			return retList;
		}
		
		return null;

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see oor.images.detectors.OptionDetector#isSupported(oor.images.metadata.Field.FieldTypes)
	 */
	@Override
	public boolean isSupported(FieldTypes type) {
		return type.equals(FieldTypes.FLEX);
	}

	

}
