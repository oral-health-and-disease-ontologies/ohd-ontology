/**
 * Class that represent a line segment as denoted by two end points
 */
package oor.images.geometry;

/**
 * @author nikhillo
 * 
 */
public class LineSegment implements Shape {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date$ : $Rev$";

	protected final Point startPoint;
	protected final Point endPoint;
	private final float slope;
	private final float intercept;

	/**
	 * Default constructor
	 * 
	 * @param start : The start point of the line segment
	 * @param end : The end point of the line segment
	 */
	public LineSegment(Point start, Point end) {
		startPoint = start;
		endPoint = end;
		slope = (endPoint.yc - startPoint.yc) / (endPoint.xc - startPoint.xc);
		intercept = endPoint.yc - (slope * endPoint.xc);

	}

	/**
	 * Method to check if the given Point lies on the line segment
	 * 
	 * @param pt : The point to be checked
	 * @return true iff the Point lies on the line segment, false otherwise
	 */
	public boolean contains(Point pt) {
		return (containsExtrapolated(pt) && pt.xc >= startPoint.xc
				&& pt.xc <= endPoint.xc && pt.yc >= startPoint.yc && pt.yc <= endPoint.yc);
	}
	
	/**
	 * Method to check if the Point lies on the line as extrapolated by the given line segment
	 * @param pt : The Point to be checked
	 * @return true if the Point lies on the extrapolated line, false otherwise
	 */
	public boolean containsExtrapolated(Point pt) {
		return (pt.yc == (pt.xc * slope - intercept));
	}

	public double getLength() {
		return endPoint.distance(startPoint);
	}
	
	/**
	 * Method to get the start point of the line segment
	 * @return The Point that represent the start point
	 */
	public Point start() {
		return startPoint;
	}
	
	/**
	 * Method to get the end point of the given line segment
	 * @return The Point that represents the end point
	 */
	public Point end() {
		return endPoint;
	}
}
