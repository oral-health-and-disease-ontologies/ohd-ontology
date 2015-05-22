/**
 * A class that represents a Point in two dimensional Euclidean space
 */
package oor.images.geometry;

/**
 * @author nikhillo
 *
 */
public class Point implements Shape {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date$ : $Rev$";
	
	protected float xc;
	protected float yc;
	
	/**
	 * Default constructor
	 * @param x : The x coordinate of the point
	 * @param y : The y coordinate of the point
	 */
	public Point (float x, float y) {
		xc = x;
		yc = y;
	}
	
	@Override
	/**
	 * Method to check if one point is equal to another
	 */
	public boolean equals(Object o) {
		if (o instanceof Point) {
			Point p = (Point) o;
			return (p.xc == xc && p.yc == yc);
		}
		return o.equals(this);
		
	}
	
	@Override
	public int hashCode() {
		int hc = -1;
		hc = (int) ((31 * hc) + xc);
		hc = (int) ((31 * hc) + yc);
		return hc;
	}
	
	/**
	 * Method to check if the given point "dominates" this point
	 * @param p : The Point to be checked
	 * @return true iff both the x and y coordinate of th given point are greater than or equal
	 * to the corresponding coordinates of this point
	 */
	public boolean greaterThan(Point p) {
		return (this.xc >= p.xc && this.yc >= p.yc);
	}
	
	/**
	 * Method to compute the euclidean distance between two points
	 * @param p
	 * @return
	 */
	public double distance (Point p) {
		return Math.sqrt(Math.pow(this.xc  - p.xc, 2) + Math.pow(this.yc - p.yc, 2));
	}
	
	/**
	 * Method to compute a Point that represents the geometric midpoint between this point
	 * and another given point
	 * @param p : The other point to be considered
	 * @return A Point that represents the midpoint
	 */
	public Point midPoint(Point p) {
		return new Point((xc + p.xc) / 2, (yc + p.yc) / 2);
	}
	
	
	@Override
	public String toString() {
		return "( " + xc + " , " + yc + " )";
	}
}
