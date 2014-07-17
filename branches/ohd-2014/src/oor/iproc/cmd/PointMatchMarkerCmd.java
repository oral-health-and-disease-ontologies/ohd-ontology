/**
 * 
 */
package oor.iproc.cmd;

import java.awt.Color;
import java.util.Collection;

import ij.process.ImageProcessor;

import mpicbg.models.PointMatch;

import org.apache.commons.chain.Context;

/**
 * @author nikhillo
 *
 */
public class PointMatchMarkerCmd extends AbstractCommand {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Rev$ $Date$";
	
	/* (non-Javadoc)
	 * @see oor.iproc.cmd.AbstractCommand#execute(org.apache.commons.chain.Context)
	 */
	@Override
	@SuppressWarnings("unchecked")
	public boolean execute(Context ctxt) throws Exception {
		ImageProcessor srcImg = getSource(ctxt);
		ImageProcessor tgtImg = getTarget(ctxt);
		Color color = (Color) ctxt.get(PMM_COLOR);
		Collection<PointMatch> points = (Collection<PointMatch>) ctxt.get(PMM_MATCHES);
		float vis_scale = 256.0f / srcImg.getWidth();
		float[] m_p1, m_p2;
		
		srcImg.setColor(color);
		tgtImg.setColor(color);
		srcImg.setLineWidth(2);
		tgtImg.setLineWidth(2);
		
		for (PointMatch m : points) {
			m_p1 = m.getP1().getL();
			m_p2 = m.getP2().getL();
			srcImg.drawDot( ( int )Math.round( vis_scale * m_p2[ 0 ] ), ( int )Math.round( vis_scale * m_p2[ 1 ] ) );
			tgtImg.drawDot( ( int )Math.round( vis_scale * m_p1[ 0 ] ), ( int )Math.round( vis_scale * m_p1[ 1 ] ) );
		}
		return false;
	}

}
