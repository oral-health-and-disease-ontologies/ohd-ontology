/**
 * 
 */
package oor.iproc.cmd;

import ij.plugin.filter.BackgroundSubtracter;
import ij.process.ImageProcessor;

import org.apache.commons.chain.Context;

/**
 * @author nikhillo
 *
 */
public class SubtractBackgroundCmd extends AbstractCommand {
	@SuppressWarnings("unused")
	private static final String __REV__ = "%Rev$ $Date$";
	
	private BackgroundSubtracter bgs;
	public SubtractBackgroundCmd() {
		bgs = new BackgroundSubtracter();
	}
	
	/* (non-Javadoc)
	 * @see org.apache.commons.chain.Command#execute(org.apache.commons.chain.Context)
	 */
	@Override
	public boolean execute(Context ctxt) throws Exception {
		ImageProcessor img = getTarget(ctxt);
		double radius = getDblValue(ctxt, SB_RADIUS);
		boolean createBackground = getBoolValue(ctxt, SB_CREATEBG);
		boolean lightBackground = getBoolValue(ctxt, SB_LIGHTBG);
        boolean useParaboloid = getBoolValue(ctxt, SB_USEPARA);
        boolean doPresmooth = getBoolValue(ctxt, SB_PRESMOOTH);
        boolean correctCorners = getBoolValue(ctxt, SB_CORRCORN);
        bgs.rollingBallBackground(img, radius, createBackground, lightBackground, useParaboloid, doPresmooth, correctCorners);
        setTarget(ctxt, img);
		return false;
	}

}
