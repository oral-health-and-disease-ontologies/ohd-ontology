/**
 * Command to perform mathematical operations on two images
 */
package oor.iproc.cmd;

import ij.process.ImageProcessor;

import org.apache.commons.chain.Context;

/**
 * @author nikhillo
 *
 */
public class ImageCalculatorCmd extends AbstractCommand {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Rev$ $Date$";
	/* (non-Javadoc)
	 * @see oor.iproc.cmd.AbstractCommand#execute(org.apache.commons.chain.Context)
	 */
	@Override
	public boolean execute(Context ctxt) throws Exception {
		ImageProcessor refImg = getSource(ctxt);
		ImageProcessor tgtImg = getTarget(ctxt);
		int operation = getIntValue(ctxt, IC_OPERATION);
		
		float v1, v2;
		int width = tgtImg.getWidth();
		int height = tgtImg.getHeight();
		
		for (int x = 0; x < width; x++) {
			for (int y = 0; y < height; y++) {
				v1 = refImg.getPixelValue(x, y);
				v2 = tgtImg.getPixelValue(x, y);
				switch (operation) {
				case 1:
					v2 += v1;
					break;
				case 2:
					v2 -= v1;
					break;
				case 3:
					v2 *= v1;
					break;
				case 4:
					v2 = (v1 == 0) ? 0 : v2/v1;
					break;
				}
				
				tgtImg.putPixelValue(x, y, v2);
			}
		}
		setTarget(ctxt, tgtImg);
		return false;
	}

}
