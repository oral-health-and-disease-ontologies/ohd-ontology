/**
 * Command to make an image binary
 */
package oor.iproc.cmd;

import ij.process.ImageProcessor;

import org.apache.commons.chain.Context;

/**
 * @author nikhillo
 *
 */
public class MakeBinaryCmd extends AbstractCommand {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Rev$ $Date$";
	
	/* (non-Javadoc)
	 * @see oor.iproc.cmd.AbstractCommand#execute(org.apache.commons.chain.Context)
	 */
	@Override
	public boolean execute(Context ctxt) throws Exception {
		ImageProcessor iproc = getTarget(ctxt);
		iproc.autoThreshold();
		setTarget(ctxt, iproc);
		return false;
	}
}
