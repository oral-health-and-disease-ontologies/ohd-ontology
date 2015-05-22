/**
 * 
 */
package oor.iproc.cmd;

import ij.ImagePlus;
import ij.process.ImageProcessor;

import org.apache.commons.chain.Context;

import bunwarpj.bUnwarpJ_;

/**
 * @author nikhillo
 *
 */
public class BunWarpCmd extends AbstractCommand {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Rev$ $Date$";
	
	/* (non-Javadoc)
	 * @see oor.iproc.cmd.AbstractCommand#execute(org.apache.commons.chain.Context)
	 */
	@Override
	public boolean execute(Context ctxt) throws Exception {
		ImagePlus targetImp = getTargetImagePlus(ctxt);
		ImagePlus sourceImp = getSourceImagePlus(ctxt);
		ImageProcessor targetMskIp = getTarget(ctxt);
		ImageProcessor sourceMskIp = getSource(ctxt);
		int mode = getIntValue(ctxt, BW_MODE);
		int img_subsamp_fact = getIntValue(ctxt, BW_SUBSAMPFACT);
		int min_scale_deformation = getIntValue(ctxt, BW_MINDEFORM);
		int max_scale_deformation = getIntValue(ctxt, BW_MAXDEFORM);
		double divWeight = getDblValue(ctxt, BW_DIVWT);
		double curlWeight = getDblValue(ctxt, BW_CURLWT);
		double landmarkWeight = getDblValue(ctxt, BW_LMARKWT);
		double imageWeight = getDblValue(ctxt, BW_IMGWT);
		double consistencyWeight = getDblValue(ctxt, BW_CNSTCYWT);
		double stopThreshold = getDblValue(ctxt, BW_THRES);
		ImagePlus[] result = bUnwarpJ_.alignImagesBatch(targetImp, sourceImp, targetMskIp, sourceMskIp, mode, img_subsamp_fact,
				min_scale_deformation, max_scale_deformation, divWeight, curlWeight,
				landmarkWeight, imageWeight, consistencyWeight, stopThreshold);
		ImagePlus srcToTgt = result[0];
		ImageProcessor rslt = srcToTgt.getProcessor();
		setTarget(ctxt, rslt);
		return false;
	}

}
