/**
 * 
 */
package oor.iproc.cmd;

import java.util.ArrayList;

import mpicbg.ij.SIFT;
import mpicbg.imagefeatures.Feature;
import mpicbg.imagefeatures.FloatArray2DSIFT;
import mpicbg.imagefeatures.FloatArray2DSIFT.Param;

import org.apache.commons.chain.Context;

/**
 * @author nikhillo
 *
 */
public class SIFTExtractFeaturesCmd extends AbstractCommand {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Rev$ $Date$";
	
	/* (non-Javadoc)
	 * @see oor.iproc.cmd.AbstractCommand#execute(org.apache.commons.chain.Context)
	 */
	@Override
	public boolean execute(Context ctxt) throws Exception {
		Param p = new Param();
		p.fdBins = getIntValue(ctxt, SIF_FDBINS);
		p.fdSize = getIntValue(ctxt, SIF_FDSZ);
		p.initialSigma = getFLoatValue(ctxt, SIF_INITSIG);
		p.maxOctaveSize = getIntValue(ctxt, SIF_MAXOCT);
		p.minOctaveSize = getIntValue(ctxt, SIF_MINOCT);
		p.steps = getIntValue(ctxt, SIF_STEPS);
		SIFT mops = new SIFT(new FloatArray2DSIFT(p));
		ArrayList<Feature> features = new ArrayList<Feature>();
		mops.extractFeatures(getTarget(ctxt), features);
		setTargetFeatures(ctxt, features);
		return false;
	}

}
